open Ache

type kind =
  | Request
  | Response of Method.t

type next =
  | Read
  | Start
  | Close
  | Failed of string * string

type snapshot =
  { next : next
  ; left : int
  ; heads : int
  }

type message =
  { head : string
  ; body : string
  ; finish : string
  }

type outcome =
  { mid : snapshot
  ; pre : snapshot
  ; post : snapshot
  ; messages : message list
  }

type grammar =
  { request : (string, string) result
  ; response : (string, string) result
  ; chunk_size : (string, string) result
  ; trailers : (string, string) result
  }

type case =
  { kind : kind
  ; limit : int
  ; input : string
  ; grammar : grammar
  ; outcomes : outcome array
  }

let summary value =
  if String.length value <= 120
  then value
  else "md5:" ^ Digest.to_hex (Digest.string value)

let headers_to_string headers =
  String.concat
    ";"
    (List.map
       (fun (name, value) -> Printf.sprintf "%S:%S" name value)
       (Headers.to_list headers))

let request_to_string { Request.meth; target; version; headers } =
  summary
    (Printf.sprintf
       "%S %S %s [%s]"
       (Method.to_string meth)
       target
       (Version.to_string version)
       (headers_to_string headers))

let response_to_string { Response.version; status; reason; headers } =
  summary
    (Printf.sprintf
       "%s %d %S [%s]"
       (Version.to_string version)
       (Status.to_code status)
       reason
       (headers_to_string headers))

module Run (P : module type of Ache_private.Parse) = struct
  type body =
    { reader : Body.Reader.t
    ; head : string
    ; buffer : Buffer.t
    ; mutable finish : string
    }

  let rec drain b =
    if b.finish = "open"
    then (
      let progress = ref false in
      Body.Reader.schedule_read
        b.reader
        ~on_read:(fun bs ~off ~len ->
          progress := true;
          Buffer.add_string b.buffer (Bigstringaf.substring bs ~off ~len))
        ~on_eof:(fun () -> b.finish <- "eof")
        ~on_error:(fun () -> b.finish <- "error");
      if !progress then drain b)

  let grammar input =
    let map f = Result.map f in
    { request = map request_to_string (P.Grammar.request input)
    ; response = map response_to_string (P.Grammar.response input)
    ; chunk_size = map Int64.to_string (P.Grammar.chunk_size input)
    ; trailers =
        map (fun h -> summary (headers_to_string h)) (P.Grammar.trailers input)
    }

  let request_error = function
    | `Invalid_request (request, error) ->
      ( "invalid_request"
      , request_to_string request
        ^ " "
        ^
          (match error with
          | `Bad_request -> "bad_request"
          | `Not_implemented -> "not_implemented"
          | `Http_version_not_supported -> "http_version_not_supported") )
    | `Parse message -> "parse", message
    | `Method_too_long -> "method_too_long", ""
    | `Target_too_long -> "target_too_long", ""
    | `Header_too_large -> "header_too_large", ""

  let response_error = function
    | `Invalid_response_body_length response ->
      "invalid_response_body_length", response_to_string response
    | `Parse message -> "parse", message
    | `Header_too_large -> "header_too_large", ""
    | `Unsolicited_response -> "unsolicited_response", ""

  let next_of = function
    | `Read -> Read
    | `Start -> Start
    | `Close -> Close
    | `Error (variant, detail) -> Failed (variant, detail)

  let run kind ~limit chunks =
    let bodies = ref [] in
    let collect head reader =
      bodies :=
        { reader; head; buffer = Buffer.create 16; finish = "open" } :: !bodies
    in
    let read, next =
      match kind with
      | Request ->
        let r =
          P.Reader.request ~max_header_size:limit (fun request body ->
            collect (request_to_string request) body)
        in
        ( P.Reader.read_with_more r
        , fun () ->
            next_of
              (match P.Reader.next r with
              | `Error e -> `Error (request_error e)
              | (`Read | `Start | `Close) as n -> n) )
      | Response meth ->
        let queue = Queue.create () in
        for _ = 1 to 3 do
          Queue.push
            (Ache_private.Respd.awaiting
               (Request.create meth "/")
               (fun response body -> collect (response_to_string response) body))
            queue
        done;
        let r = P.Reader.response ~max_header_size:limit queue in
        ( P.Reader.read_with_more r
        , fun () ->
            next_of
              (match P.Reader.next r with
              | `Error e -> `Error (response_error e)
              | (`Read | `Start | `Close) as n -> n) )
    in
    let pending = ref "" in
    let offer more =
      let len = String.length !pending in
      let bs = Bigstringaf.of_string !pending ~off:0 ~len in
      let n = read bs ~off:0 ~len more in
      if n < 0 || n > len then failwith "read consumed outside its bytes";
      pending := String.sub !pending n (len - n);
      n
    in
    let rec feed () =
      if !pending <> "" && offer P.Incomplete > 0 then feed ()
    in
    let snapshot () =
      { next = next ()
      ; left = String.length !pending
      ; heads = List.length !bodies
      }
    in
    let snapshots =
      List.map
        (fun chunk ->
           pending := !pending ^ chunk;
           feed ();
           snapshot ())
        chunks
    in
    let pre = snapshot () in
    ignore (offer P.Complete : int);
    let post = snapshot () in
    let messages =
      List.rev_map
        (fun b ->
           drain b;
           { head = b.head
           ; body = summary (Buffer.contents b.buffer)
           ; finish = b.finish
           })
        !bodies
    in
    { mid = List.hd snapshots; pre; post; messages }

  let case kind ~limit input =
    let len = String.length input in
    let at k =
      if k = 0
      then run kind ~limit [ input ]
      else
        run kind ~limit [ String.sub input 0 k; String.sub input k (len - k) ]
    in
    { kind
    ; limit
    ; input
    ; grammar = grammar input
    ; outcomes = Array.init (max 1 len) at
    }
end

let kind_of_string = function
  | "request" -> Request
  | s when String.starts_with ~prefix:"response:" s ->
    Response (Method.of_string (String.sub s 9 (String.length s - 9)))
  | s -> failwith ("unknown kind " ^ s)

let load path =
  let ib = Scanf.Scanning.open_in_bin path in
  let token () = Scanf.bscanf ib " %s" Fun.id in
  let int () = Scanf.bscanf ib " %d" Fun.id in
  let string () = Scanf.bscanf ib " %S" Fun.id in
  let result () =
    match token () with
    | "ok" -> Ok (string ())
    | "error" -> Error (string ())
    | t -> failwith ("bad result " ^ t)
  in
  let snapshot ?previous () =
    let next =
      match token (), previous with
      | "=", Some previous -> previous
      | "read", _ -> Read
      | "start", _ -> Start
      | "close", _ -> Close
      | "error", _ ->
        let variant = token () in
        Failed (variant, string ())
      | t, _ -> failwith ("bad next " ^ t)
    in
    let left = int () in
    let heads = int () in
    { next; left; heads }
  in
  let rec splits acc =
    match token () with
    | "split" ->
      let first = int () in
      let last = int () in
      let slopes = Array.init 3 (fun _ -> int ()) in
      let mid = snapshot () in
      let pre = snapshot ~previous:mid.next () in
      let post = snapshot ~previous:pre.next () in
      let messages =
        match token () with
        | "=" -> (List.hd acc).messages
        | count ->
          List.init (int_of_string count) (fun _ ->
            let head = string () in
            let body = string () in
            let finish = token () in
            { head; body; finish })
      in
      let o = { mid; pre; post; messages } in
      let at k =
        let d i = slopes.(i) * (k - first) in
        { o with
          mid = { mid with left = mid.left + d 0 }
        ; pre = { pre with left = pre.left + d 1 }
        ; post = { post with left = post.left + d 2 }
        }
      in
      splits
        (List.rev_append
           (List.init (last - first + 1) (fun i -> at (first + i)))
           acc)
    | t -> Array.of_list (List.rev acc), t
  in
  let rec cases acc = function
    | "case" ->
      let kind = kind_of_string (token ()) in
      let limit = int () in
      let input = string () in
      if token () <> "grammar" then failwith "expected grammar";
      let request = result () in
      let response = result () in
      let chunk_size = result () in
      let trailers = result () in
      let outcomes, t = splits [] in
      cases
        ({ kind
         ; limit
         ; input
         ; grammar = { request; response; chunk_size; trailers }
         ; outcomes
         }
        :: acc)
        t
    | "" -> List.rev acc
    | t -> failwith ("bad token " ^ t)
  in
  let r = cases [] (token ()) in
  Scanf.Scanning.close_in ib;
  r
