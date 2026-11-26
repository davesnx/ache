(* A TLS server for the tests of the idle-connection check of the TLS clients
   (undici GHSA-35p6-xmwp-9g52). It runs in another domain on blocking sockets,
   so that the client domain can block while the stray records arrive. *)

type stray =
  | Data of string  (** Application data: bytes that look like a response. *)
  | Key_update  (** A TLS 1.3 KeyUpdate that asks for the client's: no data. *)
  | Close_notify  (** A close_notify alert, with the TCP connection open. *)

let write socket s =
  try ignore (Unix.write_substring socket s 0 (String.length s) : int) with
  | Unix.Unix_error _ -> ()

let has_head s =
  let rec from i =
    i + 4 <= String.length s && (String.sub s i 4 = "\r\n\r\n" || from (i + 1))
  in
  from 0

(* Reads TLS records until the plaintext holds a request head, or the input
   ends. *)
let read_head socket state =
  let buf = Bytes.create 0x4000 in
  let rec loop state acc =
    if has_head acc
    then state, acc
    else
      match Unix.read socket buf 0 (Bytes.length buf) with
      | 0 | (exception Unix.Unix_error _) -> state, acc
      | n ->
        (match Tls.Engine.handle_tls state (Bytes.sub_string buf 0 n) with
        | Ok (state, eof, `Response response, `Data data) ->
          Option.iter (write socket) response;
          let acc = acc ^ Option.value data ~default:"" in
          if Option.is_some eof then state, acc else loop state acc
        | Error (_, `Response response) ->
          write socket response;
          state, acc)
  in
  loop state ""

let send socket state s =
  match Tls.Engine.send_application_data state [ s ] with
  | Some (state, data) ->
    write socket data;
    state
  | None -> failwith "Ache_test_tls_stray: the session is not ready"

let send_stray socket state = function
  | Data s -> send socket state s
  | Key_update ->
    (match Tls.Engine.key_update ~request:true state with
    | Ok (state, data) ->
      write socket data;
      state
    | Error _ -> failwith "Ache_test_tls_stray: key update needs TLS 1.3")
  | Close_notify ->
    let state, data = Tls.Engine.send_close_notify state in
    write socket data;
    state

(* For each of [strays] in turn: accepts a connection, answers the first request
   with [200 ok], sends the stray 50 ms later, and answers a second request with
   [200 real] if one comes within 1 s. The domain returns the heads of the
   second requests ([""] for none). *)
let start config strays =
  let listening = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt listening Unix.SO_REUSEADDR true;
  Unix.bind listening (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen listening 1;
  let port =
    match Unix.getsockname listening with
    | Unix.ADDR_INET (_, port) -> port
    | Unix.ADDR_UNIX _ -> assert false
  in
  let serve stray =
    let socket, _ = Unix.accept listening in
    Fun.protect
      ~finally:(fun () -> Unix.close socket)
      (fun () ->
         let state, _ = read_head socket (Tls.Engine.server config) in
         let state =
           send socket state "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok"
         in
         Unix.sleepf 0.05;
         let state = send_stray socket state stray in
         Unix.setsockopt_float socket Unix.SO_RCVTIMEO 1.0;
         let state, second = read_head socket state in
         (* After its close_notify the server sends nothing more. *)
         if has_head second && stray <> Close_notify
         then
           ignore
             (send
                socket
                state
                "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\nreal"
              : Tls.Engine.state);
         second)
  in
  let serve stray =
    try serve stray with exn -> "exception " ^ Printexc.to_string exn
  in
  ( port
  , Domain.spawn (fun () ->
      Fun.protect
        ~finally:(fun () -> Unix.close listening)
        (fun () -> List.map serve strays)) )

(* The name of each case, its stray, and the outcome of the second request that
   the client must see. *)
let cases =
  [ ( "a response"
    , Data "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\nevil"
    , "Connection_closed" )
  ; "part of a head", Data "HTTP/1.1 200 OK\r\n", "Connection_closed"
  ; "close_notify", Close_notify, "Connection_closed"
  ; "a key update", Key_update, "200 real"
  ; "an empty line", Data "\r\n", "200 real"
  ]

let strays = List.map (fun (_, stray, _) -> stray) cases

(* Whether the server must see a second request in each case. *)
let second_requests_sent =
  List.map (fun (_, _, outcome) -> outcome <> "Connection_closed") cases
