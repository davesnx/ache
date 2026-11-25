exception Truncated

type flow = [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t

module Tcp = struct
  type t =
    { flow : flow
    ; eof : bool ref
    }

  let read_methods = []

  let single_read t buf =
    match Eio.Flow.single_read t.flow buf with
    | n -> n
    | exception End_of_file ->
      t.eof := true;
      raise End_of_file

  let single_write t bufs = Eio.Flow.single_write t.flow bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown t command = Eio.Flow.shutdown t.flow command
  let close t = Eio.Resource.close t.flow
end

(* tls-eio makes one record for each buffer it is given. [single_write] joins
   the leading buffers into one record of up to 16 KiB, the largest record,
   through [scratch], one buffer for each connection. A first buffer of 16 KiB
   or more goes as is. [Eio.Flow.write] calls it again for the rest. *)
let record_size = 0x4000
let scratch () = Cstruct.create record_size

let single_write tls scratch = function
  | first :: _ when Cstruct.length first >= record_size ->
    Eio.Flow.single_write tls [ first ]
  | bufs ->
    let rec fill pos = function
      | [] -> pos
      | buf :: rest ->
        let n = min (Cstruct.length buf) (record_size - pos) in
        Cstruct.blit buf 0 scratch pos n;
        if pos + n < record_size then fill (pos + n) rest else pos + n
    in
    Eio.Flow.single_write tls [ Cstruct.sub scratch 0 (fill 0 bufs) ]

type guard =
  { tcp_eof : bool ref
  ; mutable close_delimited : bool
  }

module Guarded = struct
  type t =
    { tls : Tls_eio.t
    ; guard : guard
    ; scratch : Cstruct.t
    }

  let read_methods = []

  let single_read t buf =
    match Eio.Flow.single_read t.tls buf with
    | n -> n
    | exception End_of_file when t.guard.close_delimited && !(t.guard.tcp_eof)
      ->
      raise Truncated
    | exception Eio.Io (Eio.Net.E (Connection_reset _), _)
      when t.guard.close_delimited ->
      raise Truncated

  let single_write t bufs = single_write t.tls t.scratch bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown t command = Eio.Flow.shutdown t.tls command
  let close t = Eio.Resource.close t.tls
end

module Server_flow = struct
  type t =
    { tls : Tls_eio.t
    ; tcp : flow
    ; scratch : Cstruct.t
    }

  let read_methods = []
  let single_read t buf = Eio.Flow.single_read t.tls buf
  let single_write t bufs = single_write t.tls t.scratch bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

  let shutdown t = function
    | `Receive -> Eio.Flow.shutdown t.tcp `Receive
    | (`Send | `All) as command -> Eio.Flow.shutdown t.tls command

  let close t = Eio.Resource.close t.tls
end

let two_way
      (type t)
      (module Tls_flow : Eio.Flow.Pi.TWO_WAY with type t = t)
      close
  =
  Eio.Resource.handler
    (Eio.Resource.bindings (Eio.Flow.Pi.two_way (module Tls_flow))
    @ [ H (Eio.Resource.Close, close) ])

let tcp_handler = two_way (module Tcp) Tcp.close
let guarded_handler = two_way (module Guarded) Guarded.close
let server_handler = two_way (module Server_flow) Server_flow.close

let close_delimited (request : Ache.Request.t) (response : Ache.Response.t) =
  let request_method =
    match request.meth with
    | #Ache.Method.standard as meth -> meth
    | `Other _ -> `GET
  in
  match response.status, request_method with
  | `Switching_protocols, _ -> false
  | status, `CONNECT when Ache.Status.is_successful status -> false
  | _ -> Ache.Response.body_length ~request_method response = `Close_delimited

type peer =
  [ `Host of [ `host ] Domain_name.t
  | `Ip of Ipaddr.t
  | `Insecure_no_name_check
  ]

module Client = struct
  type t =
    { client : Ache_eio.Client.t
    ; guard : guard
    }

  let create_connection ?config ~sw ~tls ~(peer : peer) flow =
    let guard = { tcp_eof = ref false; close_delimited = false } in
    let tcp : flow =
      Eio.Resource.T ({ Tcp.flow :> flow; eof = guard.tcp_eof }, tcp_handler)
    in
    let tls =
      match peer with
      | `Host host -> Tls_eio.client_of_flow tls ~host tcp
      | `Ip ip -> Tls_eio.client_of_flow tls ~ip tcp
      | `Insecure_no_name_check -> Tls_eio.client_of_flow tls tcp
    in
    let guarded : flow =
      Eio.Resource.T
        ({ Guarded.tls; guard; scratch = scratch () }, guarded_handler)
    in
    { client = Ache_eio.Client.create_connection ?config ~sw guarded; guard }

  let request
        t
        ?flush_headers_immediately
        request
        ~error_handler
        ~response_handler
    =
    Ache_eio.Client.request
      t.client
      ?flush_headers_immediately
      request
      ~error_handler
      ~response_handler:(fun response body ->
        if close_delimited request response then t.guard.close_delimited <- true;
        response_handler response body)

  let shutdown t = Ache_eio.Client.shutdown t.client
  let is_closed t = Ache_eio.Client.is_closed t.client
  let upgrade t impl = Ache_eio.Client.upgrade t.client impl
end

module Server = struct
  let create_connection_handler
        ?config
        ?clock
        ?idle_timeout
        ?head_timeout
        ~tls
        ~request_handler
        ~error_handler
    =
    let handler =
      Ache_eio.Server.create_connection_handler
        ?config
        ?clock
        ?idle_timeout
        ?head_timeout
        ~request_handler
        ~error_handler
    in
    fun tcp client_addr ->
      let handshake () = Tls_eio.server_of_flow tls tcp in
      let flow =
        let limit =
          match idle_timeout, head_timeout with
          | None, None -> None
          | Some seconds, None | None, Some seconds -> Some seconds
          | Some idle, Some head -> Some (Float.min idle head)
        in
        match clock, limit with
        | Some clock, Some seconds ->
          Eio.Time.with_timeout_exn clock seconds handshake
        | _ -> handshake ()
      in
      handler
        (Eio.Resource.T
           ( { Server_flow.tls = flow; tcp :> flow; scratch = scratch () }
           , server_handler )
         : flow)
        client_addr
end
