open Ache

let get_head =
  "GET /wp-content/uploads/2010/03/hello-kitty-darth-vader-pink.jpg HTTP/1.1\r\n\
   Host: www.kittyhell.com\r\n\
   User-Agent: Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; ja-JP-mac; \
   rv:1.9.2.3) Gecko/20100401 Firefox/3.6.3 Pathtraq/0.9\r\n\
   Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n\
   Accept-Language: ja,en-us;q=0.7,en;q=0.3\r\n\
   Accept-Encoding: gzip,deflate\r\n\
   Accept-Charset: Shift_JIS,utf-8;q=0.7,*;q=0.7\r\n\
   Keep-Alive: 115\r\n\
   Connection: keep-alive\r\n\
   \r\n"

let chunked_post =
  let chunk = Printf.sprintf "400\r\n%s\r\n" (String.make 1024 'x') in
  "POST /upload HTTP/1.1\r\n\
   Host: example.com\r\n\
   Transfer-Encoding: chunked\r\n\
   \r\n"
  ^ String.concat "" (List.init 16 (fun _ -> chunk))
  ^ "0\r\n\r\n"

let response = Response.create `OK

let request_handler reqd =
  let body = Reqd.request_body reqd in
  let rec on_read _ ~off:_ ~len:_ =
    Body.Reader.schedule_read body ~on_read ~on_eof
  and on_eof () = Reqd.respond_with_string reqd response "" in
  Body.Reader.schedule_read body ~on_read ~on_eof

let rec drain conn =
  match Server_connection.next_write_operation conn with
  | `Write iovecs ->
    Server_connection.report_write_result conn (`Ok (IOVec.lengthv iovecs));
    drain conn
  | `Yield | `Close _ -> ()

let feed conn bs reads =
  let start =
    List.fold_left
      (fun start (off, len) ->
         match Server_connection.next_read_operation conn with
         | `Read ->
           let len = off + len - start in
           start + Server_connection.read conn bs ~off:start ~len
         | `Yield | `Close -> failwith "connection is not reading")
      0
      reads
  in
  if start <> Bigstringaf.length bs then failwith "input not consumed";
  drain conn

let reads_of_size size total =
  let rec go off acc =
    if off >= total
    then List.rev acc
    else
      let len = min size (total - off) in
      go (off + len) ((off, len) :: acc)
  in
  go 0 []

let bench name input reads iterations =
  let bs = Bigstringaf.of_string input ~off:0 ~len:(String.length input) in
  let conn = Server_connection.create request_handler in
  for _ = 1 to 1000 do
    feed conn bs reads
  done;
  Gc.full_major ();
  let words = Gc.minor_words () in
  let t0 = Sys.time () in
  for _ = 1 to iterations do
    feed conn bs reads
  done;
  let t1 = Sys.time () in
  let words = Gc.minor_words () -. words in
  let n = float_of_int iterations in
  Printf.printf
    "%-40s %8.0f ns/request %8.0f minor words/request\n%!"
    name
    ((t1 -. t0) *. 1e9 /. n)
    (words /. n)

let () =
  let head = String.length get_head in
  bench
    (Printf.sprintf "GET head (%d bytes), 1 read" head)
    get_head
    [ 0, head ]
    1_000_000;
  bench
    (Printf.sprintf "GET head (%d bytes), 2 reads" head)
    get_head
    [ 0, 200; 200, head - 200 ]
    1_000_000;
  bench
    (Printf.sprintf "POST, 16 KiB chunked body, 4096-byte reads")
    chunked_post
    (reads_of_size 4096 (String.length chunked_post))
    100_000
