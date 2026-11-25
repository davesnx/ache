let days = [| "Thu"; "Fri"; "Sat"; "Sun"; "Mon"; "Tue"; "Wed" |]

let months =
  [| "Jan"
   ; "Feb"
   ; "Mar"
   ; "Apr"
   ; "May"
   ; "Jun"
   ; "Jul"
   ; "Aug"
   ; "Sep"
   ; "Oct"
   ; "Nov"
   ; "Dec"
  |]

(* Howard Hinnant's civil_from_days. *)
let civil_of_days z =
  let z = z + 719468 in
  let era = (if z >= 0 then z else z - 146096) / 146097 in
  let doe = z - (era * 146097) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let d = doy - (((153 * mp) + 2) / 5) + 1 in
  let m = if mp < 10 then mp + 3 else mp - 9 in
  let y = yoe + (era * 400) + if m <= 2 then 1 else 0 in
  y, m, d

let floor_div a b = if a >= 0 then a / b else ((a + 1) / b) - 1

let imf_fixdate seconds =
  let day = floor_div seconds 86400 in
  let second_of_day = seconds - (day * 86400) in
  let year, month, mday = civil_of_days day in
  let weekday = ((day mod 7) + 7) mod 7 in
  Printf.sprintf
    "%s, %02d %s %04d %02d:%02d:%02d GMT"
    days.(weekday)
    mday
    months.(month - 1)
    year
    (second_of_day / 3600)
    (second_of_day / 60 mod 60)
    (second_of_day mod 60)

type cache =
  { mutable second : int
  ; mutable text : string
  }

let cache = Domain.DLS.new_key (fun () -> { second = min_int; text = "" })

let cached seconds =
  let c = Domain.DLS.get cache in
  if c.second <> seconds
  then (
    c.second <- seconds;
    c.text <- imf_fixdate seconds);
  c.text

external time_seconds :
   unit
  -> (int[@untagged])
  = "ache_time_seconds_byte" "ache_time_seconds"
[@@noalloc]

let now () = cached (time_seconds ())
