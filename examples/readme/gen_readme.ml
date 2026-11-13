let marker_prefix = "<!-- example: "
let marker_suffix = " -->"

let read_file path =
  In_channel.with_open_bin path In_channel.input_all
  |> String.split_on_char '\n'

let example_path line =
  let line = String.trim line in
  let prefix = String.length marker_prefix in
  let suffix = String.length marker_suffix in
  let length = String.length line in
  if
    length > prefix + suffix
    && String.starts_with ~prefix:marker_prefix line
    && String.ends_with ~suffix:marker_suffix line
  then Some (String.sub line prefix (length - prefix - suffix))
  else None

let rec skip_block = function
  | [] -> failwith "README: code block after an example marker is not closed"
  | "```" :: rest -> rest
  | _ :: rest -> skip_block rest

let rec expand ~root = function
  | [] -> []
  | line :: rest ->
    (match example_path line with
    | None -> line :: expand ~root rest
    | Some path ->
      let rest =
        match rest with "```ocaml" :: rest -> skip_block rest | _ -> rest
      in
      let source = read_file (Filename.concat root path) in
      let source =
        match List.rev source with "" :: lines -> List.rev lines | _ -> source
      in
      (line :: "```ocaml" :: source) @ ("```" :: expand ~root rest))

let () =
  match Sys.argv with
  | [| _; root; readme |] ->
    print_string (String.concat "\n" (expand ~root (read_file readme)))
  | _ -> failwith "usage: gen_readme ROOT README"
