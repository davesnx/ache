# Vendored Faraday

- Upstream: https://github.com/inhabitedtype/faraday
- Tag: 0.8.2
- Commit: f5f63b542579f5496577c4f86343fee2e17e3bf1
- Copied: 2026-11-13
- Files: `faraday.ml` and `faraday.mli` are derived from upstream
  `lib/faraday.ml` and `lib/faraday.mli`. They are changed for ache, see below.
- License: BSD-3-Clause, Copyright (c) 2016 Inhabited Type LLC. The root
  `LICENSE` covers this code.

`lib/dune` copies `faraday.ml` and `faraday.mli` into the `ache` library with
`copy_files#`. The module compiles as `Ache__Faraday`, so it does not clash
with the opam `faraday` library. This directory has no `dune` file.

## Changes from 0.8.2

Removed, because nothing in ache, its runtimes, tests or benchmarks uses
them:

- `write_bytes`, `write_uint8` and `write_gen` (`write_gen` stays as an
  internal function).
- The `BE` module.
- `LE.write_uint32`, `LE.write_uint48`, `LE.write_float` and
  `LE.write_double`.
- `flush`. Use `flush_with_reason`.
- `serialize` and `serialize_to_bigstring`.
- `free_bytes_in_buffer` from the interface (it stays as an internal
  function).

The doc comments that referred to removed values are changed. Both files are
formatted with ache's `.ocamlformat`. The behaviour of the remaining values
is the same as in 0.8.2.

## Port an upstream fix

The files are not a copy of upstream, so do not replace them.

1. Read the upstream commit or release notes, and find the change in
   `lib/faraday.ml` or `lib/faraday.mli`.
2. If the change touches only removed values, skip it.
3. Otherwise, apply the change by hand to `faraday.ml` and `faraday.mli`
   here. Keep only the values that ache uses.
4. Add the upstream commit to "Changes from 0.8.2" in this file.
5. Run `make build`, `make format`, `make test` and `make docs`.
