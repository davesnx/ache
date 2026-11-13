# Vendored Faraday

- Upstream: https://github.com/inhabitedtype/faraday
- Tag: 0.8.2
- Commit: f5f63b542579f5496577c4f86343fee2e17e3bf1
- Copied: 2026-11-13
- Files: `lib/faraday.ml`, `lib/faraday.mli`, `LICENSE`. The files are
  unmodified.
- License: BSD-3-Clause, see `LICENSE` in this directory.

`lib/dune` copies `faraday.ml` and `faraday.mli` into the `ache` library with
`copy_files#`. The module compiles as `Ache__Faraday`, so it does not clash
with the opam `faraday` library. This directory has no `dune` file.

## Update

1. In a clone of the upstream repository, check out the new tag.
2. Copy `lib/faraday.ml`, `lib/faraday.mli` and `LICENSE` into this directory
   without changes, for example `git show <tag>:lib/faraday.ml > faraday.ml`.
3. Update the tag, commit and date in this file.
4. Run `dune build @all`, `dune test` and `dune fmt`. `dune fmt` must not
   change the files in this directory.
