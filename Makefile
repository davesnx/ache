project_name = ache

DUNE = opam exec -- dune

.PHONY: help
help: ## Print this help message
	@echo "";
	@echo "List of available make commands";
	@echo "";
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}';
	@echo "";

.PHONY: build
build: ## Build the project, including non installable libraries and executables
	$(DUNE) build --root . --profile=dev @all

.PHONY: build-prod
build-prod: ## Build for production (--profile=prod)
	$(DUNE) build --root . --profile=prod

.PHONY: dev
dev: ## Build in watch mode
	$(DUNE) build --root . -w --profile=dev @all

.PHONY: clean
clean: ## Clean artifacts
	$(DUNE) clean --root .

.PHONY: test
test: ## Run the unit tests
	$(DUNE) build --root . @runtest

.PHONY: test-watch
test-watch: ## Run the unit tests in watch mode
	$(DUNE) build --root . @runtest -w

.PHONY: test-promote
test-promote: ## Updates snapshots and promotes it to correct
	$(DUNE) build --root . @runtest --auto-promote

.PHONY: format
format: ## Format the codebase with ocamlformat
	@DUNE_CONFIG__GLOBAL_LOCK=disabled $(DUNE) build --root . @fmt --auto-promote

.PHONY: format-check
format-check: ## Checks if format is correct
	@DUNE_CONFIG__GLOBAL_LOCK=disabled $(DUNE) build --root . @fmt

NODE = opam exec -- node
BENCH_DEPS = httpaf.0.7.1 httpaf-lwt-unix.0.7.1 httpun.0.2.0 httpun-lwt-unix.0.2.0 httpun-eio.0.2.0 eio_main

.PHONY: bench-deps
bench-deps: ## Install httpaf and httpun for the comparison benchmark
	opam install -y $(BENCH_DEPS)

.PHONY: bench-build
bench-build: ## Build the comparison benchmark in the release profile
	$(DUNE) build --root . --profile=release @benchmarks/vs/all

CHECK_FLAGS = --reps 11 --time 0.12

.PHONY: bench-check
bench-check: bench-build ## Compare ache with httpun and httpaf in process against benchmarks/vs/expectations.txt (about 1 min)
	$(if $(CHECK_CPU),taskset -c $(CHECK_CPU) )_build/default/benchmarks/vs/core_bench.exe --check benchmarks/vs/expectations.txt $(CHECK_FLAGS)

.PHONY: bench-quick
bench-quick: bench-build ## Run the comparison benchmark, quick profile (about 15 min)
	$(NODE) benchmarks/vs/run.ts --profile quick $(BENCH_FLAGS)

.PHONY: bench
bench: bench-build ## Run the comparison benchmark, full profile (about 3 h)
	$(NODE) benchmarks/vs/run.ts --profile full $(BENCH_FLAGS)

.PHONY: docs
docs: ## Generate odoc documentation
	$(DUNE) build --root . @doc

.PHONY: create-switch
create-switch: ## Create opam switch
	opam switch create . 5.4.0 --deps-only --with-test -y

.PHONY: install
install: ## Install dependencies
	opam install . --deps-only --with-test --with-doc --with-dev-setup -y

.PHONY: init
init: create-switch install ## Create a local dev enviroment

.PHONY: subst
subst: ## Run dune substitute
	$(DUNE) subst
