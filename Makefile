.PHONY: check test bench bench-compare bench-guard bench-call-perl bench-call-ocaml

BENCH_CALLS ?= 10000
BENCH_DEPTH ?= 300
BENCH_ITERS ?= 3
BENCH_WARMUP ?= 1
BENCH_MAX_REC_PCT ?= 10
BENCH_MAX_MOD_PCT ?= 10
BENCH_MAX_STRICT_PCT ?= 10

check: test

test:
	./tests/run-tests.sh

bench:
	@tmp=$$(mktemp); \
	tmpjson=$$(mktemp); \
	perl bench/overhead-suite.pl --calls $(BENCH_CALLS) --depth $(BENCH_DEPTH) --iters $(BENCH_ITERS) --warmup $(BENCH_WARMUP) --json-out "$$tmpjson" > "$$tmp"; \
	ts=$$(date -u +"%Y-%m-%dT%H:%M:%SZ"); \
	ts_file=$$(date -u +"%Y%m%dT%H%M%SZ"); \
	os=$$(uname -srm); \
	perlver=$$(perl -e 'print $$^V'); \
	mkdir -p bench/history; \
	cp "$$tmpjson" "bench/history/$$ts_file.json"; \
	{ \
		echo ""; \
		echo "## Baseline Run ($$ts)"; \
		echo ""; \
		echo "- Date (UTC): \`$$ts\`"; \
		echo "- OS: \`$$os\`"; \
		echo "- Perl: \`$$perlver\`"; \
		echo "- JSON snapshot: \`bench/history/$$ts_file.json\`"; \
		echo "- Command:"; \
		echo ""; \
		echo "\`\`\`sh"; \
		echo "perl bench/overhead-suite.pl --calls $(BENCH_CALLS) --depth $(BENCH_DEPTH) --iters $(BENCH_ITERS) --warmup $(BENCH_WARMUP)"; \
		echo "\`\`\`"; \
		echo ""; \
		echo "\`\`\`text"; \
		cat "$$tmp"; \
		echo "\`\`\`"; \
	} >> bench/baseline.md; \
	cat "$$tmp"; \
	perl bench/compare-baseline.pl --dir bench/history; \
	rm -f "$$tmp" "$$tmpjson"

bench-compare:
	perl bench/compare-baseline.pl --dir bench/history

bench-guard:
	perl bench/guard-baseline.pl --dir bench/history --max-rec $(BENCH_MAX_REC_PCT) --max-mod $(BENCH_MAX_MOD_PCT) --max-strict $(BENCH_MAX_STRICT_PCT)

bench-call-perl:
	perl bench/call-overhead.pl --impl perl

bench-call-ocaml:
	opam exec -- dune build ./simp.exe
	opam exec -- perl bench/call-overhead.pl --impl ocaml
