#!/usr/bin/env sh
set -eu
prove -v tests/simp.t tests/conformance.t tests/static-check.t tests/strict-globals.t tests/simp-ocaml.t
