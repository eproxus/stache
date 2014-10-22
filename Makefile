.PHONY: test doc

.tmp.config: rebar.config development.config
	@cat rebar.config > .tmp.config
	@cat development.config >> .tmp.config

dev-deps: .tmp.config
	@rebar -C .tmp.config prepare-deps

test: dev-deps .tmp.config
	@rebar -C .tmp.config skip_deps=true eunit

test-quick: .tmp.config
	@rebar -C .tmp.config skip_deps=true eunit

doc: dev-deps .tmp.config
	@rebar -C .tmp.config skip_deps=true doc
