.PHONY: test

.tmp.config: rebar.config development.config
	@cat rebar.config > .tmp.config
	@cat development.config >> .tmp.config

test-deps: .tmp.config
	@rebar -C .tmp.config prepare-deps

test: test-deps .tmp.config
	@rebar -C .tmp.config eunit

test-quick: .tmp.config
	@rebar -C .tmp.config eunit
