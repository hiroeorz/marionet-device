.PHONY: test

ERL=erl
BEAMDIR=./deps/*/ebin ./ebin
REBAR=./rebar
DIALYZER=dialyzer
EDOWN=./make_doc

all: update-deps get-deps clean compile xref edo

dev: compile xref edoc

update-deps:
	@$(REBAR) update-deps

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

xref:
	@$(REBAR) xref skip_deps=true

clean:
	@$(REBAR) clean; rm -rf build/html; 

test:
	@$(REBAR) skip_deps=true eunit

edoc:
	@$(EDOWN)

dialyzer: compile
	@$(DIALYZER) -I include ebin

setup-dialyzer:
	@$(DIALYZER) --build_plt \
                     --apps kernel stdlib mnesia eunit erts crypto
