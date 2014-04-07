.PHONY: test

ERL=erl
BEAMDIR=./deps/*/ebin ./ebin
REBAR=./rebar
REBAR_GEN=../../rebar
DIALYZER=dialyzer
EDOWN=./make_doc

all: update-deps get-deps compile xref edoc

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
	@$(REBAR) clean
	rm -rf  ./rel/raspberrypi/raspberrypi
	rm -rf  ./rel/galileo/galileo
	rm -rf  ./rel/plc/plc

test:
	@$(REBAR) skip_deps=true eunit

edoc:
	@$(EDOWN)

generate-pi:
	cd rel/raspberrypi && $(REBAR_GEN) generate

generate-galileo:                                                
	cd rel/galileo && $(REBAR_GEN) generate

generate-plc:
	cd rel/plc && $(REBAR_GEN) generate

dialyzer: compile
	@$(DIALYZER) ebin deps/serial/ebin

setup-dialyzer:
	@$(DIALYZER) --build_plt \
                 --apps kernel stdlib mnesia eunit erts crypto
