.PHONY: deps

all: deps compile

compile:
	@./rebar compile

app:
	@./rebar compile skip_deps=true

deps:
	@./rebar get-deps

clean:
	@./rebar clean
	rm -f erl_crash.dump

dist-clean: clean
	@./rebar delete-deps

start: compile 
	erl -pa ebin -pa deps/*/ebin -boot start_sasl -s anekdar
