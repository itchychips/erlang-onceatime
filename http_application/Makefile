all: submodule
	rebar3 compile

submodule:
	./initialize_submodules.sh

analyze:
	rebar3 dialyzer

clean:
	rm -rf _build
	rm -rf templates/*.beam

superclean: clean
	rm -rf rebar.lock
