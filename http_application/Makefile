all: submodule
	rebar3 compile

submodule:
	git submodule init assets
	git submodule update assets

analyze:
	rebar3 dialyzer

clean:
	rm -rf _build
	rm -rf templates/*.beam

superclean: clean
	rm -rf rebar.lock
