all:
	rebar3 compile

analyze:
	rebar3 dialyzer

clean:
	rm -rf _build
	rm -rf templates/*.beam

superclean: clean
	rm -rf rebar.lock
