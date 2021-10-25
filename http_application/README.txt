Build: Use `make` or `rebar3 compile`.

I wanted to hand-roll an HTTP application.  It's not stateful, but the
templating and assets get were a nice time sink for the day.

I did like using make more than rebar3.  Also using `make analyze` to run
dialyzer was really nice.

The hardest issue was figuring out how and where to compile the templates.  For
now, they just reside beside their sources, but later they might go somewhere
else.  Also, I have no idea how to package this application; I assert this is a
problem for future me.
