Build: Use `make` or `rebar3 compile`.

I wanted to hand-roll an HTTP application.  It's not stateful, but the
templating and assets get were a nice time sink for the day.

I did like using make more than rebar3.  Also using `make analyze` to run
dialyzer was really nice.  Make is just a good standard frontend program to
run.

The hardest issue was figuring out how and where to compile the templates.  For
now, they just reside beside their sources, but later they might go somewhere
else.  Also, I have no idea how to package this application; I assert this is a
problem for future me.

Layout of the project:
    - rebar.config
        - Rebar3 config for compilation and dependency resolution
    - rebar.lock
        - Lockfile file the dependencies; similar to package-lock.json for
          frontend dev.
    - src
        - Source files
        - src/http_application.app.src
            - Not entirely sure what this file is.
        - src/http_application_app.erl
            - The application.  This is basically the full application
              initialization step before it starts the supervisor to actually
              start worker processes.
        - src/http_application_sup.erl
            - The supervisor.  Pretty much this is where I consider to be the
              main entry point, and would spawn other supervisors or other
              processes.
        - src/templates.erl
            - A safe templates module to do some basic validation before using
              a template.
    - templates
        - Templates for the frontend pages.  This is compiled during
          application initialization normally, or via templates:compile().

Some future possible additions:
    - Be able to handle POST requests for some sort of stateful project
    - Watch the templates directory and recompile templates on the fly
