-module(templates).
-export([add_path/1, compile/0, render/2, render_or_fail/2]).
-license("GNU AGPL-3.0").
-license_link("https://www.gnu.org/licenses/agpl-3.0.txt").

add_path(Path) ->
    code:add_patha(Path).

compile() ->
    Templates = filelib:wildcard("templates/*.html"),
    Compile = fun(RelativePath) ->
        ModuleName = filename:rootname(filename:basename(RelativePath)),
        erlydtl:compile(RelativePath, ModuleName, [{out_dir, "templates"}, report, return, force_recompile]),
        io:format("Compiled template ~p~n", [ModuleName])
    end,
    lists:foreach(Compile, Templates).

render(Module, Variables) ->
    DeclaredVariables = apply(Module, variables, []),
    Keys = sets:from_list(proplists:get_keys(Variables)),
    DeclaredKeys = sets:from_list(proplists:get_keys(DeclaredVariables)),
    Intersected = sets:intersection(Keys, DeclaredKeys),
    KeysLength = sets:size(Keys),
    DeclaredKeysLength = sets:size(DeclaredKeys),
    IntersectedLength = sets:size(Intersected),
    case {KeysLength, DeclaredKeysLength, IntersectedLength} of
        {X, X, X} ->
            apply(Module, render, [Variables]);
        _ ->
            ExcludedKeys = sets:to_list(sets:subtract(DeclaredKeys, Keys)),
            {error, excluded_variables, ExcludedKeys}
    end.

render_or_fail(Module, Variables) ->
    Result = render(Module, Variables),
    case Result of
        {error, excluded_variables, ExcludedKeys} ->
            Message = io_lib:format("Not all variables are included!  Excluded keys: ~p~n", [ExcludedKeys]),
            io:format(Message),
            error(Result);
        _ ->
            Result
    end.


