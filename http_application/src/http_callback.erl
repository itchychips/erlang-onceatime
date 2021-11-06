-module(http_callback).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behavior(elli_handler).

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle(Method, Path, Req) ->
    GitDir = lists:member(<<".git">>, Path),
    if
        GitDir ->
            {404, [], <<"Not Found">>};
        not GitDir ->
            handle_request(Method, Path, Req)
    end.


handle_request('GET', [], _Req) ->
    {ok, [], base_page()};
handle_request('GET', [<<"assets">>|_]=Path, _Req) ->
    {ok, Data} = file:read_file(elli_path_to_list(Path)),
    {ok, [], Data};
handle_request(_, _, _Req) ->
    {404, [], <<"Not Found">>}.


handle_event(invalid_return, [Request, ReturnValue], _) -> 
    io:format("Error: ~p ;; ~p~n", [Request, ReturnValue]),
    ok;
handle_event(_Event, _Data, _Args) ->
    ok.

elli_path_to_list(Path) ->
    Convert = fun(S) -> binary:bin_to_list(S) end,
    lists:flatten(lists:join("/", lists:map(Convert, Path))).

% Page definitions:

base_page() ->
    {ok, Data} = templates:render_or_fail(test, [
        {name, "Donny"},
        {age, middle_aged}
    ]),
    Data.
