-module(stopper).

-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, stop_application/1, start_link/1]).

stop_application(ApplicationPid) ->
    gen_server:cast(ApplicationPid, stop_application).

start_link(ApplicationPid) ->
    gen_server:start_link(?MODULE, [ApplicationPid], []).

init([ApplicationPid]) ->
    {ok, ApplicationPid}.

handle_call(_, _From, ApplicationPid) ->
    {reply, {error, undefined}, ApplicationPid}.
    
handle_cast(stop_application, ApplicationPid) ->
    erlang:halt(0, [{flush, true}]),
    %Result = application:stop(ApplicationPid, shutdown),
    %io:format("Stopping application ~p: ~p~n", [ApplicationPid, Result]),
    {noreply, ApplicationPid}.
