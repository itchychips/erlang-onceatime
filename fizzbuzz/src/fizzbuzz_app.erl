%%%-------------------------------------------------------------------
%% @doc fizzbuzz public API
%% @end
%%%-------------------------------------------------------------------

-module(fizzbuzz_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(WORKER_COUNT, 16).
%-define(MAX_NUMBER, 1000000000).
%-define(MAX_NUMBER, 1000000).
-define(MAX_NUMBER, 25).

start(_StartType, _StartArgs) ->
    {ok, SupervisorPid} = fizzbuzz_sup:start_link(),
    {ok, StopperPid} = supervisor:start_child(SupervisorPid, #{id => stopper, start => {stopper, start_link, [self()]}}),
    {ok, WriterPid} = supervisor:start_child(SupervisorPid, #{id => writer, start => {writer, start_link, [StopperPid, ?MAX_NUMBER]}}),
    Calculators = lists:map(fun(X) ->
        Id = io_lib:format("~p~p", ["Worker", X]),
        {ok, Pid} = supervisor:start_child(SupervisorPid, #{id => Id, start => {fizzbuzz_calculator, start, [WriterPid]}}),
        Pid
    end, lists:seq(1,?WORKER_COUNT)),
    %lists:foreach(fun(Natural) ->
    %        WorkerNumber = (Natural rem ?WORKER_COUNT) + 1,
    %        Pid = lists:nth(WorkerNumber, Calculators),
    %        Pid ! Natural
    %    end, lists:seq(1, ?MAX_NUMBER)),
    distribute_numbers(Calculators, ?MAX_NUMBER),
    {ok, self()}.

stop(_State) ->
    ok.

%% internal functions

distribute_numbers(Workers, Max) ->
    distribute_numbers(Workers, 1, Max).

distribute_numbers(_, N, Max) when N > Max ->
    ok;
distribute_numbers(Workers, N, Max) ->
    WorkerNumber = (Max rem ?WORKER_COUNT) + 1,
    Pid = lists:nth(WorkerNumber, Workers),
    Pid ! N,
    distribute_numbers(Workers, N+1, Max).
