-module(fizzbuzz_calculator).

-export([start/1, loop/1]).

start(ReplyTo) ->
    Pid = spawn_link(?MODULE, loop, [ReplyTo]),
    {ok, Pid}.

loop(ReplyTo) ->
    receive
        Number ->
            ReplyTo ! {Number, fizzbuzz(Number)},
            loop(ReplyTo)
    end.

fizzbuzz(Number) ->
    Div3 = Number rem 3 =:= 0,
    Div5 = Number rem 5 =:= 0,
    case {Div3, Div5} of
        {true, true} ->
            "Fizzbuzz";
        {true, _} ->
            "Fizz";
        {_, true} ->
            "Buzz";
        _ ->
            Number
    end.
