-module(writer).

-export([start_link/2, loop/3]).

start_link(StopperPid, MaxNatural) ->
    Writer = spawn_link(?MODULE, loop, [StopperPid, MaxNatural, 1]),
    {ok, Writer}.

loop(StopperPid, MaxNatural, Natural) ->
    receive
        {Natural, Number} ->
            if Natural < MaxNatural ->
                write(Number),
                loop(StopperPid, MaxNatural, Natural+1);
               Natural >= MaxNatural ->
                stopper:stop_application(StopperPid),
                loop(StopperPid, MaxNatural, Natural+1)
            end;
        X when not is_tuple(X) ->
            loop(StopperPid, MaxNatural, Natural)
    end.

write(String) when is_list(String) ->
    io:format("~s~n", [String]);
write(Number) ->
    io:format("~p~n", [Number]).
