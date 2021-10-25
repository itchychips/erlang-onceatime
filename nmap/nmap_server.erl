-module(nmap_server).
-license("GNU AGPL-3.0").
-license_link("https://www.gnu.org/licenses/agpl-3.0.txt").

-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, test_port/3, run_test/2, port_tester/2, profile/1]).

-define(MAX_PORT, 1024).
-define(TIMEOUT, 120000).

%start() ->
%    undefined.

profile(Host) ->
    WorkerCounts = [
        %64,
        %128,
        %256,
        %512,
        %1024,
        %2048,
        4096,
        %8192,
        %16384,
        32768,
        65535
    ],
    Run = fun(WorkerCount) ->
        io:format("Testing ~p workers.~n", [WorkerCount]),
        Result = catch timer:tc(?MODULE, run_test, [Host, WorkerCount]),
        %io:format("~p~n", [Result]),
        case Result of
            {'EXIT', timeout} ->
                io:format("Failed.~n");
            {Microseconds, _} ->
                Seconds = Microseconds / 1000000,
                io:format("Result: ~p seconds~n", [Seconds]);
            Other ->
                io:format("Unexpected failure: ~p.~n", [Other])
        end
    end,
    lists:map(Run, WorkerCounts).

run_test(Host, WorkerCount) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [self(), Host, WorkerCount], []),
    gen_server:cast(Pid, test_ports),
    receive
        {ports_up, PortsUp} -> PortsUp
    after ?TIMEOUT ->
        gen_server:stop(Pid, timeout, 0),
        io:format("Timeout.~n")
    end.

init([From, Host, WorkerCount]) ->
    Workers = [spawn_link(?MODULE, port_tester, [self(), Host]) || _ <- lists:seq(1, WorkerCount)],
    {ok, #{host => Host, ports_up => [], ports_tested => 0, from => From, workers => Workers}}.

handle_call(test_ports, _, State) ->
    {reply, ok, State}.

handle_cast(test_ports, State) ->
    %io:format("Testing ports.~n", []),
    PortsToTest = lists:seq(1, ?MAX_PORT),
    Distributor = fun(Port) ->
        Workers = maps:get(workers, State),
        WorkerNumber = 1 + Port rem length(Workers),
        Worker = lists:nth(WorkerNumber, Workers),
        Worker ! {port, Port} end,
    lists:foreach(Distributor, PortsToTest),
    {noreply, State};
handle_cast({port_down, _Port}, State) ->
    %io:format("Port ~p is down.~n", [Port]),
    %io:format(","),
    PortsTested = maps:get(ports_tested, State)+1,
    noreply(PortsTested, State);
handle_cast({port_up, Port}, State) ->
    %io:format("Port ~p is up.~n", [Port]),
    PortsUp = [Port|maps:get(ports_up, State)],
    %io:format("."),
    PortsTested = maps:get(ports_tested, State) + 1,
    NewState = maps:update(ports_up, PortsUp, State),
    noreply(PortsTested, NewState).

%handle_info({'DOWN', _Reference, process, _Pid, Reason}, State) ->
%    io:format("Port test error: ~p~n", [Reason]),
%    PortsTested = maps:get(ports_tested, State) + 1,
%    {noreply, maps:update(ports_tested, PortsTested+1, State)}.

noreply(?MAX_PORT, State) ->
    From = maps:get(from, State),
    %io:format("~p~n", [From]),
    From ! {ports_up, maps:get(ports_up, State)},
    FinalState = maps:update(ports_tested, ?MAX_PORT, State),
    io:format("Shutting down nmap server.~n"),
    {stop, normal, FinalState};
noreply(PortsTested, State) ->
    %From = maps:get(from, State),
    %io:format("~p~n", [From]),
    {noreply, maps:update(ports_tested, PortsTested, State)}.

port_tester(From, Host) ->
    receive
        {port, Port} ->
            %io:format("Testing ~p~n", [Port]),
            Result = gen_tcp:connect(Host, Port, []),
            %io:format("Result: ~p~n", [Result]),
            parse_result(Result, From, Host, Port);
        stop ->
            ok
    end.

test_port(From, Host, Port) ->
    Result = gen_tcp:connect(Host, Port, []),
    parse_result(Result, From, Host, Port).

parse_result({error,econnrefused}, From, Host, Port) ->
    gen_server:cast(From, {port_down, Port}),
    port_tester(From, Host);
parse_result({error,etimedout}, From, Host, Port) ->
    %io:format("~p~n", [From]),
    %io:format("~p~n", [Port]),
    gen_server:cast(From, {port_down, Port}),
    port_tester(From, Host);
parse_result({ok, Connection}, From, Host, Port) ->
    gen_tcp:close(Connection),
    gen_server:cast(From, {port_up, Port}),
    port_tester(From, Host).
    %if
    %    PortsTested =:= 65535 ->
    %        From = maps:get(from, State),
    %        From ! {result, maps:get(ports_up, State)};
    %    PortsTested =/= 65535 ->
    %        {noreply, maps:update(ports_tested, PortsTested, State)}
    %end.
