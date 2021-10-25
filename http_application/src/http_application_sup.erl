%%%-------------------------------------------------------------------
%% @doc http_application top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(http_application_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    templates:add_path("templates"),
    templates:compile(),
    ElliOpts = [{callback, http_callback}, {port, 8081}],
    ElliSpec = {
        http_callback,
        {elli, start_link, [ElliOpts]},
        permanent,
        5000,
        worker,
        [elli]},
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [ElliSpec],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
