%%%-------------------------------------------------------------------
%% @doc http_application public API
%% @end
%%%-------------------------------------------------------------------

-module(http_application_app).
-license("GNU AGPL-3.0").
-license_link("https://www.gnu.org/licenses/agpl-3.0.txt").

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %{ok, _} = application:ensure_all_started(elli),
    http_application_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
