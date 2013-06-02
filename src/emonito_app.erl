-module(emonito_app).
-author('marcelog@gmail.com').

-behaviour(application).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Application callbacks
-export([start/2, stop/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Application API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Starts the application.
-spec start(
  normal | {takeover, node()} | {failover,node()}, term()
) -> {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_StartType, _StartArgs) ->
  emonito_sup:start_link().

%% @doc Stops the application.
-spec stop(term()) -> ok.
stop(_State) ->
  ok.
