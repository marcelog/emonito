-module(emonito).
-author('marcelog@gmail.com').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Required files.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("emonito.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
-export([start/0, stop/0]).
-export([info/1, pid/1]).
-export([run/3, kill/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Starts the application.
-spec start() -> ok|{error, term()}.
start() ->
  application:start(emonito).

%% @doc Stops the application.
-spec stop() -> ok|{error, term()}.
stop() ->
  application:stop(emonito),
  ok.

%% @doc Runs and supervise the given command.
-spec run(string(), env(), string()) -> supervisor:startchild_ret().
run(Cmd, Env, Logfile) ->
  emonito_sup:start_cmd(Cmd, Env, Logfile).

%% @doc Kills the given command.
-spec kill(pid()) -> ok|{error, term()}.
kill(CmdRef) ->
  emonito_sup:stop_cmd(CmdRef).

%% @doc Returns pid of the command.
-spec pid(pid()) -> pos_integer().
pid(CmdRef) ->
  emonito_cmd:pid(CmdRef).

%% @doc Returns port info for this command.
-spec info(pid()) -> [term()].
info(CmdRef) ->
  emonito_cmd:info(CmdRef).