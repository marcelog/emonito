-module(emonito_sup).

-behavior(supervisor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Required files.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("emonito.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
-export([start_link/0]).
-export([start_cmd/3, stop_cmd/1]).

%%% supervisor API.
-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Starts the supervisor.
-spec start_link() -> {ok, pid()}|{error, term()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start and supervise the given command.
-spec start_cmd(string(), env(), string()) -> supervisor:startchild_ret().
start_cmd(Cmd, Env, Logfile) ->
  supervisor:start_child(?MODULE, [Cmd, Env, Logfile]).

%% @doc Terminates the given command.
-spec stop_cmd(pid()) -> ok|{error, term()}.
stop_cmd(CmdRef) ->
  supervisor:terminate_child(?MODULE, CmdRef).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% supervisor API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns children specs.
-spec init([term()]) -> [supervisor:child_spec()].
init([]) ->
  {ok, {{simple_one_for_one, 5, 10}, [
    {cmd,
      {emonito_cmd, start_link, []},
      permanent, 10000, worker, [emonito_cmd]
    }
  ]}}.
