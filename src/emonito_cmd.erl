-module(emonito_cmd).
-author('marcelog@gmail.com').

-behavior(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Required files.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("emonito.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {
  port = undefined:: undefined|port(),
  cmd = undefined:: undefined|string(),
  logfile = undefined:: undefined|string(),
  info = undefined:: undefined|[term()]
}).
-type state():: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
-export([start_link/3]).
-export([info/1]).
-export([pid/1]).
-export([stop/1]).

%%% gen_server API.
-export([
  init/1, handle_call/3, handle_cast/2, handle_info/2,
  code_change/3, terminate/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link(string(), env(), string()) -> {ok, pid()}|{error, term()}.
start_link(Cmd, Env, Logfile) ->
  gen_server:start_link(?MODULE, [Cmd, Env, Logfile], []).
  
-spec info(pid()) -> [term()].
info(Ref) ->
  gen_server:call(Ref, {status}).

-spec pid(pid()) -> pos_integer().
pid(Ref) ->
  gen_server:call(Ref, {pid}).

-spec stop(pid()) -> ok.
stop(Ref) ->
  gen_server:cast(Ref, {stop}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init([term()]) -> {ok, state()}.
init([Cmd, Env, Logfile]) ->
  process_flag(trap_exit, true),
  Port = start_cmd(Cmd, Env),
  Info = erlang:port_info(Port),
  lager:info("~p started with pid: ~p", [Cmd, proplists:get_value(os_pid, Info)]),
  {ok, #state{
    port = Port,
    cmd = Cmd,
    logfile = Logfile,
    info = Info
  }}.

-spec handle_call(
  term(), {pid(), reference()}, state()
) -> {reply, term() | {invalid_request, term()}, state()}.
handle_call({status}, _From, State=#state{info=Info}) ->
  {reply, Info, State};
  
handle_call({pid}, _From, State=#state{info=Info}) ->
  {reply, get_pid(Info), State};

handle_call(Msg, _From, State) ->
  lager:warning("Unhandled call: ~p", [Msg]),
  {reply, unknown_call, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({stop}, State=#state{port=Port, info=Info}) ->
  stop_cmd(get_pid(Info), Port),
  {stop, normal, State};

handle_cast(Msg, State) ->
  lager:warning("Unhandled cast: ~p", [Msg]),
  {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({_Port, {data, {noeol, _D}}}, State) ->
  {noreply, State};

handle_info({_Port, {data, {eol, _D}}}, State) ->
  {noreply, State};

handle_info({Port, {exit_status, N}}, State=#state{cmd=Cmd, port=Port}) ->
  lager:error("~p died with status: ~p", [Cmd, N]),
  {stop, process_died, State};

handle_info(Msg, State) ->
  lager:warning("Unhandled msg: ~p", [Msg]),
  {noreply, State}.

-spec code_change(string(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-spec terminate(atom(), state()) -> ok.
terminate(_Reason, _State=#state{cmd=Cmd, port=Port, info=Info}) ->
  lager:info("Terminating: ~p", [Cmd]),
  stop_cmd(get_pid(Info), Port),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_cmd(Cmd, Env) ->
  lager:info("Starting: ~p", [Cmd]),
  erlang:open_port(
    {spawn, Cmd}, [exit_status, use_stdio, {env, Env}, {line, 1024}]
  ).

stop_cmd(Pid, Port) ->
  erlang:port_close(Port),
  os:cmd(io_lib:format("/bin/kill -15 ~p", [Pid])).

get_pid(Info) ->
  proplists:get_value(os_pid, Info).

