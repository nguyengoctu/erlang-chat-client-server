%%%-------------------------------------------------------------------
%%% @author ngoctu
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. May 2019 17:50
%%%-------------------------------------------------------------------
-module(chat_gen_server).
-author("ngoctu").
-behavior(gen_server).
-record(state, {listen_socket,
                clients = [],
                message_history = []}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% API calls
-export([start_link/0, worker/2, stop/0]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, terminate).

init([]) ->
  io:format("Starting chat server with pid ~p...~n",[self()]),
  io:format("Listening to incoming request...~n"),
  case gen_tcp:listen(4000, [{reuseaddr, true}, {packet, 0}, {active, false}]) of
    {ok, LSocket} ->
      spawn(?MODULE, worker, [self(), LSocket]),
      {ok, #state{listen_socket = LSocket,
                  clients = orddict:new(),
                  message_history = []}};
    Error ->
      {stop, Error}
  end.


handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.


handle_cast(new_worker, State) ->
  spawn(?MODULE, worker, [self(), State#state.listen_socket]),
  {noreply, State}.


terminate(_Reason, #state{listen_socket=ListenSocket}) ->
  gen_tcp:close(ListenSocket),
  ok.

worker(Server, LSocket) ->
  case gen_tcp:accept(LSocket) of
    {ok, CSocket} ->
      io:format("Incoming user from ~p~n", [CSocket]),
      gen_server:cast(Server, new_worker);

%%      chat_client:client_init(Server, CSocket);
    _ ->
      error
  end,
  io:format("~p worker died~n", [self()]).