%%%-------------------------------------------------------------------
%%% @author ngoctu
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. May 2019 14:22
%%%-------------------------------------------------------------------
-module(chat_server).
-author("ngoctu").
-record(state, {listen_socket,
                clients,
                message_history = []}).

-compile(export_all).

start(Port) ->
  register(?MODULE, Pid = spawn(?MODULE, init, [Port])),
  {Pid, Port}.


init(Port) ->
  {ok, LSocket} = gen_tcp:listen(Port, [{reuseaddr, true}, {packet, 0}, {active, false}]),
  spawn(?MODULE, worker, [?MODULE, LSocket]),
  loop(#state{listen_socket = LSocket, clients = orddict:new(), message_history = []}).


worker(Server, LSocket) ->
  case gen_tcp:accept(LSocket) of
    {ok, CSocket} ->
      io:format("Incoming user from ~p~n", [CSocket]),
      Server ! new_worker;

    _ ->
      error
  end,
  io:format("~p worker died~n", [self()]).


loop(S) ->
  receive
    {ready, From, Username} ->
      io:format("~p with pid ~p has joined the chat room~n", [Username, From]),
      loop(S);

    new_worker ->
      Pid = spawn(?MODULE, worker, [?MODULE, S#state.listen_socket]),
      io:format("Starting new worker ~p for listening to new connection~n", [Pid]),
      loop(S);

    {check_username, From, Username} ->
      loop(S);

    {broadcast, From, Message} ->
      loop(S);

    _ ->
      error,
      loop(S)
  end.