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
                clients = [],
                message_history = []}).

-export([start/1, stop/0, init/1, worker/2]).


start(Port) ->
  io:format("Starting chat server...~n"),
  register(?MODULE, Pid = spawn(?MODULE, init, [Port])),
  io:format("Chat server started with pid ~p~n", [Pid]).


stop() ->
  io:format("Server is shutting down...~n"),
  Server = whereis(?MODULE),
  Server ! down,
  exit(Server, normal).


init(Port) ->
  {ok, LSocket} = gen_tcp:listen(Port, [{reuseaddr, true}, {packet, 0}, {active, false}]),
  spawn(?MODULE, worker, [?MODULE, LSocket]),
  loop(#state{listen_socket = LSocket, clients = orddict:new(), message_history = []}).


worker(Server, LSocket) ->
  case gen_tcp:accept(LSocket) of
    {ok, CSocket} ->
      io:format("Incoming user from ~p~n", [CSocket]),
      Server ! new_worker,
      chat_client:client_init(Server, CSocket);
    _ ->
      error
  end,
  io:format("~p worker died~n", [self()]).


loop(S) ->
  receive
    {ready, From, Username} ->
      io:format("~p with pid ~p has joined the chat room~n", [Username, From]),
      JoinMessage = Username ++ " dit bonjour!\n",
      broadcast_message(JoinMessage, From, S#state.clients),

      io:format("Send messsage history to ~p.~n", [Username]),
      send_history(From, lists:reverse(S#state.message_history)),

      UpdatedClients = orddict:store(Username, From, S#state.clients),
      loop(S#state{clients = UpdatedClients});

    new_worker ->
      Pid = spawn(?MODULE, worker, [?MODULE, S#state.listen_socket]),
      io:format("Starting new worker ~p for listening to new connection~n", [Pid]),
      loop(S);

    {check_username, From, Username} ->
      case orddict:find(Username, S#state.clients) of
        {ok, _} ->
          From ! existed;

        error ->
          From ! ok
      end,
      loop(S);

    {broadcast, From, Message} ->
      broadcast_message(Message, From, S#state.clients),
      UpdatedHistory = [Message | S#state.message_history],
      loop(S#state{message_history = UpdatedHistory});


    {disconnected, From, Username, _Reason} ->
      LeaveMessage = Username ++ " s'est deconnecté!\n",
      broadcast_message(LeaveMessage, From, S#state.clients),
      UpdatedClients = orddict:erase(Username, S#state.clients),
      loop(S#state{clients = UpdatedClients});

    down ->
      gen_tcp:close(S#state.listen_socket),
      lists:map(fun({_Username, UserPid}) -> UserPid ! {server_down} end, S#state.clients);

    _ ->
      oops,
      loop(S)
  end.


%% Client sends message to Clients (except himself)
broadcast_message(Message, Client, Clients) ->
  BroadList = lists:filter(fun({_Username, UserPid}) -> UserPid =/= Client
                           end, Clients),
  lists:map(fun({_Username, UserPid}) -> UserPid ! {receive_message, Message} end, BroadList).


%% when a new user comes, message history will be sent to him
send_history(_Client, []) -> ok;
send_history(Client, [Message | MessageHistory]) ->
  Client ! {receive_message, Message},
  send_history(Client, MessageHistory).