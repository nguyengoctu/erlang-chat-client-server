%%%-------------------------------------------------------------------
%%% @author ngoctu
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. May 2019 15:52
%%%-------------------------------------------------------------------
-module(chat_client).
-author("ngoctu").
-compile(export_all).

%% API
-export([client_init/2]).

%% initialize client with username
client_init(Server, Socket) ->
  ok = gen_tcp:send(Socket, "Bonjour\n"),
  ok = gen_tcp:send(Socket, "Choisissez votre nom d'utilisateur: "),
  Username = get_username(Server, Socket),

  ok = gen_tcp:send(Socket, Username ++ ": "),
  Server ! {ready, self(), Username},
  spawn_link(?MODULE, client_listener, [self(), Socket]),
  client_loop(Server, Username, Socket).

%% receive message from the listener, forward it to the server
%% send message received from server back to client via Socket
client_loop(Server, Username, Socket) ->
  receive
    % message from listener
    {send_message, Message} ->
      Server ! {broadcast, self(), Username ++ ": " ++ Message};

    {tcp_closed, Reason} ->
      Server ! {disconnected, self(), Username, Reason},
      exit(normal);

    % message from server
    {receive_message, Message} ->
      ok = gen_tcp:send(Socket, "\r" ++ Message)
  end,

  ok = gen_tcp:send(Socket, Username ++ ": "),
  client_loop(Server, Username, Socket).


%% waiting for input for the client, then send the input message to the client himself
client_listener(Client, Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Pack} ->
      Client ! {send_message, Pack},
      client_listener(Client, Socket);
    {error, Reason} ->
      Client ! {tcp_closed, Reason}
  end.

%% check username exists or not
get_username(Server, Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Pack} ->
      {Username, _} = lists:splitwith(fun(X) -> X =/= $\r end, Pack),
      Server ! {check_username, self(), Username},
      receive
        existed ->
          ok = gen_tcp:send(Socket, "Nom d'utilisateur déjà pris, chosissez un autre: "),
          get_username(Server, Socket);

        ok ->
          Username
      end;

    {error, Reason} ->
      exit(Reason)
  end.


