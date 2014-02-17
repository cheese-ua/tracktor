%% Copyright
-module(consumer_socket).
-author("cheese").

-include("types.hrl").
-behaviour(gen_server).
-define(TIMEOUT_CONNECT, 5000).
-define(TIMEOUT_START, 3000).
-record(state, {
  handler ,
  socket_info :: #socket_info{},
  socket_instance ,
  server_name :: atom(),
  file_name
}).

%% API
-export([start_link/2, send_message/2, handle_message/2, get_file_name/1, handle_one_message/2, execute_handle_messages/2]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-spec(start_link(#socket_info{}, _) -> 'ignore' | {'error',_} | {'ok',pid()}).
start_link(Socket, Handler) ->
  Name = Socket#socket_info.name,
  LogFileName = get_file_name(Name),
  logger:register_file(LogFileName),
  logger:info("start_link: ~w~n", [?MODULE], LogFileName),
  logger:info("ConsumerSocket ~w: ~p~n", [Name, Socket], LogFileName),
  consumer_control:unregister(#consumer_info{name = Name}),
	gen_server:start_link({local, Name}, ?MODULE, [Socket, Handler], []).

send_message(Bytes, ServerName) ->
  gen_server:call(ServerName, {send, Bytes}).


%%%===================================================================
-spec(init([#socket_info{}]) -> {ok,#state{}}).
init([Socket, Handler]) ->
  socket_utilites:timeout_seconds(?TIMEOUT_START),
  Name = Socket#socket_info.name,
  LogFileName = get_file_name(Name),
  logger:info("init: ~w, ~p~n", [?MODULE, Socket], LogFileName),
  gen_server:cast(Name, {init, Socket, Handler}),
  {ok, #state{}}.

%%%===================================================================
handle_call({send, Bytes}, _From, State) ->
  Res = case gen_tcp:send(State#state.socket_instance, Bytes) of
    ok ->
      logger:info("Send [ok] to ~w ~w bytes: ~s~n", [State#state.server_name, erlang:byte_size(Bytes), bytes_extension:bin_to_hexstr(Bytes)], State#state.file_name),
      ok;
    {error, Reason} ->
      logger:info("Send [error: ~w] to ~w ~w bytes: ~s~n", [Reason, State#state.server_name, erlang:byte_size(Bytes), bytes_extension:bin_to_hexstr(Bytes)], State#state.file_name),
      {error, Reason}
  end,
  {reply, Res, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%===================================================================
handle_cast({init, SocketInfo, Handler}, State) ->
  Name = SocketInfo#socket_info.name,
  LogFileName = get_file_name(Name),
  Ip = SocketInfo#socket_info.ip,
  Port = SocketInfo#socket_info.port,
  logger:info("Try connect to ~p:~w~n", [Ip, Port], LogFileName),
  Socket = case gen_tcp:connect(Ip, Port,  [binary, {packet, 0}], ?TIMEOUT_CONNECT) of
    {ok, SocketConnected} ->
      consumer_control:register(#consumer_info{name = Name, pid = self()}),
      SocketConnected;
     {error, Reason} ->
      logger:info("Connect failed: ~w~n", [Reason], LogFileName),
      error(Reason)
  end,
  logger:info("Connected to ~p:~w success: ~w ~n", [Ip, Port, Socket], LogFileName),
  socket_utilites:timeout_seconds(1000),
  NewState = State#state{
     handler = Handler,
     socket_info = SocketInfo,
     socket_instance = Socket,
     file_name = LogFileName,
     server_name = Name
  },
  {noreply, NewState};
%%===================================================================
handle_cast(_Request, State) ->
	{noreply, State}.

%%%===================================================================
handle_info({tcp, RemoteSocket, Bytes}, State) ->
  {ok,{Ip,Port}} = inet:peername(RemoteSocket),
  logger:info("Receive message from ~w [~p:~w] ~w bytes: ~s~n", [State#state.server_name, Ip,Port, erlang:byte_size(Bytes), bytes_extension:bin_to_hexstr(Bytes)], State#state.file_name),
  handle_message(Bytes, State),
  {noreply, State};
%%%===================================================================
handle_info({tcp_closed, RemoteSocket}, State) ->
  logger:info("Client disconnected: ~w~n", [RemoteSocket], State#state.file_name),
  consumer_control:unregister(#consumer_info{name = State#state.server_name, pid = self()}),
  error(disconnect),
  {noreply, State};
%%%===================================================================
handle_info(_Info, State) ->
	{noreply, State}.

%%%===================================================================
terminate(_Reason, _State) ->
	ok.

%%%===================================================================
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


get_file_name(Name) ->
  "log/" ++ atom_to_list(Name) ++ ".log".


%%***************
%% ------------------------------------------------------------------
%% Если обработчик события есть - обрабатываем
%% ------------------------------------------------------------------
handle_message(Bytes, #state{ file_name = FileName} = State) ->
  Messages = socket_utilites:prepare_l2l1_messages_from_bytes(binary:bin_to_list(Bytes), [], FileName),
  execute_handle_messages(Messages, State).

%% ------------------------------------------------------------------
%% Запустить на обработку все сообщения
%% ------------------------------------------------------------------
execute_handle_messages(ignored, State) ->
  logger:info("message ignored", State#state.file_name),
  ok;
execute_handle_messages([], _State) ->
  ok;
execute_handle_messages([{HeadMessage} | OtherMessage], #state{ server_name = _ServerName} = State) ->
  spawn(?MODULE, handle_one_message, [HeadMessage, State]),
  execute_handle_messages(OtherMessage, State).

%% ------------------------------------------------------------------
%% Запустить на обработку одно конкретное сообщение
%% ------------------------------------------------------------------
handle_one_message(Bytes, #state{ handler = Handler, file_name = FileName}) ->
  case Handler of
    undefined -> {undefined, handler_is_absent};
    _ -> try Handler(Bytes) of
           Res -> Res
         catch
           ErrRes ->
             logger:info("Consumer handler error: ~w~n", [ErrRes], FileName),
             ErrRes
         end
  end.

