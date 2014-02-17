%% Copyright
-module(socket_utilites).
-author("cheese").
-include("types.hrl").

%% API
-export([parseSocketList/1, parseSocket/1, get_name/1, get_name/2, timeout_seconds/1, prepare_l2l1_messages_from_bytes/3, our_list/1]).

%%---------------------------------------------------------
%%  Prepare list of #socket_info from application settings
%%---------------------------------------------------------
-spec(parseSocketList(_)-> [#socket_info{}]).
parseSocketList([]) ->
	[];
parseSocketList([Head | Tail]) ->
	[parseSocket(Head) | parseSocketList(Tail)].

%%---------------------------------------------------------
%%  Prepare #socket_info from application settings
%%---------------------------------------------------------
-spec(parseSocket(_)-> #socket_info{}).
parseSocket([{type, ServerType}, {ip, ServerIP}, {port, ServerPort}, {name, Name}]) ->
	#socket_info{ip = ServerIP, port =ServerPort, type = ServerType,name = Name	}.

%%---------------------------------------------------------
%%  get name param from application setting
%%---------------------------------------------------------
-spec( get_name([term()]) -> atom()).
get_name([]) ->
  unknown;
get_name([{Key, Val} | Tail]) ->
  case Key of
    name -> Val;
    _ -> get_name(Tail)
  end.

%%---------------------------------------------------------
%%  Prepare #socket_info from application settings
%%  and add prefix
%%---------------------------------------------------------
-spec( get_name([term()], [term()]) -> atom()).
get_name(List, Prefix) ->
  Name = get_name(List),
  Suffix = atom_to_list(Name),
  list_to_atom(Prefix ++ Suffix).

timeout_seconds(Seconds) ->
  receive
    illegal_message ->
      ok
  after Seconds ->
    ok
  end.



%% ------------------------------------------------------------------
%% Подготовить сообщения из "пакета"
%% ------------------------------------------------------------------
prepare_l2l1_messages_from_bytes([], Res, _LogFileName) ->
  Res;
prepare_l2l1_messages_from_bytes([L1| [L2 | Bytes]], Res, LogFileName) ->
  HeaderLen = L1*256 + L2,
  DataLen = erlang:length(Bytes),
  if
    HeaderLen > DataLen ->
      logger:info("Header len: ~w, Data len: ~w. Message ignored: ~w~n", [HeaderLen, DataLen, Bytes], LogFileName),
      Res;
    true ->
      Message = lists:sublist(Bytes, HeaderLen),
      Tail = lists:sublist(Bytes, HeaderLen+1, DataLen - HeaderLen),
      prepare_l2l1_messages_from_bytes(Tail, [{erlang:list_to_binary([L1 | [L2 | Message]])} | Res], LogFileName)
  end;
prepare_l2l1_messages_from_bytes(Bytes, Res, LogFileName) ->
  logger:info("Message ignored [invalid size]: ~w~n", [Bytes], LogFileName),
  Res.


our_list(Pos) ->
  fun() ->
    {Pos, our_list(Pos+1)}
  end.
