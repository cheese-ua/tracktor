%% Copyright
-module(consumer_socket_sup).
-author("cheese").

-include("types.hrl").
-behaviour(supervisor).
-define(LOG_FILE, "log/consumer_control.log").

%% API
-export([start_link/1, prepare_workers/2, handle_message/1]).

%% supervisor
-export([init/1]).

%% API
-spec(start_link([]) -> 'ignore' | {'error',_} | {'ok',pid()}).
start_link(Socket) ->
  logger:register_file(?LOG_FILE),
  logger:info("Start supervisor ~w~n", [?MODULE], ?LOG_FILE),
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Socket]).


%% supervisor callbacks
init([ClientsSockets]) ->
  logger:info("Init supervisor ~w: ~p~n", [?MODULE, ClientsSockets], ?LOG_FILE),

  WorkersSockets = prepare_workers(ClientsSockets, []),

  WorkerControl = {consumer_control,
    {consumer_control, start_link, []},
    permanent, 2, worker,
    []},

	{ok, {
    {one_for_one, 200000000, 1},
    [WorkerControl | WorkersSockets]}}.


-spec(prepare_workers([term()], [#socket_info{}]) -> [{atom(), {consumer_socket, start_link, [#socket_info{}]},  permanent, 2, worker,  []}]).
prepare_workers([], Res) ->
  lists:reverse(Res);
prepare_workers([Socket | Tail], Res) ->
  #socket_info{name = Name} = Socket,
  Suffix = atom_to_list(Name),
  WorkerName = list_to_atom("consumer_worker_" ++ Suffix),
  NewRes = [
    {WorkerName,
      {consumer_socket, start_link, [Socket, fun consumer_socket_sup:handle_message/1]},
      permanent, 2, worker,
      []}
     | Res],
  prepare_workers(Tail, NewRes).

handle_message(Bytes) ->
  main_socket:send_to_last_accepted(Bytes).