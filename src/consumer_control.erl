%%%-------------------------------------------------------------------
%%% @author cheese
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2014 8:53 AM
%%%-------------------------------------------------------------------
-module(consumer_control).
-author("cheese").

-include("types.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0, register/1, unregister/1, get_next_consumer/0, send_message/1, send_echo/0]).

%% gen_server callbacks
-export([init/1,  handle_call/3,  handle_cast/2,  handle_info/2,  terminate/2,  code_change/3]).

-define(SERVER, ?MODULE).
-define(LOG_FILE, "log/consumer_control.log").

-record(state, {
  consumers :: [#consumer_info{}]
}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(register(#consumer_info{}) -> ok).
register(Consumer) ->
  gen_server:cast(?SERVER, {register, Consumer}),
  ok.

-spec(unregister(#consumer_info{}) -> ok).
unregister(Consumer) ->
  gen_server:cast(?SERVER, {unregister, Consumer}),
  ok.

-spec(get_next_consumer() -> #consumer_info{} | not_present).
get_next_consumer() ->
  case gen_server:call(?SERVER, get_next_consumer) of
    not_present ->
      not_present;
    {ok, Consumer} ->
      Consumer
  end.

send_echo() ->
  send_message("ping").

send_message(Bytes) ->
  case get_next_consumer() of
    not_present
      ->
      logger:info("message ignored", ?LOG_FILE),
      {failed, not_present };
    Consumer ->
      consumer_socket:send_message(Bytes, Consumer#consumer_info.name)
  end.
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start_link() ->
  logger:register_file(?LOG_FILE),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init([]) ->
  {ok, #state{consumers = []}}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_call(get_next_consumer, _From, State) ->
  Consumers = State#state.consumers,
  case Consumers of
    [] ->
      logger:info("no consumer found", ?LOG_FILE),
      {reply, not_present, State};
    [Head | Tail] ->
      NewState = State#state{
      consumers = Tail ++ [Head]
       },
      logger:info("next consumer: ~w~n", [Head], ?LOG_FILE),
      {reply, {ok, Head}, NewState}
  end;
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
  logger:info("unknown handle_call: ~w~n", [Request], ?LOG_FILE),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_cast({register, OneConsumer}, #state{consumers = Consumers} = State) ->
  logger:info("register consumer: ~w~n", [OneConsumer], ?LOG_FILE),
  NewConsumers = [C || C <- Consumers, C#consumer_info.name /= OneConsumer#consumer_info.name],
  {noreply, State#state{consumers = [OneConsumer | NewConsumers]}};
%%--------------------------------------------------------------------
handle_cast({unregister, OneConsumer}, #state{consumers = Consumers} = State) ->
  logger:info("unregister consumer: ~w~n", [OneConsumer], ?LOG_FILE),
  NewConsumers = [C || C <- Consumers, C#consumer_info.name /= OneConsumer#consumer_info.name],
  {noreply, State#state{consumers = NewConsumers}};
%%--------------------------------------------------------------------
handle_cast(Request, State) ->
  logger:info("unknown handle_cast: ~w~n", [Request], ?LOG_FILE),
  {noreply, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  logger:info("Info: ~w~n", [State], ?LOG_FILE),
  {noreply, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

