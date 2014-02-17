-module(tracktor_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
  io:format("start/0: ~w~n",[?MODULE]),
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  application:start(tracktor_app).

start(_StartType, _StartArgs) ->
  io:format("start/2: ~w~n",[?MODULE]),
  Dispatch = cowboy_router:compile([
    %% {URIHost, list({URIPath, Handler, Opts})}
    {'_', [{'_', http_api_handler, []}]}
  ]),

  %% Name, NbAcceptors, TransOpts, ProtoOpts
  Res = cowboy:start_http(my_http_listener, 100,
    [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
  ),
  io:format("cowboy:start_http: ~w. Res: ~w~n", [?MODULE, Res]),

  io:format("cowboy:start_http: ~w~n",[?MODULE]),
  tracktor_sup:start_link().

stop(_State) ->
  application:stop(crypto),
  application:stop(ranch),
  application:stop(cowboy),
  ok.
