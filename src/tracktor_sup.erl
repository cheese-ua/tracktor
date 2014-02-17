-module(tracktor_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(LOG_FILE, "log/init.log").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  io:format("start_link: ~w~n",[?MODULE]),
  logger:register_file(?LOG_FILE),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  io:format("init: ~w~n",[?MODULE]),

  LogWorker = {logger_sup,
    {logger, start_link, []},
    permanent, 2000, worker,
    []},

  {ok, {
    {one_for_one, 2, 5},
    [LogWorker]
  }
  }.
