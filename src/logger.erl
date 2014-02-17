%%%-------------------------------------------------------------------
%%% @author cheese
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jan 2014 1:38 PM
%%%-------------------------------------------------------------------
-module(logger).
-author("cheese").

-behaviour(gen_server).

%% API
-export([start_link/0, info/2, info/3, register_file/1]).

%% gen_server callbacks
-export([init/1,  handle_call/3,  handle_cast/2,  handle_info/2,  terminate/2,  code_change/3, delete_file/3, move_file/1, check_file_size/1, zip/1, check_size/1]).

-define(SERVER, ?MODULE).
-define(TIMEOUT_CHECK_SIZE, 300000).

-define(LOG_FILE, "log/logger.log").


-record(state, {files :: [term()]}).

%%%===================================================================
%%% API
%%%===================================================================
info(Message, FileName) ->
  gen_server:cast(?SERVER, {info, self(), Message, FileName}).

info(Format, Data, FileName) ->
  gen_server:cast(?SERVER, {info, self(), Format, Data, FileName}).

register_file(FileName) ->
  gen_server:cast(?SERVER, {register, self(), FileName}).

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
start_link() ->
  io:format("start_link: ~w~n",[?MODULE]),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
init([]) ->
  io:format("init: ~w~n",[?MODULE]),
  erlang:send_after(?TIMEOUT_CHECK_SIZE, self(), check_size),
  {ok, #state{files = []}}.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
handle_cast({register, _Pid, FileName}, #state{ files = Files} = State) ->
  info("Register file: ~p~n", [FileName], ?LOG_FILE),
  check_file_size(FileName),
  NewFiles = delete_file(FileName, Files, []),
  info("Files: ~p~n", [NewFiles], ?LOG_FILE),
  {noreply, State#state{files = [FileName | NewFiles] }};
%%--------------------------------------------------------------------
handle_cast({info, Pid, Message, FileName}, State) ->
  {{Year, Month, Day}, {Hour, Min, Second} } = calendar:local_time(),
  Date = lists:flatten(io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B ~w ", [Year, Month, Day, Hour, Min, Second, Pid])),
  MessageDate = io_lib:fwrite("~s ~s~n",[Date , Message]),
  io:format("~s",[MessageDate]),
  file:write_file(FileName, MessageDate, [append]),
  {noreply, State};
%%--------------------------------------------------------------------
handle_cast({info, Pid, Format, Data, FileName}, State) ->
  {{Year, Month, Day}, {Hour, Min, Second} } = calendar:local_time(),
  Date = lists:flatten(io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B ~w ", [Year, Month, Day, Hour, Min, Second, Pid])),
  Message = io_lib:fwrite(Format, Data),
  MessageDate = io_lib:fwrite("~s ~s",[Date , Message]),
  io:format("~s",[MessageDate]),
  file:write_file(FileName, MessageDate, [append]),
  {noreply, State};
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
handle_info(check_size, #state{files = Files}= State) ->
  info("check_size: ~p~n", [Files], ?LOG_FILE),
  spawn(?MODULE, check_size, [Files]),
  erlang:send_after(?TIMEOUT_CHECK_SIZE, self(), check_size),
  {noreply, State};
%%--------------------------------------------------------------------
handle_info(status, #state{files = Files}= State) ->
  info("Files: ~p ~n", [Files], ?LOG_FILE),
  {noreply, State};
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
delete_file(_FileName, [], Res) ->
  Res;
delete_file(FileName, [HeadFile | TailFiles], Res) ->
  case lists_extension:are_equal(HeadFile,FileName) of
     true ->
      delete_file(FileName, TailFiles, Res);
    _OtherFileName ->
      delete_file(FileName, TailFiles, [HeadFile | Res])
  end.


check_size([]) ->
  ok;
check_size([File | Other]) ->
  check_file_size(File),
  check_size(Other).


check_file_size(FileName) ->
  FileSize = filelib:file_size(FileName),
  if
    FileSize > 25000000 ->
      move_file(FileName);
    true ->
        ok
  end.

move_file(FileName) ->
  FileNameAbs = filename:absname(FileName),
  FileNameWithoutExt = filename:basename(FileName),
  FileDir = filename:absname(filename:dirname(FileName)),
  {{Year, Month, _}, _ } = calendar:local_time(),
  DateDir = lists:flatten(io_lib:format("~B~2.10.0B", [Year, Month])),

  ArchDir= FileDir ++ "/arch",
  file:make_dir(ArchDir),

  NewFileDir =ArchDir ++ "/"++ DateDir,
  file:make_dir(NewFileDir),

  NewFileName = NewFileDir ++ "/" ++ FileNameWithoutExt ++ ".arch",
  Res = file:rename(FileNameAbs, NewFileName),
  info("rename: ~p [~p] to [~p] ~n", [Res, FileNameAbs, NewFileName], ?LOG_FILE),
  spawn(?SERVER, zip, [NewFileName]).

zip(FileName) ->
  {{Year, Month, Day}, {Hour, Min, Second} } = calendar:local_time(),
  Date = lists:flatten(io_lib:format("~B~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B", [Year, Month, Day, Hour, Min, Second])),

  FileDir = filename:absname(filename:dirname(FileName)),
  FileNameWithoutExt = filename:basename(FileName),

  ZipCommand = "zip -mj " ++FileDir ++ "/" ++ Date ++ "_" ++ FileNameWithoutExt ++ ".zip " ++ FileName,
  info("ZipCommand: ~p~n", [ZipCommand], ?LOG_FILE),
  os:cmd(ZipCommand).