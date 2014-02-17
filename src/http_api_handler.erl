%%%-------------------------------------------------------------------
%%% @author cheese
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2014 9:12 AM
%%%-------------------------------------------------------------------
-module(http_api_handler).
-behaviour(cowboy_http_handler).
-author("cheese").

-define(LOG_FILE, "log/http.log").
-define(HTTP_TIMEOUT, 10000).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([process_titp/2,process_tic/2, prepare_response/3]).


init({tcp, http}, Req, _Opts) ->
  {ok, Req, undefined_state}.

handle(Req, State) ->
  {Path, Req2} = cowboy_req:path(Req),
  {ok, BodyRequest, Req3} = cowboy_req:body(Req2),
  Fun = case Path of
    <<"/titp">> ->
      logger:info("TITP request: ~s~n", [BodyRequest], ?LOG_FILE),
      fun http_api_handler:process_titp/2;
    <<"/tic">> ->
      logger:info("TIC request: ~s~n", [BodyRequest], ?LOG_FILE),
      fun http_api_handler:process_tic/2;
    Other ->
      logger:info("Not supported route: ~s [~s]~n", [Other, BodyRequest], ?LOG_FILE),
      undefined
  end,

  case Fun of
    undefined ->
      logger:info("Not processed", ?LOG_FILE),
      {ok, prepare_response(500, <<"">>, Req3), State};
    _ ->
      FunResp = Fun(BodyRequest, self()),
      logger:info("Result: ~w~n", [FunResp], ?LOG_FILE),
      ReqLastResp = receive
        {ok, Resp} ->
          logger:info("OK response: ~s~n", [Resp], ?LOG_FILE),
          prepare_response(200, Resp, Req3);
        {error, Reason} ->
          logger:info("Response error: ~w~n", [Reason], ?LOG_FILE),
          prepare_response(500, <<"">>, Req3)
      after ?HTTP_TIMEOUT ->
          logger:info("Response timeout", ?LOG_FILE),
          prepare_response(500, <<"">>, Req3)
      end,
      {ok, ReqLastResp, State}
  end.

prepare_response(HttpStatus, Data, CowboyReq) ->
  {ok, ReqLast} = cowboy_req:reply(HttpStatus, [], Data, CowboyReq),
  ReqLast.

process_titp(_Data, _Pid) ->
  logger:info("TITP process...",  ?LOG_FILE),
  {error, not_implemented}.

process_tic(_Data, _Pid) ->
  logger:info("TIC process...",  ?LOG_FILE),
  {error, not_implemented}.

terminate(_Reason, _Req, _State) ->
  ok.
