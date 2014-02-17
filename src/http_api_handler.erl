%%%-------------------------------------------------------------------
%%% @author cheese
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Feb 2014 9:12 AM
%%%-------------------------------------------------------------------
-module(http_api_handler).
-author("cheese").
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init({tcp, http}, Req, _Opts) ->
  io:format("~s~n",["init http_api_handler"]),
  {ok, Req, undefined_state}.

handle(Req, State) ->
  Body = <<"<h1>It works!</h1>">>,
  {ok, Req2} = cowboy_req:reply(200, [], Body, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
