%%%-------------------------------------------------------------------
%%% @author cheese
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jan 2014 2:28 PM
%%%-------------------------------------------------------------------
-module(lists_extension).
-author("cheese").

%% API
-export([are_equal/2, are_equal_internal/2]).

are_equal(List1, List2) when erlang:length(List1) == erlang:length(List2) ->
  are_equal_internal(List1, List2);
are_equal(_List1, _List2) ->
  false.

are_equal_internal([], []) ->
  true;
are_equal_internal([Head1 | Tail1], [Head2 | Tail2])  when Head1 == Head2 ->
  are_equal_internal(Tail1, Tail2);
are_equal_internal(_List1, _List2) ->
  false.