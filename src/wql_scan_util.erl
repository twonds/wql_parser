%% @hidden

-module(wql_scan_util).
-export([until/3]).
	

%%-------------------------------------------------------------------
%% until/3 assumes requested charcter exists somewhere. It doesn't 
%% return the request character with either Rest or Acc.
%%-------------------------------------------------------------------

until(QT, <<QT, Rest/binary>>, Acc) ->
	{Rest, Acc};
until(QT, <<X, Rest/binary>>, Acc) ->
	until(QT, Rest, <<Acc/binary, X>>).

