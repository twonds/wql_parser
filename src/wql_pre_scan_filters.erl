%% @hidden

-module(wql_pre_scan_filters).

% API
-export([filter/1]).

% For testing only
-export([fix_missing_quotes/2, fix_missing_parens/2]).

filter(B) when is_binary(B) ->
	B1 = fix_missing_quotes(B, <<>>),
	fix_missing_parens(B1, <<>>).

%%------------------------------------------------------------------
%% Fix Missing Quotes
%%------------------------------------------------------------------

fix_missing_quotes(<<$', Rest/binary>>, Acc) ->
	fix_missing_quotes_q(Rest, <<Acc/binary, $'>>);
fix_missing_quotes(<<$", Rest/binary>>, Acc) ->
	fix_missing_quotes_dq(Rest, <<Acc/binary, $">>);
fix_missing_quotes(<<X, Rest/binary>>, Acc) ->
	fix_missing_quotes(Rest, <<Acc/binary, X>>);
fix_missing_quotes(<<>>, Acc) ->
	Acc.

fix_missing_quotes_q(<<$', Rest/binary>>, Acc) ->
	fix_missing_quotes(Rest, <<Acc/binary, $'>>);
fix_missing_quotes_q(<<X, Rest/binary>>, Acc) ->
	fix_missing_quotes_q(Rest, <<Acc/binary, X>>);
fix_missing_quotes_q(<<>>, Acc) ->
	fix_missing_quotes(<<>>, <<Acc/binary, $'>>).

fix_missing_quotes_dq(<<$", Rest/binary>>, Acc) ->
	fix_missing_quotes(Rest, <<Acc/binary, $">>);
fix_missing_quotes_dq(<<X, Rest/binary>>, Acc) ->
	fix_missing_quotes_dq(Rest, <<Acc/binary, X>>);
fix_missing_quotes_dq(<<>>, Acc) ->
	fix_missing_quotes(<<>>, <<Acc/binary, $">>).


%%------------------------------------------------------------------
%% Fix Missing Parentheses
%%
%% State 1: fix_missing_parens/2
%%     . If you see open "(", go to state 2 
%%     . If you see closing ")", compensate by adding a "(" to front
%%     . Ignore parenthesis nested inside single or double quotes.
%%
%% State 2: fix_missing_parens_open/3
%%     . If you see open "(", go one level deeper
%%     . If you see closing ")": 
%%          - If at depth 1 return to State 1
%%          - Else move up one level
%%     . If you reach end of string, add a compensating ")" to end for 
%%       each missing one (ie one for each level of depth).
%%     . Ignore parenthesis nested inside single or double quotes.
%%-------------------------------------------------------------------

fix_missing_parens(<<QT, Rest/binary>>, Acc) when (QT =:= $') or (QT =:= $") ->
	{R, A} = wql_scan_util:until(QT, Rest, <<Acc/binary, QT>>), 
	fix_missing_parens(R, <<A/binary, QT>>);
fix_missing_parens(<<$(, Rest/binary>>, Acc) ->
	fix_missing_parens_open(Rest, <<Acc/binary, $(>>, 1);
fix_missing_parens(<<$), Rest/binary>>, Acc) ->			% crux
	fix_missing_parens(Rest, <<$(, Acc/binary, $)>>);	% add "(" to front
fix_missing_parens(<<X, Rest/binary>>, Acc) ->
	fix_missing_parens(Rest, <<Acc/binary, X>>);
fix_missing_parens(<<>>, Acc) ->
	Acc.

fix_missing_parens_open(<<QT, Rest/binary>>, Acc, Depth) when  
					(QT =:= $') or (QT =:= $") ->
	{R, A} = wql_scan_util:until(QT, Rest, <<Acc/binary, QT>>), 
	fix_missing_parens_open(R, <<A/binary, QT>>, Depth);
fix_missing_parens_open(<<$(, Rest/binary>>, Acc, Depth) ->		% down
	fix_missing_parens_open(Rest, <<Acc/binary, $(>>, Depth + 1);
fix_missing_parens_open(<<$), Rest/binary>>, Acc, 1) ->			% up to surface
	fix_missing_parens(Rest, <<Acc/binary, $)>>);
fix_missing_parens_open(<<$), Rest/binary>>, Acc, Depth) ->		% up 
	fix_missing_parens_open(Rest, <<Acc/binary, $)>>, Depth - 1);
fix_missing_parens_open(<<X, Rest/binary>>, Acc, Depth) ->
	fix_missing_parens_open(Rest, <<Acc/binary, X>>, Depth);
fix_missing_parens_open(<<>>, Acc, Depth) ->
	Balance = list_to_binary(string:chars($), Depth)),
	fix_missing_parens(<<>>, <<Acc/binary, Balance/binary>>).


