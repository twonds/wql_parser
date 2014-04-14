%% @hidden

-module(wql_post_scan_filters).
-export([filter/1]).

-include("../include/wql_scanner.hrl").

filter(Tokens) when is_list(Tokens) ->
	without_unparsable_NOTs(Tokens, []).
	
%%------------------------------------------------------------------
%% Remove illegal NOTs that the parser won't be able to handle
%%------------------------------------------------------------------

%%
%% NOT is only a valid token when it comes before 
%%    (a) a keyword token
%%    (b) a field token
%%    (c) an opening parens token
%% Note that (a) and (b) have a token size of 3 while (c) has a token
%% size of only two, which is relevant in the pattern matching below.
%% 

without_unparsable_NOTs([{'NOT', _} = T | [{Next, _, _} | _] = Rest], Acc) when 
						Next =:= 'KEYWORD';
						Next =:= 'FIELD' ->
	without_unparsable_NOTs(Rest, [T | Acc]);
without_unparsable_NOTs([{'NOT', _} = T | [{'(', _} | _] = Rest], Acc) ->
	without_unparsable_NOTs(Rest, [T | Acc]);
without_unparsable_NOTs([{'NOT', _} | Rest], Acc) ->
	without_unparsable_NOTs(Rest, Acc);
without_unparsable_NOTs([T | Rest], Acc) ->
	without_unparsable_NOTs(Rest, [T | Acc]);
without_unparsable_NOTs([], Acc) ->
	lists:reverse(Acc).

