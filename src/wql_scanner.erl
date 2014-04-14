%% @hidden

-module(wql_scanner).
-export([scan/1]).

-include("../include/wql_scanner.hrl").
-define(TL, 1).

scan(S) ->
	B = pre_scan(list_to_binary(string:strip(S))),
	Tokens = scan(B, []),
	{ok, post_scan(Tokens)}.

pre_scan(B) ->
	wql_pre_scan_filters:filter(B).

post_scan(Tokens) ->
	wql_post_scan_filters:filter(Tokens).

%%############################################################
%% Lexer Stuff 
%%###########################################################

scan(<<>>, TokAcc) ->
	lists:reverse([?END_TOKEN | TokAcc]);

%% Subquery start with special OR definition syntax: ...(| ...
scan(<<"(| ", Rest/binary>>, TokAcc) ->
	scan(Rest, [sym('|') | [sym('(') | TokAcc]]);

%% Regular subquery start 
scan(<<"(", Rest/binary>>, TokAcc) ->
	scan(Rest, [sym('(') | TokAcc]);

%% Subquery end
scan(<<")", Rest/binary>>, TokAcc) ->
	scan(Rest, [sym(')') | TokAcc]);

%% Subquery start with special OR definition syntax: ...(| ...



scan(<<O, R, Z, Rest/binary>>, TokAcc) when 
			(O =:= $O) ,
			(R =:= $R) ,
			(Z =:= $\s) or (Z =:= $() or (Z =:= $)) ->
	Acc = add_token({'OR', [O, R]}, TokAcc),
	scan(<<Z, Rest/binary>>, Acc);

scan(<<O, R>>, TokAcc) when 
			(O =:= $O) ,
			(R =:= $R) ->
	Acc = add_token({'OR', [O, R]}, TokAcc),
	scan(<<>>, Acc);


%% add support for minus operator
scan(<<O, Z, Rest/binary>>, TokAcc) when 
                        (O =:= $-) ->
	Acc = add_token({'NOT', [O]}, TokAcc),
	scan(<<Z, Rest/binary>>, Acc);

scan(<<O>>, TokAcc) when 
			(O =:= $-) ->
	Acc = add_token({'NOT', [O]}, TokAcc),
	scan(<<>>, Acc);

%% Single and double quoted strings are keywords. Note, we're *not*
%% going to require strings to terminate with something special
%% such as whitespace.
scan(<<QT, Rest/binary>>, TokAcc) when (QT =:= $') or (QT =:= $") ->
	{Rest1, Str} = wql_scan_util:until(QT, Rest, <<>>), 
	scan(Rest1, [keyword(Str) | TokAcc]);

%% Initialy we don't know what this is - it could turn out to be 
%% a bare keyword or a field.
scan(<<C, Rest/binary>>, TokAcc) when 
					(C >= $A) and (C =< $Z);
					(C >= $a) and (C =< $z);
					(C >= $0) and (C =< $9)
					->
	{Rest1, Word} = to_word_end(Rest, [C]),
	case TokAcc of
		[{'FIELD', _, _} | _Tail] ->
		        case Rest1 of 
			    <<$:, Rest2/binary>> ->
				{Rest3, Word2} = to_word_end(Rest2, [$:]),
				scan(Rest3, [keyword(Word++Word2) | TokAcc]);
			    _ ->
				scan(Rest1, [keyword(Word) | TokAcc])
			end;
	       
		_ ->	
			case Rest1 of 			
				<<$:, $\s, R/binary>> -> 		% Illegal Space
					Field = field(Word),
					Keyword = keyword(""), 
					scan(R, [Keyword | [Field | TokAcc]]);
				<<$:>> ->
					Field = field(Word),
					Keyword = keyword(""),
					scan(<<>>, [Keyword | [Field | TokAcc]]);
				<<$:, R/binary>> -> 
					scan(R, [field(Word) | TokAcc]);	
				_Other -> 
					scan(Rest1, [keyword(Word) | TokAcc])
			end
	end;

%% Ignore whitespace and control characters
scan(<<I, Rest/binary>>, TokAcc) when (I >= 0) and (I =< $\s) ->
	scan(Rest, TokAcc);

%% Treat everything else as a bare keyword
scan(<<C, Rest/binary>>, TokAcc) ->
	{Rest1, Word} = to_word_end(Rest, [C]),
	scan(Rest1, [keyword(Word) | TokAcc]).	

%% This is a hack to avoid duplicating the code which checks for
%% AND, OR and NOT. Generally when such patterns occur we want 
%% to treat them as operator tokens. However, when we encounter
%% such a pattern immediately after a field (eg "region:AnD ") 
%% then we want to treat AnD as a keyword string.
%%
%% This function just checks to see if the last token that was 
%% added to the Accumulator was a 'FIELD' token. If it was 
%% then we treat the text as a keyword. Otherwise treat it 
%% as an operator Token.

add_token({AssumedType, ActualString}, TokAcc) ->
	Token = case TokAcc of
			[{'FIELD', _, _} | _Tail] ->
				keyword(string:to_lower(ActualString));
			_Other -> 
				sym(AssumedType)
		end,
	[Token | TokAcc].
	

%%##############################################################
%% Helpers - help seek the end of strings, etc. Results are
%%           passed back - no tokens are created here.
%%#############################################################

to_word_end(<<C, Rest/binary>>, Acc) when 
					(C >= $A) and (C =< $Z);
					(C >= $a) and (C =< $z);
					(C >= $0) and (C =< $9);
					(C =:= $_) or 
					(C =:= $&) or 
					(C =:= $-) or
					(C =:= $!) or
					(C =:= $#) or
					(C =:= $*) or	
					(C =:= $+) or
					(C =:= $=) or
					(C =:= $.) or
					(C =:= $/) or
					(C =:= $<) or
					(C =:= $>) or
					(C =:= $?) or
					(C =:= $@) or
					(C =:= $[) or
					(C =:= $]) or
					(C =:= $^) or
					(C =:= $~) or
					(C =:= ${) or
					(C =:= $|) or
					(C =:= $}) 
					->
	to_word_end(Rest, [C | Acc]);
to_word_end(<<C, Rest/binary>>, Acc) ->
	{<<C,Rest/binary>>, lists:reverse(Acc)};
to_word_end(<<>>, Acc) ->
	{<<>>, lists:reverse(Acc)}.
	

field(X) ->
	{'FIELD', ?TL, X}.

keyword(X) when is_binary(X) ->
	keyword(binary_to_list(X));
keyword(X) ->
	{'KEYWORD', ?TL, X}.

sym(X) ->
	{X, ?TL}.

