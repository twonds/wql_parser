
-module(wql_scanner_tests).
-compile(export_all).

-include("../../include/wql_scanner.hrl").

% recc parser requires a line number which is irrlevant to our app.
-define(TL, 1).

test() ->
	t:run(?MODULE).

%%-------------------------------------------------------------
%% The following batch check the fix_missing_quotes function
%% ------------------------------------------------------------

test__quotes__without_quotes() ->
	B = <<"no quotes">>,
	t:equal(B, wql_pre_scan_filters:fix_missing_quotes(B, <<>>)).

test__quotes__proper_doubles() ->
	NoDifference = <<>>,
	check_quotes(proper_doubles(), NoDifference). 

test__quotes__proper_singles() ->
	NoDifference = <<>>,
	check_quotes(proper_singles(), NoDifference).

test__quotes__with_missing_doubles() ->
	ExtraDoubleQuoteExpected = <<$">>,
	check_quotes(missing_doubles(), ExtraDoubleQuoteExpected).

test__quotes__with_missing_singles() ->
	ExtraSingleQuoteExpected = <<$'>>,
	check_quotes(missing_singles(), ExtraSingleQuoteExpected).
	
check_quotes([S | Rest], Extra) ->
	B = list_to_binary(S),
	t:equal(<<B/binary, Extra/binary>>, 
			wql_pre_scan_filters:fix_missing_quotes(B, <<>>)),
	check_quotes(Rest, Extra);
check_quotes([], _Extra) ->
	done.

proper_doubles() ->
	["with \"proper doubles\" in middle",
	 "with proper doubles \"at end\"",
	 "\"with\" proper doubles at begining",
	 "\"with double\" \"proper doubles\"",
	 "with \"proper doubles 'and' nested 'singles'\"",
	 "with \"proper doubles 'and' some\" 'unnested singles'",
	 "with \"proper doubles 'and missing nested\" singles"
	 ].

proper_singles() ->
	[switch_double_single(S) || S <- proper_doubles()].

missing_doubles() ->
	["with missing \"doubles in middle",
	 "\"with missing doubles at the start",
	 "with missing doubles at the end\"",
	 "with bunched missing \"\"\"doubles in middle",
	 "\"\"\"with bunched missing doubles at the start",
	 "with bunched missing doubles at the end\"\"\"",
	 "with \"missing 'doubles' and nested singles \"in \"middle",
	 "with \"missing 'doubles and nested missing singles\" in \"middle",
	 "with \"missing doubles 'before missing singles" ,
	 "with \"proper\" then \"bad doubles 'before missing singles" ,
	 "with interleved \"missing 'doubles \"before \"missing singles" 
	].

missing_singles() ->
	[switch_double_single(S) || S <- missing_doubles()].

switch_double_single(S) ->
	lists:map(fun($') -> $";
		     ($") -> $';
		     (X) -> X end, S).

%%-------------------------------------------------------------
%% The following batch deal with the fix_missing_parens function
%% ------------------------------------------------------------

test__parens__without_parens() ->
	B = <<"hello world">>,
	t:equal(B, wql_pre_scan_filters:fix_missing_parens(B, <<>>)).

test__parens__with_balanced_parens() ->
	Samples = [
		"With (one level) of parens",
		"(With one level) of parens starting at beginning",
		"With one level of (parens ending at end)",
		"(With one level of parens starting at beginning and ending at end)",
		"With ((two levels) of) parens",
		"With ((two levels) of) parens",
		"(With (two levels) of parens covering start and end)",
		"With (two levels) of (unnested) parens",
		"(With two levels) of parens (covering start and end, unnested.)",
		"With open parens (within ' a (single ' quote ) string",
		"With open parens (within \" a (double \" quote ) string",
		"With close parens (within ' a )single ' quote ) string",
		"With close parens (within \" a )double \" quote ) string",
		"With two open parens (within ' a (single( ' quote ) string" ,
		"With two open parens (within \" a (double( \" quote ) string",
		"With many open parens (within \"( both '( double ' ( and single '(\" quotes), wow.",
		"With 'many (open' parens inside (various) 'both \"open\" and (\"closed' strings.",
		"With 'many )close' parens inside (various) 'both \")open\") and \"closed' strings.",
		"With ('many (op(en) and) closed)' parens )(inside (various) '(both \")op(en\") and \"closed') strings."
		],
	lists:foreach(
		fun(S) ->
			B = list_to_binary(S),
			t:equal(B, wql_pre_scan_filters:fix_missing_parens(B, <<>>))
		end, Samples).

test__parens__with_missing_closed_parens() ->
	Samples = [
		{1, "Missing (one closing parens"},
		{2, "(Missing (two closing parens"},
		{2, "((Missing two closing parens"},
		{2, "Missing two closing parens(("},
		{1, "Missing one (closing parens with 'two ( more ( in single string to ignore', huh."},
		{2, "Missing two (closing parens with \"two ( more ( in double string to ignore\", (huh, huh."},
		{5, "M(issing (about five( or so (parens ( closing parens"},
		{1, "Mixing( opening and ( closing) parens) (to make ( one missing) parens result"},
		{4, "(Mixing( opening and ( closing) (parens) (to make ((((four missing) parens ))result"},
		{1, "Mixing( opening and '( closing) (parens) and strings (to make' \"((((one missing) parens ))\"result"}
		],
	lists:foreach(
		fun({N, S}) ->
			B = list_to_binary(S),
			Extra = list_to_binary(string:chars($), N)),
			t:equal(<<B/binary, Extra/binary>>, 
					wql_pre_scan_filters:fix_missing_parens(B, <<>>))
		end, Samples).

test__parens__with_missing_opening_parens() ->
	Samples = [
		{1, "Missing )one opening parens"},
		{2, "Missing )two opening parens)"},
		{1, "One) (match ) one miss"},
		{1, "'String numbed and one (miss')"},
		{1, ")\"String numbed and one )miss\""},
		{5, "Mis)sing) about) )fiv(e) )yokimagijs"},
		{5, ")))Mis)sing) fiv(e) from begininning"}
		],
	lists:foreach(
		fun({N, S}) ->
			B = list_to_binary(S),
			Extra = list_to_binary(string:chars($(, N)),
			t:equal(<<Extra/binary, B/binary>>, 
					wql_pre_scan_filters:fix_missing_parens(B, <<>>))
		end, Samples).

test__parens__with_missing_both_parens() ->
	Samples = [
		{1, 1, ")Missing one of each("},
		{2, 2, "Missing) two) (of (each"},
		{1, 1, "Missing) (one of) (each again"},
		{1, 1, "Missing) ')one of(' (each, despite strings"}
		],
	lists:foreach(
		fun({N1, N2, S}) ->
			B = list_to_binary(S),
			Extra1 = list_to_binary(string:chars($(, N1)),
			Extra2 = list_to_binary(string:chars($), N2)),
			t:equal(<<Extra1/binary, B/binary, Extra2/binary>>, 
					wql_pre_scan_filters:fix_missing_parens(B, <<>>))
		end, Samples).
	
%%----------------
%% Core Tests
%%----------------

test__scan__single_bare_string() ->
	S = "  hello   ",
	Expected = {'KEYWORD', ?TL, "hello"},
	t:equal(all_expected(Expected), wql_scanner:scan(S), S).

test__scan__ampersand() ->
	S = "AT&T",
	Expected = {'KEYWORD', ?TL, "AT&T"},
	t:equal(all_expected(Expected), wql_scanner:scan(S), S).
% %% NOT's get removed when at end of statement.
% %% TODO - test NOTs properly
% test__scan__not() ->
% 	S = "  NOT   ",
% 	Expected = {'NOT', ?TL},
% 	t:equal(all_expected(Expected), wql_scanner:scan(S), S).

test__scan__and() ->
	S = "  AND   ",
	Expected = {'KEYWORD', ?TL, "AND"},
	t:equal(all_expected(Expected), wql_scanner:scan(S), S).

test__scan__or() ->
	S = "  OR   ",
	Expected = {'OR', ?TL},
	t:equal(all_expected(Expected), wql_scanner:scan(S), S).

test__scan__multiple_bare_strings() ->
	S = "   hello   world   yay  with    white  space   ",
	Expected = lists:map(
			fun(X) -> {'KEYWORD', ?TL, X} end,
			string:tokens(S, " ")),
	t:equal(all_expected(Expected), wql_scanner:scan(S), S).

test__scan__quoted_string() ->
	Samples = [
		"'single quoted'", 
		"\"double quoted\"",
		"'multi quoted \"starting with\" single quotes'",
		"\"multi quoted 'starting with' double quotes\""
	],
	lists:foreach(
		fun(S) ->
			Expected = {'KEYWORD', ?TL, 
					string:substr(S, 2, (length(S) - 2))},
			t:equal(all_expected(Expected), wql_scanner:scan(S), S)
		end, Samples).

test__scan__single_field() ->
	Samples = [
		{"region:dublin", "dublin"},
		{"region:'single quotes'", "single quotes"},
		{"region:\"double quotes\"", "double quotes"},
		{"  region:\"with surrounding whitespace\"  ", "with surrounding whitespace"},
		{"region:  ", ""},   
					%% Note how the following keywords are downcase'd
		{"region:and", "and"},
		{"region:AND", "AND"},
		{"region:or", "or"},
		{"region:OR", "or"},
		{"region:not", "not"},
		{"region:NOT", "NOT"},

		{"region:", ""}		% should have empty keyword
	],
	lists:foreach(
		fun({S, K}) ->
			Expected = [{'FIELD', ?TL, "region"}, {'KEYWORD', ?TL, K}],
			t:equal(all_expected(Expected), wql_scanner:scan(S), S)
		end, Samples).

test__scan__field_with_subquery() ->
	Samples = [
		{"region:(dublin)", [
				{'FIELD', ?TL, "region"},
				{'(', ?TL},
				{'KEYWORD', ?TL, "dublin"},
				{')', ?TL}]},
		{"region:(\"dublin centre\" OR \"dublin south\")", [
				{'FIELD', ?TL, "region"},	
				{'(', ?TL},
				{'KEYWORD', ?TL, "dublin centre"},
				{'OR', ?TL},
				{'KEYWORD', ?TL, "dublin south"},
				{')', ?TL}]},
		{"region:(not (mayo \"galway city\")", [	% note missing closing paren
				{'FIELD', ?TL, "region"},	
				{'(', ?TL},
				{'KEYWORD', ?TL, "not"},
				{'(', ?TL},
				{'KEYWORD', ?TL, "mayo"},
				{'KEYWORD', ?TL, "galway city"},
				{')', ?TL},
				{')', ?TL}]},
		{"region: (\"space invaders\")", [		% note illegal space
				{'FIELD', ?TL, "region"},	
				{'KEYWORD', ?TL, ""},
				{'(', ?TL},
				{'KEYWORD', ?TL, "space invaders"},
				{')', ?TL}]},
		{"region:(| dublin)", [				% note subquery negation with "|"
				{'FIELD', ?TL, "region"},	
				{'(', ?TL},
				{'|', ?TL},
				{'KEYWORD', ?TL, "dublin"},
				{')', ?TL}]},
		{"region:(| |)", [		
				{'FIELD', ?TL, "region"},	
				{'(', ?TL},
				{'|', ?TL},
				{'KEYWORD', ?TL, "|"},
				{')', ?TL}]},
		{"region:(||)", [		
				{'FIELD', ?TL, "region"},	
				{'(', ?TL},
				{'KEYWORD', ?TL, "||"},
				{')', ?TL}]},
		{"region:(|)", [		
				{'FIELD', ?TL, "region"},	
				{'(', ?TL},
				{'KEYWORD', ?TL, "|"},
				{')', ?TL}]},
		{"region:(| )", [	
				{'FIELD', ?TL, "region"},	
				{'(', ?TL},
				{'|', ?TL},
				{')', ?TL}]}
	],
	lists:foreach(
		fun({Query, Expected}) ->
			t:equal(all_expected(Expected), wql_scanner:scan(Query), Query)
		end, Samples).

test__scan__open_parens() ->
	Test1 = fun() ->
			Samples = [
				" ying ( yang ",
				" ying (yang ",
				" 'ying' ('yang' ",
				" ying(yang ",
				"'ying'('yang'",
				" 'ying'(yang ",
				" 'ying' ( \"yang\" "
			],
			Expected = [
				{'KEYWORD', ?TL, "ying"},
				{'(', ?TL},
				{'KEYWORD', ?TL, "yang"},
				{')', ?TL}
			],
			run_tests(Samples, Expected)
		end,
	Test2 = fun(Op) ->
			Samples = [
				io_lib:format("'~s' ( yang ", [Op]),
				io_lib:format("\"~s\" ( yang ", [Op]),
				io_lib:format("'~s'( yang ", [Op]),
				io_lib:format("\"~s\"( yang ", [Op])
			],
			Expected = [
				{'KEYWORD', ?TL, Op},
				{'(', ?TL},
				{'KEYWORD', ?TL, "yang"},
				{')', ?TL}
			],
			run_tests(Samples, Expected)
		 end,
	Test3 = fun(Op) ->
			Samples = [
				io_lib:format(" ~s ( yang ", [Op]),
				io_lib:format(" ~s( yang ", [Op])
			],
			Expected = [
				{list_to_atom(Op), ?TL},
				{'(', ?TL},
				{'KEYWORD', ?TL, "yang"},
				{')', ?TL}
			],
			run_tests(Samples, Expected)
		 end,
	Operators = ["OR"],
	Test1(),
	lists:foreach(Test2, Operators),
	lists:foreach(Test3, Operators).

test__scan__close_parens() ->
	Test1 = fun() ->
			Samples = [
				" ying ) yang ",
				" ying) yang ",
				" ying )yang ",
				" ying)yang ",
				" 'ying' ) 'yang' ",
				" 'ying') 'yang' ",
				" 'ying' )'yang' ",
				"'ying')'yang'",
				" 'ying')yang ",
				" ying)'yang' ",
				" 'ying' ) \"yang\" "
			],
			Expected = [
				{'(', ?TL},
				{'KEYWORD', ?TL, "ying"},
				{')', ?TL},
				{'KEYWORD', ?TL, "yang"}
			],
			run_tests(Samples, Expected)
		end,
	Test2 = fun(Op) ->
			Samples = [
				io_lib:format("yang ) '~s' " , [Op]),
				io_lib:format("yang ) \"~s\" ", [Op]),
				io_lib:format("yang )'~s' ", [Op]),
				io_lib:format("yang )\"~s\" ", [Op])
			],
			Expected = [
				{'(', ?TL},
				{'KEYWORD', ?TL, "yang"},
				{')', ?TL},
				{'KEYWORD', ?TL, Op}
			],
			run_tests(Samples, Expected)
		 end,
	Test3 = fun(Op) ->
			Samples = [
				   io_lib:format("yang ) ~s", [Op]),
				   io_lib:format("'yang')~s ", [Op])
			],
			Expected = [
				{'(', ?TL},
				{'KEYWORD', ?TL, "yang"},
				{')', ?TL},
				{list_to_atom(Op), ?TL}
			],
			run_tests(Samples, Expected)
		 end,
	Test1(),
	lists:foreach(Test2, ["OR"]),
	lists:foreach(Test3, ["OR"]).

test__scan__other_stuff() ->
	Samples = [
		{"(AND AND) AND", [
				{'(', ?TL},
				{'KEYWORD', ?TL, "AND"},
				{'KEYWORD', ?TL, "AND"},
				{')', ?TL},
				{'KEYWORD', ?TL, "AND"}]},
		{"(ANDZ ANDZ) ANDZ", [
				{'(', ?TL},
				{'KEYWORD', ?TL, "ANDZ"},
				{'KEYWORD', ?TL, "ANDZ"},
				{')', ?TL},
				{'KEYWORD', ?TL, "ANDZ"}]},
		{"(LAND LAND) LAND", [
				{'(', ?TL},
				{'KEYWORD', ?TL, "LAND"},
				{'KEYWORD', ?TL, "LAND"},
				{')', ?TL},
				{'KEYWORD', ?TL, "LAND"}]}
	],
	lists:foreach(
		fun({Query, Expected}) ->
			t:equal(all_expected(Expected), wql_scanner:scan(Query), Query)
		end, Samples).


all_expected(T) when is_tuple(T) ->
	{ok, [T, ?END_TOKEN]};
all_expected(L) when is_list(L) ->
	{ok, L ++ [?END_TOKEN]}.

run_tests(Samples, Expected) ->
	F = fun(S) -> t:equal(all_expected(Expected), wql_scanner:scan(S), S) end,
	lists:foreach(F, Samples).

