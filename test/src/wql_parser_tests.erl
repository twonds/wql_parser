
-module(wql_parser_tests).
-compile(export_all).

-define(LEGAL_FIELDS, [region, title]).

test() ->
	t:run(?MODULE).


test__parse__expectations() ->
		Samples = [
			% First bunch deals with the basics
			[
				"linux",
				"\"linux\"",
				"Testing single bare term"
			], [
				"\"linux\"",
			 	"\"linux\"", 
				"Testing single term in double quotes"
			], [
				"'linux'",
			 	"\"linux\"", 
				"Testing single term in single quotes"
			], [
				"linux administrator",
			 	"(\"linux\" AND \"administrator\")", 
				"Testing two bare terms"
			], [
				"AT&T",
			 	"\"AT&T\"", 
				"Testing ampersand"
			], [
				"\"linux administrator\"",
			 	"\"linux administrator\"", 
				"Testing double quoted phrase"
			], [
				"'linux administrator'",
			 	"\"linux administrator\"", 
				"Testing single quoted phrase"
			], [
				"\"linux administrator",
			 	"\"linux administrator\"", 
				"Testing double quoted phrase with missing closing quotation mark"
			], [
				"'linux administrator",
			 	"\"linux administrator\"", 
				"Testing single quoted phrase with missing closing quotation mark"
 %	# TODO		], [
 %	#			"linux administrator\"",
 %	#		 	"\"linux administrator\"", 
 %	#			"Testing double quoted phrase with missing opening quotation mark"
 %	#		], [
 %	#			"linux administrator'",
 %	#		 	"\"linux administrator\"", 
 %	#			"Testing single quoted phrase with missing opening quotation mark"
			], 
	
			% Next bunch deals with basic parenthesis grouping
			[
				"(linux)",
				"\"linux\"",
				""
			], [
				"(linux",
				"\"linux\"",
				"With missing closing parenthesis"
			], [
				"linux)",
				"\"linux\"",
				"With missing opening parenthesis"
			], [
				"((linux))",
				"\"linux\"",
				""
			], [
				"((linux",
				"\"linux\"",
				"With two missing closing parenthesis"
			], [
				"linux))",
				"\"linux\"",
				"With two missing opening parenthesis"
			], [
				"((linux windows))",
				"(\"linux\" AND \"windows\")",
				""
			], [
				"(linux windows",
				"(\"linux\" AND \"windows\")",
				"With one missing opening parenthesis"
			], [
				"linux windows)",
				"(\"linux\" AND \"windows\")",
				"With one missing opening parenthesis"
			], [
				"((linux windows",
				"(\"linux\" AND \"windows\")",
				"With two missing opening parenthesis"
			], [
				"linux windows))",
				"(\"linux\" AND \"windows\")",
				"With two missing opening parenthesis"
			], [
				"(not linux)",
				"(\"not\" AND \"linux\")",
				""
			], [
				"(not linux or windows)",
				"(((\"not\" AND \"linux\") AND \"or\") AND \"windows\")",
				""
			], [
				"((not linux or windows))",
				"(((\"not\" AND \"linux\") AND \"or\") AND \"windows\")",
				""
			], [
				"((linux) windows)",
				"(\"linux\" AND \"windows\")",
				""
			], [
				"(linux (windows))",
				"(\"linux\" AND \"windows\")",
				""
			], [
				"linux (windows ruby)",
				"(\"linux\" AND (\"windows\" AND \"ruby\"))",
				""
			], [
				"linux windows ruby 'system administrator'",
				"(((\"linux\" AND \"windows\") AND \"ruby\") AND \"system administrator\")",
				""
			], [
				"linux not windows or ruby and 'system administrator'",
				"((((((\"linux\" AND \"not\") AND \"windows\") AND \"or\") AND \"ruby\") AND \"and\") AND \"system administrator\")",
				""
			], [
				"(linux windows) (ruby 'system administrator')",
				"((\"linux\" AND \"windows\") AND (\"ruby\" AND \"system administrator\"))",
				""
			], [
				"(linux windows) NOT (ruby 'system administrator')",
				"(((\"linux\" AND \"windows\") AND \"NOT\") AND (\"ruby\" AND \"system administrator\"))",
				""
			], [
				"linux (ruby or c++ (xml xslt) windows)",
				"(\"linux\" AND ((((\"ruby\" AND \"or\") AND \"c++\") AND (\"xml\" AND \"xslt\")) AND \"windows\"))",
				""
			], [
				"linux ()",
				"\"linux\"",
				""
			], [
				"linux (NOT)",
				"(\"linux\" AND \"NOT\")",
				""
			], [
				"linux (AND)",
				"(\"linux\" AND \"AND\")",
				""
			], [
				"linux (OR)",
				"\"linux\"",
				""
			], [
				"linux (|)",
				"(\"linux\" AND \"|\")",
				""
			], [
				"linux (| )",
				"\"linux\"",
				""
			], [
				"linux (region:)",
				"\"linux\"",
				""
			], [
				"linux (ruby)",
				"(\"linux\" AND \"ruby\")",
				""
			], [
				"linux NOT (ruby)",
				"((\"linux\" AND \"NOT\") AND \"ruby\")",
				""
			], [
				"linux NOT ()",
				"(\"linux\" AND \"NOT\")",
				""
			], [
				"linux OR ()",
				"\"linux\"",
				""
			], [
				"linux AND ()",
				"(\"linux\" AND \"AND\")",
				""
			], [
				"linux NOT (NOT)",
				"((\"linux\" AND \"NOT\") AND \"NOT\")",
				""
			], [
				"linux NOT (AND)",
				"((\"linux\" AND \"NOT\") AND \"AND\")",
				""
			], [
				"linux NOT (OR)",
				"(\"linux\" AND \"NOT\")",
				""
			], [
				"linux NOT (|)",
			    "((\"linux\" AND \"NOT\") AND \"|\")",
				""
			], [
				"linux AND (NOT)",
				"((\"linux\" AND \"AND\") AND \"NOT\")",
				""
			], [
				"linux AND (AND)",
				"((\"linux\" AND \"AND\") AND \"AND\")",
				""
			], [
				"linux AND (OR)",
				"(\"linux\" AND \"AND\")",
				""
			], [
				"linux AND (|)",
				"((\"linux\" AND \"AND\") AND \"|\")",
				""
			], [
				"linux OR (NOT)",
				"(\"linux\" OR \"NOT\")",
				""
			], [
				"linux OR (AND)",
				"(\"linux\" OR \"AND\")",
				""
			], [
				"linux OR (OR)",
				"\"linux\"",
				""
			], [
				"linux OR (|)",
				"(\"linux\" OR \"|\")",
				""
			], [
				"linux OR (| )",
				"\"linux\"",
				""
			], [
				"linux OR (NOT NOT)",
				"(\"linux\" OR (\"NOT\" AND \"NOT\"))",
				""
			], [
				"linux OR (AND AND)",
				"(\"linux\" OR (\"AND\" AND \"AND\"))",
				""
			], [
				"linux OR (OR OR)",
				"\"linux\"",
				""
			], [
				"linux (yabba:)",
				"\"linux\"",
				""
			], [
				"linux (yabba:whatever)",
				"(\"linux\" AND \"yabba:whatever\")",
				""
			], 
	
			% Next bunch deals with fields
			[
				"title:administrator",
				"title:\"administrator\"",
				"Testing valid field with bare keyword - keyword part of result should be quoted"
	 		], [
	 			"invalid:administrator",
	 			"\"invalid:administrator\"",
	 			"Testing invalid field with bare keyword - result should be completely quoted"
	 		], [
	 			"title:\"system administrator\"",
	 			"title:\"system administrator\"",
	 			"Testing valid field with double quoted keyword"
	 		], [
	 			"title:'system administrator'",
	 			"title:\"system administrator\"",
	 			"Testing valid field with single quoted keyword"
	 		], [
	 			"invalid:\"system administrator\"",
	 			"\"invalid:system administrator\"",
	 			"Testing invalid field with double quoted keyword"
	 		], [
	 			"invalid:'system administrator'",
	 			"\"invalid:system administrator\"",
	 			"Testing invalid field with single quoted keyword"
	 		], [
	 			"title: administrator",
	 			"(title:\"\" AND \"administrator\")",
	 			"Testing valid field with illegal whitespace"
	 		], [
	 			"title: 'system administrator'",
	 			"(title:\"\" AND \"system administrator\")",
	 			"Testing valid field with illegal whitespace, followed by quoted word"
	 		], [
	 			"title:http://administrator",
	 			"title:\"http://administrator\"",
	 			"Testing http "

	 		], [
	 			"invalid: administrator",
	 			"(\"invalid:\" AND \"administrator\")",
	 			"Testing invalid field with illegal whitespace"
	 		], [
	 			"invalid: \"system administrator\"",
	 			"(\"invalid:\" AND \"system administrator\")",
	 			"Testing invalid field with illegal whitespace, followed by quoted word"
	 		], 
			   	
			% Next bunch deals with subqueried fields
			[
				"region:(dublin)",
				"region:\"dublin\"",
				""
	 		], [
				"region:((dublin))",
				"region:\"dublin\"",
				""
	 		], [
				"region:(dublin or galway)",
				"region:((\"dublin\" AND \"or\") AND \"galway\")",
				""
	 		], [
				"region:(((dublin or galway)))",
				"region:((\"dublin\" AND \"or\") AND \"galway\")",
				""
	 		], [
				"region:(((dublin) or ((galway))))",
				"region:((\"dublin\" AND \"or\") AND \"galway\")",
				""
	 		], [
				"region:(not dublin)",
				"region:(\"not\" AND \"dublin\")",
				""
	 		], [
				"region:(not dublin or not galway)",
				"region:((((\"not\" AND \"dublin\") AND \"or\") AND \"not\") AND \"galway\")",
				""
	 		], [
				"region:(not (dublin or galway))",
				"region:(\"not\" AND ((\"dublin\" AND \"or\") AND \"galway\"))",
				""
	 		], [
				"region:(not (dublin or (galway mayo)))",
				"region:(\"not\" AND ((\"dublin\" AND \"or\") AND (\"galway\" AND \"mayo\")))",
				""
	 		], [
				"region:((galway mayo) not dublin)",
				"region:(((\"galway\" AND \"mayo\") AND \"not\") AND \"dublin\")",
				""
	 		], [
				"region:'not'",
				"region:\"not\"",
				""
	 		], [
				"region:and dublin",
				"(region:\"and\" AND \"dublin\")",
				""
	 		], [
				"region:AND dublin",
				"(region:\"AND\" AND \"dublin\")",
				""
	 		], [
				"region:or dublin",
				"(region:\"or\" AND \"dublin\")",
				""
	 		], [
				"region:OR dublin",
				"(region:\"or\" AND \"dublin\")",
				""
	 		], [
				"region:not dublin",
				"(region:\"not\" AND \"dublin\")",
				""
	 		], [
				"region:NOT dublin",
				"(region:\"NOT\" AND \"dublin\")",
				""
	 		], [
				"region:NOT NOT dublin",
				"((region:\"NOT\" AND \"NOT\") AND \"dublin\")",
				""
	 		], [
				"region:(region:dublin)",
				"region:\"dublin\"",
				""
	 		], [
				"region:(title:administrator)",
				"region:\"administrator\"",
				""
	 		], [
				"region:(title:administrator or title:developer)",
				"region:((\"administrator\" AND \"or\") AND \"developer\")",
				""
	 		], [
			    "region:(title:administrator OR title:developer)",
			    "region:(\"administrator\" OR \"developer\")",
			    ""
			   ], [
				"linux AND title:(administrator or developer not security) region:(\"dublin\" or kerry)",
				"(((\"linux\" AND \"AND\") AND title:((((\"administrator\" AND \"or\") AND \"developer\") AND \"not\") AND \"security\")) AND region:((\"dublin\" AND \"or\") AND \"kerry\"))",
				""
	 		], [
				"illegal:(administrator or developer)",
				"\"illegal:([unrecognized field tag - inner content removed.)\"",
				""
	 		], 
	 
	 		% Next bunch deals with standalone AND
	 		[
	 			"linux AND windows",
	 			"((\"linux\" AND \"AND\") AND \"windows\")",
	 			"Testing single AND"
	 		], [
	 			"linux And and AND windows",
	 		 	"((((\"linux\" AND \"And\") AND \"and\") AND \"AND\") AND \"windows\")", 
	 			"Testing multiple adjacent ANDs"
	 		], 
	 
	 		% Next bunch deals with AND and OR combinations
	 		[
	 			"linux OR AND windows",
	 		 	"((\"linux\" OR \"AND\") AND \"windows\")", 
	 			"OR followed by AND should reduce to OR "
	 		], [
	 			"linux AND OR windows",
	 		 	"((\"linux\" AND \"AND\") OR \"windows\")", 
	 			"Just OR, AND are terms"
	 		], [
	 			"linux and AND or or and OR and and or and AND windows",
	 		 	"(((((((((((\"linux\" AND \"and\") AND \"AND\") AND \"or\") AND \"or\") AND \"and\") OR \"and\") AND \"and\") AND \"or\") AND \"and\") AND \"AND\") AND \"windows\")", 
	 			"More scattered AND/OR strings. Should always reduce to the last in the sequence"
	 		], [
	 			"linux OR or AND or or and OR and and or and or OR windows",
	 		 	"(((((((((((\"linux\" OR \"or\") AND \"AND\") AND \"or\") AND \"or\") AND \"and\") OR \"and\") AND \"and\") AND \"or\") AND \"and\") AND \"or\") OR \"windows\")", 
	 			"Even more scattered AND/OR strings. Should always reduce to the last in the sequence"
	 		],
	 
	 		% Next bunch deals with standalone NOT 
	 		[
	 			"NOT windows",
	 		 	"(\"NOT\" AND \"windows\")", 
	 			"Test 1 for single NOT, NOT is a term"
	 		], [
	 			"linux NOT windows",
	 		 	"((\"linux\" AND \"NOT\") AND \"windows\")", 
	 			"Test 2 for single NOT"
	 		], [
	 			"linux not NOT Not windows", 
	 			"((((\"linux\" AND \"not\") AND \"NOT\") AND \"Not\") AND \"windows\")",
	 			"Testing multiple adjacent NOTs"
	 		], 
	 
	 		%  Next bunch deal with different combinations of NOT and OR
	 		[
	 			"linux NOT OR windows",
	 		 	"((\"linux\" AND \"NOT\") OR \"windows\")", 
	 			"NOT OR should become OR - the NOT should be ignored"
	 		], [
	 			"linux OR NOT windows",
	 		 	"((\"linux\" OR \"NOT\") AND \"windows\")", 
	 			"OR NOT should remain unchanged"
	 		], [
	 			"linux NOT NOT OR windows",
	 		 	"(((\"linux\" AND \"NOT\") AND \"NOT\") OR \"windows\")", 
	 			"NOT NOT OR should become OR - the NOTs should be ignored"
	 		], [
	 			"linux NOT OR NOT windows",
	 		 	"(((\"linux\" AND \"NOT\") OR \"NOT\") AND \"windows\")", 
	 			"NOT OR NOT should become OR NOT - NOTs are terms"
	 		], [
	 			"linux NOT OR OR windows",
	 		 	"((\"linux\" AND \"NOT\") OR \"windows\")", 
	 			"NOT OR OR should become OR - the NOT should be ignored"
	 		], [
	 			"linux OR NOT NOT windows",
	 		 	"(((\"linux\" OR \"NOT\") AND \"NOT\") AND \"windows\")", 
	 			"OR NOT NOT should become OR NOT - the NOTs should be reduced"
	 		], [
	 			"linux OR NOT OR NOT windows",
	 		 	"(((\"linux\" OR \"NOT\") OR \"NOT\") AND \"windows\")", 
	 			""
	 		], [
	 			"linux NOT OR NOT OR windows",
	 		 	"(((\"linux\" AND \"NOT\") OR \"NOT\") OR \"windows\")", 
	 			"Error "
	 		], [
	 			"linux OR NOT NOT OR NOT windows",
	 		 	"((((\"linux\" OR \"NOT\") AND \"NOT\") OR \"NOT\") AND \"windows\")", 
	 			"OR NOT NOT OR NOT should become OR NOT"
	 		], [
	 			"linux NOT OR OR NOT OR windows",
	 		 	"(((\"linux\" AND \"NOT\") OR \"NOT\") OR \"windows\")", 
	 			"NOT OR OR NOT OR should become OR - the NOTs should be ignored"
	 		],
			
	 		%  Next bunch deal with different combinations of NOT and AND
	 		[
	 			"linux NOT AND windows",
	 		 	"(((\"linux\" AND \"NOT\") AND \"AND\") AND \"windows\")", 
	 			"NOT AND should become AND - the NOT should be ignored"
	 		], [
	 			"linux AND NOT windows",
	 		 	"(((\"linux\" AND \"AND\") AND \"NOT\") AND \"windows\")", 
	 			""
	 		], [
	 			"linux NOT NOT AND windows",
	 		 	"((((\"linux\" AND \"NOT\") AND \"NOT\") AND \"AND\") AND \"windows\")", 
	 			"NOT NOT AND should become AND - the NOTs are terms"
	 		], [
	 			"linux NOT AND NOT windows",
	 		 	"((((\"linux\" AND \"NOT\") AND \"AND\") AND \"NOT\") AND \"windows\")", 
	 			""
	 		], [
	 			"linux NOT AND AND windows",
	 		 	"((((\"linux\" AND \"NOT\") AND \"AND\") AND \"AND\") AND \"windows\")", 
	 			"NOT AND AND should become AND - the NOTs are terms"
	 		],
			
	 		% Next bunch deals with subexpressions and field subexpressions with default or "|"
	 		[
	 			"linux (| ruby c++)",
	 		 	"(\"linux\" AND (\"ruby\" OR \"c++\"))", 
	 			""
	 		], [
	 			"linux (| ruby c++ lisp)",
	 		 	"(\"linux\" AND ((\"ruby\" OR \"c++\") OR \"lisp\"))", 
	 			""
	 		], [
	 			"linux (| ruby AND c++)",
	 		 	"(\"linux\" AND ((\"ruby\" OR \"AND\") OR \"c++\"))", 
	 			""
	 		], [
	 			"linux (| ruby AND c++ lisp)",
	 		 	"(\"linux\" AND (((\"ruby\" OR \"AND\") OR \"c++\") OR \"lisp\"))", 
	 			""
	 		], [
	 			"linux (| AND ruby c++)",
	 		 	"(\"linux\" AND ((\"AND\" OR \"ruby\") OR \"c++\"))", 
	 			""
	 		], [
	 			"linux (| OR ruby c++)",
	 		 	"(\"linux\" AND (\"ruby\" OR \"c++\"))", 
	 			""
	 		], [
	 			"linux (| NOT ruby NOT c++)",
	 		 	"(\"linux\" AND (((\"NOT\" OR \"ruby\") OR \"NOT\") OR \"c++\"))", 
	 			""
			    ],
	 		%  Next tests how the '|' symbol is handled in various situations
	 		[
	 			"|",
	 		 	"\"|\"", 
	 			""
	 		], [
	 			"||",
	 		 	"\"||\"", 
	 			""
	 		], [
	 			"| |",
	 		 	"(\"|\" AND \"|\")", 
	 			""
	 		], [
	 			"| | |",
	 		 	"((\"|\" AND \"|\") AND \"|\")", 
	 			""
	 		], [
	 			"(|)",
	 		 	"\"|\"", 
	 			""
	 		], [
	 			"(||)",
	 		 	"\"||\"", 
	 			""
	 		], [
	 			"(| |)",
	 		 	"\"|\"", 
	 			""
	 		], [
	 			"(| | |)",
	 		 	"(\"|\" OR \"|\")", 
	 			""
	 		], [
	 			"(|(|))",
	 		 	"(\"|\" AND \"|\")", 
	 			""
	 		], [
	 			"(| (| ))",
	 		 	"", 
	 			""
	 		], [
	 			"(| (|))",
	 		 	"\"|\"", 
	 			""
	 		], [
	 			"region:(|(|))",
	 		 	"region:(\"|\" AND \"|\")", 
	 			""
	 		], [
	 			"region:(| (|))",
	 		 	"region:\"|\"", 
	 			""
	 		], [
	 			"linux region:(|(|))",
	 		 	"(\"linux\" AND region:(\"|\" AND \"|\"))", 
	 			""
	 		], [
	 			"linux region:(| (|))",
	 		 	"(\"linux\" AND region:\"|\")", 
	 			""
	 		], [
	 			"linux region:(| (| ))",
	 		 	"\"linux\"", 
	 			""
	 		],
	
	 		%  The following are taken from the "Advanced Search" on the real system
	 		[
	 			"( ) NOT (| NOT (| )) title:( ) NOT title:(| ) AND ( )",
	 		 	"(((\"NOT\" AND \"NOT\") AND \"NOT\") AND \"AND\")", 
	 			""
	 		], [
	 			"( linux ) NOT (  NOT (|  )) title:(  ) NOT title:(  ) AND ( )",
	 		 	"((((\"linux\" AND \"NOT\") AND \"NOT\") AND \"NOT\") AND \"AND\")", 
	 			""

	 		], [
	 			"template",
	 		 	"\"template\"", 
	 			""
	 		]
 		],
	lists:foreach(
		fun([Query, Expected, Reason]) ->
			{ok, Ast1} = wql_parser:parse(Query, ?LEGAL_FIELDS),
			S = binary_to_list(wql_parser:to_string(Ast1)),
			t:equal(Expected, S, Reason),

			%% remove test for back to query to_string is to go to solr now.
			ok
		end, Samples).

test__no_explode() ->
	Samples = [
		"",
		"  ",
		":",
		"::",
		"''",
		"\"\"",
		"' '",
		"()",
		"() ()",
		"(())",
		"|",
		"||",
		"(|)",
		"(||)",
		"(| |)",
		"(| | |)",
		"( | | )",
		"( | | | )",
		"(|||||)",
		"linux ()",
		"linux () ()",
		"linux (())",
		"NOT",
		"AND",
		"OR",
		"OR OR",
		"OR OR OR",
		"OR OR OR",
		"(AND)",
		"(AND AND)",
		"(AND) (AND)",
		"(AND AND) AND",
		"AND (AND) AND",
		"AND (AND AND)",
		"AND linux",
		"linux AND",
		"(AND linux)",
		"(AND( linux",
		"(AND) linux",
		"linux OR",
		"linux (OR)",
		"linux (OR OR)",
		"linux (OR OR OR)",
		"OR linux",
		"(OR) linux",
		"(OR OR) linux",
		"(OR OR OR) linux",
		"(tree) linux",
		"(OR tree) linux",
		"(tree OR) linux",
		"linux ('tree' OR)",
		"linux (OR 'tree' OR)",
		"(linux (OR 'tree' OR))",
		"linux NOT",
		"NOT linux",
		"(NOT) linux",
		"(NOT NOT) linux",
		"(NOT NOT NOT) linux",
		"(linux NOT) linux",
		"linux (NOT)",
		"region:(dublin)",
		"region:(region:dublin)",
		":region",
		":region:",
		"::region",
		"::region:",
		"::region::()",
		"::region::(wow)",
		"region:(region::dublin)",
		"region:(region:dublin:)",
		"region:not",
		"region:()",
		"region::",
		"region:::",
		"region::wherever",
		"region:::wherever",
		"region:: wherever",
		"linux region:",
		"linux region: ",
		"linux region:()",
		"linux region:() ",
		"linux region:dublin",
		"linux region:dublin ",
		"linux region:(and)",
		"linux region:(or)",
		"linux region:(not)",
		"linux region:(|)",
		"linux region:(||)",
		"linux region:(|||||)",
		"linux region:(| |)",
		"linux region:(| | | |)"
	],
	Tests = lists:map(fun(X) ->
				{fun() -> 
					{ok, _Ast} = wql_parser:parse(X)
				end, io_lib:format("[~p]", [X])}
			  end, Samples),
	lists:foreach(fun t:no_die/1, Tests).

