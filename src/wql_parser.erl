
%% @doc
%% A safe, soft-landing parser for a website query language called WQL 
%% based directly on the
%% <a target="_top"
%%    href="http://lucene.apache.org/java/2_0_0/queryparsersyntax.html">
%%        Lucene Query Parser Syntax</a>.
%% This parser was designed to be used in conjunction with 
%% <a target="_top"
%%    href="http://lucene.apache.org/java/docs/index.html">
%%         Apache Lucene
%% </a>, or 
%% <a target="_top"
%%    href="http://lucene.apache.org/solr/">Solr</a>
%% to provide a rich query language for websites.
%%
%% To get started with wql_parser, you should read the main 
%% documentation on the 
%% {@link 'overview-summary'. overview} page.
%% @end

-module(wql_parser).
-export([parse/1, 
	 parse/2, 
	 to_string/1, 
	 selected_fields/1]).

%% @type ast() = tuple().
%%       A basic abstract syntax tree representation. Some examples:
%% <pre>
%%     Query: linux
%%     
%%         "linux"
%%     
%%     Query: linux or unix
%%     
%%         {'OR',"linux","unix"}
%%     
%%     Query: linux or unix or freebsd
%%     
%%         {'OR',
%%             {'OR',"linux","unix"},
%%             "freebsd"}
%%     
%%     Query: linux or unix and not aix
%%     
%%         {'AND',
%%             {'OR',"linux","unix"},
%%             {'NOT',"unix"}}
%%     
%%     Query: title:linux and "netword administration"
%%     
%%         {'AND',
%%             {{'FIELD',"title"},"linux"},
%%             "network administration"}
%%     
%%     Query: title:(linux or unix or openbsd) and security
%%     
%%         {'AND',
%%             {{'FIELD',"title"}, 
%%                 {'OR',
%%                     {'OR',"linux","unix"},
%%                     "openbsd"}}, 
%%             "security"}
%% </pre>


%% @spec parse(Query) -> {ok, ast()}
%%       Query = string()
%% @doc
%% Calls {@link parse/2} as <code>parse(S, all)</code>.
%% 
%% This call should only be used during development/testing.
%% @end
parse(Query) ->
	parse(Query, all).

%% @spec parse(Query, LegalFields) -> {ok, ast()}
%%       Query = string()
%%       LegalFields = [atom()] | all
%% @doc 
%% Parses your query string and returns and a syntax tree representation
%% (which can be used with {@link to_string/1}).
%%
%% It's important to accurately specify the list of <code>LegalFields</code> 
%% here, or <code>[]</code> if there are no legitimate fields. 
%% @end
parse(Query, LegalFields) ->
	{ok, Tokens} = wql_scanner:scan(Query),
	{ok, Ast1} = wql_parser_core:parse(Tokens),
	case LegalFields of
		all ->
			{ok, Ast1};
		_ ->
			L = [atom_to_list(A) || A <- LegalFields],
			Ast2 = legalize_fields(Ast1, L),
			{ok, Ast2}
	end.

%% @spec to_string(Ast) -> binary()
%%       Ast = ast()
%% @doc
%% Converts a syntax tree that was previously made by {@link parse/2}
%% into a query string that is compatible with 
%% <a target="_top"
%%    href="http://lucene.apache.org/java/2_0_0/queryparsersyntax.html">
%%        Apache Lucene's Query Parser Syntax</a>. 
%% The string (binary actually) which is returned should be safe to 
%% pass to 
%% <a target="_top"
%%    href="http://lucene.apache.org/java/docs/index.html">Lucene</a>
%% or <a target="_top"
%%       href="http://lucene.apache.org/solr/">Solr</a>.
%%
%% The query string produced is "safe" in the sense that missing 
%% parenthesis are added in, all terms and phrases are quoted, 
%% etc. The only time that Lucene or Solr should have a reason to 
%% complain (ie explode) is if there is a field query on a non-existant
%% field (eg "<code>country:spain</code>" when there is no 
%% <code>country </code> field). To prevent this from happening, make 
%% sure you accurately specify the list of legal fields when you call 
%% {@link parse/2}.
%% @end
to_string(nil) ->
	<<"">>;
to_string(Ast) ->
	iolist_to_binary(to_infix_string(Ast)).

%% @private
%% Note: Doesn't take 'NOT' into account
selected_fields(nil) ->
	[];
selected_fields(Ast) ->
	get_fields(Ast, []).

get_fields({{'FIELD', FieldName}, V}, Acc) ->
	get_fields(V, [FieldName | Acc]);
get_fields({_, L, R}, Acc) ->
	get_fields(R, get_fields(L, Acc));
get_fields({_, V}, Acc) ->
	get_fields(V, Acc);
get_fields(_, Acc) ->
	Acc.

to_infix_string({'AND', L, R}) ->
	["(", to_string(L), " AND ", to_string(R), ")"];
to_infix_string({'OR', L, R}) ->
	["(", to_string(L), " OR ", to_string(R), ")"];
to_infix_string({'NOT', Val}) ->
	["NOT ", to_string(Val)];
to_infix_string({'-', Val}) ->
	["NOT ", to_string(Val)];
to_infix_string({{'FIELD', FieldName}, {'NOT', _} = Val}) ->
	[FieldName, ":(", to_string(Val), ")"];
to_infix_string({{'FIELD', FieldName}, {'-', _} = Val}) ->
	[FieldName, ":(", to_string(Val), ")"];
to_infix_string({{'FIELD', FieldName}, Val}) ->
	[FieldName, ":", to_string(Val)];
to_infix_string(Keyword) when is_list(Keyword) ->
	["\"", Keyword, "\""].

legalize_fields(nil, _) ->
	nil;
legalize_fields({{'FIELD', Name}, _} = T, LegalFields) ->
	case lists:member(Name, LegalFields) of
		true ->
			T;
		false ->
			field_to_keyword(T)
	end;
legalize_fields({Op, L, R}, LegalFields) ->
	Left = legalize_fields(L, LegalFields),
	Right = legalize_fields(R, LegalFields),
	{Op, Left, Right};
legalize_fields({Op, V}, LegalFields) ->
	Val = legalize_fields(V, LegalFields),
	{Op, Val};
legalize_fields(Keyword, _LegalFields) when is_list(Keyword) ->
	Keyword.

field_to_keyword({{'FIELD', Name}, Val}) when is_tuple(Val) ->
	Name ++ ":([unrecognized field tag - inner content removed.)";
field_to_keyword({{'FIELD', Name}, Val}) ->
	Name ++ ":" ++ Val.

