
Nonterminals 
	line query	res_query 
	or_query 	or_res_query 
	not		item 		
	res_item 	keyword_item 
	field_item	illegal_field_item
	subquery 	restricted_subquery 
	bin_op.

Terminals 'KEYWORD' 'FIELD' '(' ')' 'AND' 'OR' 'NOT' '|' '-'.

Rootsymbol line.

Endsymbol '$thats_all_folks$'.

%%-------------------------------------------------------------
%% The Fun Starts Here!!
%%-------------------------------------------------------------

line -> query : '$1'.
line -> '$empty' : nil.

query -> item : '$1'.
query -> bin_op : bin_node('$1', nil, nil).
query -> query item : bin_node('AND', '$1', '$2').	% default to 'AND'
query -> query bin_op : bin_node('$2', '$1', nil).
query -> query bin_op item : bin_node('$2', '$1', '$3').

% Restricted query is the same as query but it deals only in restricted items
res_query -> res_item : '$1'.
res_query -> bin_op : bin_node('$1', nil, nil).
res_query -> res_query res_item : bin_node('AND', '$1', '$2').	% default to 'AND'
res_query -> res_query bin_op : bin_node('$2', '$1', nil).
res_query -> res_query bin_op res_item : bin_node('$2', '$1', '$3').

% Exactly the same as query but it defaults to 'OR' rather than 'AND'
or_query -> item : '$1'.
or_query -> bin_op : bin_node('$1', nil, nil).
or_query -> or_query item : bin_node('OR', '$1', '$2').		% default to 'OR'
or_query -> or_query bin_op : bin_node('$2', '$1', nil).
or_query -> or_query bin_op item : bin_node('$2', '$1', '$3').

% Exactly the same as res_query but it defaults to 'OR' rather than 'AND'
or_res_query -> res_item : '$1'.
or_res_query -> bin_op : bin_node('$1', nil, nil).
or_res_query -> or_res_query res_item : bin_node('OR', '$1', '$2').	% default to 'OR'
or_res_query -> or_res_query bin_op : bin_node('$2', '$1', nil).
or_res_query -> or_res_query bin_op res_item : bin_node('$2', '$1', '$3').

item -> keyword_item : '$1'.
item -> subquery : '$1'.
item -> field_item : '$1'.
item -> not item : not_node('$2').

res_item -> keyword_item : '$1'.
res_item -> illegal_field_item : '$1'.
res_item -> restricted_subquery : '$1'.
res_item -> not res_item : not_node('$2').

keyword_item -> 'KEYWORD' : value_of('$1').

field_item -> 'FIELD' keyword_item : field_node(value_of('$1'), '$2').
field_item -> 'FIELD' restricted_subquery  : field_node(value_of('$1'), '$2'). 

% We strip out the field-names - they are not permitted here
illegal_field_item -> 'FIELD' keyword_item : '$2'.  
illegal_field_item -> 'FIELD' restricted_subquery : '$2'.  

% Many of these are just meaningless statements that we reduce to nil.
subquery -> '(' query ')' : '$2'.
subquery -> '(' ')' : nil.
subquery -> '(' 'FIELD' ')' : nil.
subquery -> '(' '|' or_query ')' : '$3'.
subquery -> '(' '|' ')' : nil.
subquery -> '(' '|' 'FIELD' ')' : nil.

% Many of these are just meaningless statements that we reduce to nil.
restricted_subquery -> '(' res_query ')' : '$2'.
restricted_subquery -> '(' ')' : nil.
restricted_subquery -> '(' 'FIELD' ')' : nil.
restricted_subquery -> '(' '|' or_res_query ')' : '$3'.
restricted_subquery -> '(' '|' ')' : nil.
restricted_subquery -> '(' '|' 'FIELD' ')' : nil.

bin_op -> 'AND' : 'AND'.
bin_op -> 'OR' : 'OR'.

not -> 'NOT' : 'NOT'.
not -> '-' : 'NOT'.


Erlang code.

value_of(Token) ->
	element(3, Token).

bin_node(_Op, nil, nil) ->
	nil;
bin_node(_Op, X, nil) ->
	X;
bin_node(_Op, nil, X) ->
	X;
bin_node(Op, X, Y) ->
	{Op, X, Y}.

not_node(nil) ->
	nil;
not_node({'NOT', _} = Val) ->
	Val;
not_node({'-', _} = Val) ->
	Val;
not_node(Val) ->
	{'NOT', Val}.

field_node(_FieldName, nil) ->
	nil;
field_node(FieldName, Val) ->
	{{'FIELD', FieldName}, Val}.


