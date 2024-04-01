-module(scribe_scanner).
-author('Tyra Bjurman <tyra.bjurman@gmail.com>').
-export([
	tokenize/1
]).

%% ----------------------------------------------------------------------------
% @spec tokenize(Template) -> [Tokens]
% @doc Tokenizes a template text string
%% ----------------------------------------------------------------------------
tokenize(Template) ->
	Tokens = tokenize(Template,{1,1},text, {{1,1}, {text, []}}),
	lists:reverse(lists:flatten(Tokens)).

%% Text mode
tokenize(Template,{Row,Column},text, Token) ->
	case Template of
		%% Start script
		["<?" | Rest] ->
			NewToken = {{Row,Column+2}, {variable, []}},
			Tokens = add_tokens({{Row,Column}, script_start}, Token),
			[tokenize(Rest,{Row,Column+2},script, NewToken) | Tokens];

		%% Text
		[$\n | Rest] ->
			%% Append text with a new line
			tokenize(Rest,{Row+1,1},text, append_token(Token, $\n));
		[Symbol | Rest] ->
			%% Append text with an arbitrary symbol
			tokenize(Rest,{Row,Column+1},text, append_token(Token, Symbol))
	end;

%% Script mode
tokenize(Template,{Row,Column},script, Token) ->
	case Template of
		%% End script
		["?>" | Rest] ->
			NewToken = {{Row,Column+2}, {text, []}},
			Tokens = add_tokens({{Row,Column}, script_end}, Token),
			[tokenize(Rest,{Row,Column+2},text, NewToken) | Tokens];
		
		%% Variable
		[" " | Rest] ->
			%% Ignore spaces in script
			tokenize(Rest,{Row,Column+1},script, Token);
		[$\t | Rest] ->
			%% Ignore tabulations in script
			tokenize(Rest,{Row,Column+1},script, Token);
		[$\n | Rest] ->
			%% Ignore new lines in script
			tokenize(Rest,{Row+1,1},script, Token);
		[Symbol | Rest] ->
			%% Append variable with an arbitrary symbol
			tokenize(Rest,{Row,Column+1},script, append_token(Token, Symbol))
	end;

%% End of template
tokenize([],{_Row,_Column},_Mode, Token) ->
	case Token of
		{_Position, {_Type, []}} ->
			%% Do not add a token without content
			[];
		{Position, {Type, Content}} ->
			%% Add final content token to the token list
			{Position, {Type, lists:reverse(lists:flatten(Content))}}
	end.
	
%% ----------------------------------------------------------------------------
% @spec
% @doc Append a symbol to existing content
%% ----------------------------------------------------------------------------
append_token({Position, {Type, Content}}, Symbol) ->
	%% Append a symbol to the content token
	{Position, {Type, [Symbol | Content]}}.

%% ----------------------------------------------------------------------------
% @spec
% @doc Add tokens to the token list
%% ----------------------------------------------------------------------------
add_tokens(Delimiter, {_Position, {_Type, []}}) ->
	%% Do not add a token without content
	Delimiter;
add_tokens(Delimiter, {Position, {Type, Content}}) ->
	%% Add a delimiter and a content token
	[Delimiter, {Position, {Type, lists:reverse(lists:flatten(Content))}}].
