-module(scribe_parser).
-author('Tyra Bjurman <tyra.bjurman@gmail.com>').
-export([
	interpret/1
]).

%% ----------------------------------------------------------------------------
% @spec interpret(Tokens) -> [Content]
% @doc Interprets tokens for content
%% ----------------------------------------------------------------------------
interpret(Tokens) ->
	lists:reverse(lists:flatten(add_content(Tokens))).

%% Content
add_content(Tokens) ->
	case Tokens of
		[{Position, {text, Text}} | Rest] ->
			%% Add text
			[add_content(Rest) | {Position, {text, Text}}];
		[{Position, script_start} | Rest] ->
			%% Add script
			add_script(Tokens, {Position, {script, []}});
		[] ->
			%% End of tokens
			[]	
	end.
	
%% Script
add_script(Tokens, {Position, {script, Variable}}) ->
	case Tokens of
		[{Position, {variable, Variable}} | Rest] ->
			%% Add variable 
			add_script(Rest, {Position, {script, Variable}});
		[{Position, script_end} | Rest] ->
			%% End of script
			[add_content(Rest) | {Position, {script, Variable}}]
	end.
