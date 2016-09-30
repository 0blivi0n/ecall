%%
%% Copyright 2015-16 Joaquim Rocha <jrocha@gmailbox.org>
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

-module(mercury_protocol).

-include("mercury.hrl").

-behaviour(ranch_protocol).

-define(SERVER_ERROR, ?MERCURY_REPLY(?MERCURY_INTERNAL_SERVER_ERROR, ?MERCURY_NO_PARAMS, ?MERCURY_NO_PAYLOAD)).
-define(FAILED, failed).

%% API functions
%% ====================================================================

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, Opts) ->
	ok = ranch:accept_ack(Ref),
	{_, Handler} = lists:keyfind(mercury_handler, 1, Opts),
	{_, Timeout} = lists:keyfind(read_timeout, 1, Opts),
	loop(Socket, Transport, Handler, Timeout).

%% ====================================================================
%% Internal functions
%% ====================================================================

loop(Socket, Transport, Handler, Timeout) ->
	case Transport:recv(Socket, 0, Timeout) of
		{ok, Input} ->
			Output = handle(Handler, Input),
			Transport:send(Socket, Output),
			loop(Socket, Transport, Handler, Timeout);
		_ ->
			ok = Transport:close(Socket)
	end.

handle(Handler, Input) ->
	Request = decode(Input),
	Reply = process(Handler, Request),
	encode(Reply).

process(Handler, ?MERCURY_REQUEST(Operation, Resource, Params, Payload)) when is_binary(Operation), is_list(Resource), is_map(Params) ->
	case invoke(Handler, Operation, Resource, Params, Payload) of
		Reply = ?MERCURY_REPLY(Status, Props, _) when is_integer(Status), is_map(Props) -> Reply;
		?FAILED -> ?SERVER_ERROR;
		Other ->
			LogArgs = [?MODULE, Handler, Other],
			error_logger:error_msg("~p: Invalid reply from module ~p: ~p\n", LogArgs),
			?SERVER_ERROR
	end;
process(_Handler, ?FAILED) -> ?SERVER_ERROR;
process(_Handler, Request) ->
	LogArgs = [?MODULE, Request],
	error_logger:error_msg("~p: Not a valid request: ~p\n", LogArgs),
	?SERVER_ERROR.

invoke(Handler, Operation, Resource, Params, Payload) ->
	try Handler:handle(Operation, Resource, Params, Payload)
	catch Error:Reason ->
			LogArgs = [?MODULE, Handler, Operation, Resource, Params, Payload, Error, Reason],
			error_logger:error_msg("~p: Error while executing ~p:handle(~p, ~p, ~p, ~p) -> ~p:~p\n", LogArgs),
			?FAILED
	end.

decode(Input) ->
	try erlang:binary_to_term(Input, [safe])
	catch Error:Reason ->
			LogArgs = [?MODULE, Input, Error, Reason],
			error_logger:error_msg("~p: Error while executing erlang:binary_to_term(~p, [safe]) -> ~p:~p\n", LogArgs),
			?FAILED
	end.

encode(Reply) -> erlang:term_to_binary(Reply).