%%
%% Copyright 2015 Joaquim Rocha <jrocha@gmailbox.org>
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

-module(ecall_protocol).

-include("ecall.hrl").

-behaviour(ranch_protocol).

-define(REQUEST(Operation, Resource, Params, Payload), {request, Operation, Resource, Params, Payload}).
-define(RESPONSE(Status, Payload), {response, Status, Payload}).
-define(SIMPLE_RESPONSE(Status), ?RESPONSE(Status, empty)).
-define(SERVER_ERROR, ?SIMPLE_RESPONSE(?ECALL_INTERNAL_SERVER_ERROR)).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, Opts) ->
	ok = ranch:accept_ack(Ref),
	{_, Handler} = lists:keyfind(ecall_handler, 1, Opts), 
	{_, Timeout} = lists:keyfind(read_timeout, 1, Opts), 
	loop(Socket, Transport, Handler, Timeout).

%% ====================================================================
%% Internal functions
%% ====================================================================

loop(Socket, Transport, Handler, Timeout) ->
	case Transport:recv(Socket, 0, Timeout) of
		{ok, Data} ->
			Reply = process(Handler, Data),
			Transport:send(Socket, Reply),
			loop(Socket, Transport, Handler, Timeout);
		_ ->
			ok = Transport:close(Socket)
	end.

process(Handler, Data) ->
	Request = erlang:binary_to_term(Data, [safe]),
	Response = response(Handler, Request),
	erlang:term_to_binary(Response).

response(Handler, ?REQUEST(Operation, Resource, Params, Payload)) 
		when is_binary(Operation) 
		andalso is_list(Resource) 
		andalso is_list(Params) ->
	try Handler:handle(Operation, Resource, Params, Payload) of
		{reply, Status} when is_integer(Status) ->
			?SIMPLE_RESPONSE(Status);
		{reply, Status, Reply} when is_integer(Status) ->
			?RESPONSE(Status, Reply);
		Msg ->
			error_logger:error_report("~p: Invalid reply from module ~p: ~p\n", [?MODULE, Handler, Msg]),
			?SERVER_ERROR
	catch Error:Reason -> 
			LogArgs = [?MODULE, Handler, Operation, Resource, Params, Payload, Error, Reason],
			error_logger:error_report("~p: Error while executing ~p:handle(~p, ~p, ~p, ~p) -> ~p:~p\n", LogArgs),
			?SERVER_ERROR
	end;
response(_Handler, Request) -> 
	error_logger:error_report("~p: Not a valid request: ~p\n", [?MODULE, Request]),
	?SERVER_ERROR.