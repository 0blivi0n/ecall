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

-module(ecall).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/3,
		 stop/1]).

-spec start(Name :: atom(), Port :: non_neg_integer(), Handler :: atom()) -> {ok, pid()} | {error, badarg}.
start(Name, Port, Handler) when is_atom(Name) andalso is_integer(Port) andalso is_atom(Handler) ->
	Acceptors = application:get_env(ecall, ecall_acceptors, 100),
	MaxConn = application:get_env(ecall, ecall_max_connections, infinity),
	Timeout = application:get_env(ecall, ecall_read_timeout, 5000),
	TransOpts = [{port, Port}, {max_connections, MaxConn}],
	ProtoOpts = [{ecall_handler, Handler}, {read_timeout, Timeout}],
	ranch:start_listener(Name, Acceptors, ranch_tcp, TransOpts, ecall_protocol, ProtoOpts).

-spec stop(Name :: atom()) -> ok | {error, not_found}.
stop(Name) when is_atom(Name) ->
	ranch:stop_listener(Name).

%% ====================================================================
%% Internal functions
%% ====================================================================


