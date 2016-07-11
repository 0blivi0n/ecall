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

-module(mercury).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/3,
				 stop/1]).

-spec start(Name :: atom(), Port :: non_neg_integer(), Handler :: atom()) -> {ok, pid()} | {error, badarg}.
start(Name, Port, Handler) when is_atom(Name), is_integer(Port), is_atom(Handler) ->
	Acceptors = application:get_env(mercury, mercury_acceptors),
	MaxConn = application:get_env(mercury, mercury_max_connections),
	Timeout = application:get_env(mercury, mercury_read_timeout),
	TransOpts = [{port, Port}, {max_connections, MaxConn}],
	ProtoOpts = [{mercury_handler, Handler}, {read_timeout, Timeout}],
	ranch:start_listener(Name, Acceptors, ranch_tcp, TransOpts, mercury_protocol, ProtoOpts).

-spec stop(Name :: atom()) -> ok | {error, not_found}.
stop(Name) when is_atom(Name) ->
	ranch:stop_listener(Name).

%% ====================================================================
%% Internal functions
%% ====================================================================


