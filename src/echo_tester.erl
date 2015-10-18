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

-module(echo_tester).

-behaviour(mercury_handler).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1, stop/0]).
-export([handle/4]).

start(Port) when is_integer(Port) ->
	ok = application:ensure_started(ranch),
	ok = application:ensure_started(mercury),
	{ok, _} = mercury:start(?MODULE, Port, ?MODULE),
	ok.

stop() ->
	mercury:stop(?MODULE).

handle(Operation, Resource, Params, Payload) ->
	error_logger:info_msg("handle(~p, ~p, ~p, ~p)\n", [Operation, Resource, Params, Payload]),
	{reply, 0, Params, Payload}.

%% ====================================================================
%% Internal functions
%% ====================================================================


