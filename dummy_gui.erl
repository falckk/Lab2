-module(dummy_gui).
-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3, start_link/2,
	 handle_info/2, handle_call/3, handle_cast/2]).

% Mirror is the Pid of the process we send all our msgs to
init(Mirror) ->
    {ok, Mirror}.

start_link(Name, Mirror) ->
    gen_server:start_link({local,list_to_atom(Name)}, dummy_gui, Mirror, []).

% Send all messages to Mirror process
handle_call(Call, _From, Mirror) ->
    Mirror ! Call,
    {reply, {}, Mirror}.

handle_info(Info, Mirror) ->
    Mirror ! Info,
    {noreply, Mirror}.

handle_cast(Cast, Mirror) ->
    Mirror ! Cast,
    {noreply, Mirror}.

terminate(_, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.
