-module(genserver).
-export([start/3, stop/1, request/2, request/3, update/2]).

% Spawn a process and register it with a given atom
% - Atom is the atom to register the process to
% - State is the initial state for the server loop
% - Function F is the body of the server:
%   - takes 2 params : state, request message
%   - returns a tuple: new state, response message
start(Atom, State, F) ->
  Pid = spawn(fun() -> loop(State, F) end),
  catch(unregister(Atom)),
  register(Atom, Pid),
  Pid.

stop(Atom) ->
  Atom!stop,
  catch(unregister(Atom)),
  ok.

loop(State, F) ->
  receive
    {request, From, Ref, Data} ->
      case catch(F(State, Data)) of
        {'EXIT', Reason} ->
          From!{exit, Ref, Reason},
          loop(State, F);
        {reply, R, NewState} ->
          From!{result, Ref, R},
          loop(NewState, F)
        end;
    {update, From, Ref, NewF} ->
      From ! {ok, Ref},
      loop(State, NewF);
    stop ->
      true
  end.

% Send a request to a Pid and wait for a response
request(Pid, Data) ->
  request(Pid, Data, 3000).

% Send a request to a Pid and wait for a response
% With a specified timeout
% If Pid is an atom which is not registered, a badarg exception will be thrown
request(Pid, Data, Timeout) ->
  Ref = make_ref(),
  Pid!{request, self(), Ref, Data},
  receive
    {result, Ref, Result} ->
      Result;
    {exit, Ref, Reason} ->
      exit(Reason)
  after Timeout ->
    exit("Timeout")
  end.

% Update loop function
update(Pid, Fun) ->
  Ref = make_ref(),
  Pid!{update, self(), Ref, Fun},
  receive
    {ok, Ref} ->
      ok
  end.
