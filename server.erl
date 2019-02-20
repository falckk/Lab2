-module(server).
-export([start/1,stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    Pid=spawn(fun () -> server([]) end), %spawns function with an initially empty list of channels
    register(ServerAtom, Pid),  %register Pid to atom
    Pid. %return Pid

% A Channel is in the form: {Channel_name, List_of_nicks_in_channel, List_of_messages}
server(Channels)->
  receive
    {request, From, Ref, {join, Channel, Nick}} ->
      Exists = channel_exists(Channels, Channel),
      if Exists ->
          New_channels=add_nick_to_channel(Channels, Channel, Nick),
          From ! {reply, Ref, ok},
          server(New_channels);
        true ->
          From ! {reply, Ref, ok},
          server([{Channel, [Nick], []} | Channels])
      end;
    {end_server} -> ok
  end.

  %Starts channel_exists.
  channel_exists(Channels, Channel_name) -> channel_exists(Channels, Channel_name, false).

  %Tail recursive check if there is a channel with the name given inside the list of channels.
  channel_exists([], _, Acc) -> Acc;
  channel_exists([H|T], Channel_name, Acc) -> channel_exists(T, Channel_name, Acc orelse check_name(H, Channel_name)).

  %Checks if the name of the channel matches the name given.
  check_name({Channel_name, _, _}, Channel_name) -> true;
  check_name(_,_) -> false.

  %If the nick does not exist in the channel, remove the current channel and replace it with a copy with the nick added.
  add_nick_to_channel(Channels, {Channel_name, List_of_nicks_in_channel, List_of_messages}, Nick_to_add) ->
    Channel={Channel_name, List_of_nicks_in_channel, List_of_messages},
    Nick_exists=nick_exists(Channel, Nick_to_add),
    if Nick_exists ->
        Channels;
      true ->
        New_channels=lists:delete(Channel, Channels),
        [{Channel_name, List_of_nicks_in_channel ++ Nick_to_add, List_of_messages}| New_channels]
    end.


  nick_exists({ _, [], _}, _)->false;
  nick_exists({_, [H|_], _}, Nick) when H=:=Nick -> true;
  nick_exists({Channel_name, [_|List_of_Nicks], _}, Nick)-> nick_exists({Channel_name, List_of_Nicks, no_name}, Nick);
  nick_exists( _, _)->false.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    ServerAtom ! {end_server},
    catch(unregister(ServerAtom)),
    ok.
