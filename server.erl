-module(server).
-export([start/1, stop/1, handle/2]).

% This record defines the structure of the state of a server.
% list_of_Channels is a list of channel_st.
-record(server_st, {
    list_of_Channels,
    list_of_nick_to_Pid
}).

% This record defines the structure of the state of a channel.
-record(channel_st, {
    channel_name,
    list_of_nicks_in_channel
}).

initial_state() ->
    #server_st{
        list_of_Channels = [],
        list_of_nick_to_Pid = []
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, initial_state() , fun handle/2). %starts a gen server, with the initial state of a server and returns its Pid.

handle(St, Data) ->
  handle_internal(St, Data).

handle_internal(St, {join, Channel_name, Nick, Pid}) ->
  Nick_exists=lists:member({Nick, Pid}, St#server_st.list_of_nick_to_Pid),
  if Nick_exists ->
      New_list_of_nick_to_Pid = St#server_st.list_of_nick_to_Pid;
    true ->
      New_list_of_nick_to_Pid = [{Nick, Pid} | St#server_st.list_of_nick_to_Pid]
  end,

  Channels=St#server_st.list_of_Channels,
  Exists = channel_exists(Channels, Channel_name),
  if Exists ->
    Channel=find_channel(Channels, Channel_name),

    %return user_already_joined if already joined
    Nick_exists_in_channel=nick_exists(Channel#channel_st.list_of_nicks_in_channel, Nick),
    if Nick_exists_in_channel ->
        {reply, user_already_joined, St};
      true ->
        New_channels=add_nick_to_channel(Channels, Channel, Nick),
        New_state=St#server_st{list_of_Channels=New_channels, list_of_nick_to_Pid=New_list_of_nick_to_Pid},

        io:fwrite("~p~n", [New_state]),

        {reply, ok, New_state}
    end;
  true ->
    Channel=create_channel(Channel_name, Nick),
    New_channels=[Channel | Channels],
    New_state=St#server_st{list_of_Channels=New_channels, list_of_nick_to_Pid=New_list_of_nick_to_Pid},

    io:fwrite("~p~n", [New_state]),
    io:fwrite("~p~n", [Channel]),

    {reply, ok, New_state}
    end;

handle_internal(St, {leave, Channel_name, Nick}) ->
  Channels=St#server_st.list_of_Channels,
  Exists = channel_exists(Channels, Channel_name),
  if Exists ->
      Channel=find_channel(Channels, Channel_name),
      Nick_exists_in_channel=nick_exists(Channel#channel_st.list_of_nicks_in_channel, Nick),
      if Nick_exists_in_channel->
        New_channels=remove_nick_from_channel(Channels, Channel, Nick),
        New_state=St#server_st{list_of_Channels=New_channels},

        io:fwrite("~p~n", [New_state]),

        {reply, ok, New_state};
      true ->
          {reply, user_not_joined, St}
          end;
    true ->
      {reply, ok, St}
  end;

handle_internal(St, {message_send, Channel_name, Msg, Nick}) ->
  Channels=St#server_st.list_of_Channels,
  Exists = channel_exists(Channels, Channel_name),
  if Exists ->
    Channel=find_channel(Channels, Channel_name),

    %return user_not_joined if not joined
    Nick_exists_in_channel=nick_exists(Channel#channel_st.list_of_nicks_in_channel, Nick),
    if Nick_exists_in_channel ->
        Nicks=Channel#channel_st.list_of_nicks_in_channel,
        [send_message(Msg, Reciever, Channel_name, Nick, St)|| Reciever <- (Nicks--[Nick])],
        {reply, ok, St};
      true ->
        {reply, user_not_joined, St}
    end;
  true ->
      %Error, maybe
      {reply, ok, St}
  end.

send_message(Msg, Reciever, Channel_name, Sender, St) ->
  Pid=find_pid(St, Reciever),
  genserver:request(Pid, {message_receive, Channel_name, Sender, Msg}),
  ok.



find_pid(St, Nick)->
  find_pid_list(St#server_st.list_of_nick_to_Pid, Nick).

find_pid_list([{Nick,Pid}|_], Nick) -> Pid;
find_pid_list([_|T], Nick) -> find_pid_list(T, Nick);
find_pid_list(_,_)-> pid_not_in_list.


remove_nick_from_channel(Channels, Channel, Nick)->
    Nick_exists=nick_exists(Channel#channel_st.list_of_nicks_in_channel, Nick),
    if Nick_exists ->
      New_channel=Channel#channel_st{list_of_nicks_in_channel=Channel#channel_st.list_of_nicks_in_channel--[Nick]},
      New_Channels=Channels--[Channel],
      New_Channels++[New_channel];
    true ->
      Channels
    end.

%Starts channel_exists.
channel_exists(Channels, Channel_name) -> channel_exists(Channels, Channel_name, false).

%Tail recursive check if there is a channel with the name given inside the list of channels.
channel_exists([], _, Acc) -> Acc;

channel_exists([H|T], Channel_name, Acc) ->
  if Acc == true ->
      Acc;
  true ->
    channel_exists(T, Channel_name, Acc orelse check_name(H, Channel_name))
  end.

%Checks if the name of the channel matches the name given.
check_name(Channel_st, Channel_name) when is_record(Channel_st, channel_st) -> Channel_st#channel_st.channel_name==Channel_name;
check_name(_,_) -> false.

find_channel([H|T], Channel_name) ->
  Head_matches=check_name(H, Channel_name),
  if Head_matches ->
      H;
  true ->
      find_channel(T, Channel_name)
  end;
find_channel([], _) ->
  not_in_list.


%If the nick does not exist in the channel, remove the current channel and replace it with a copy with the nick added.
add_nick_to_channel(Channels, Channel, Nick_to_add) ->
  Nick_exists=nick_exists(Channel#channel_st.list_of_nicks_in_channel, Nick_to_add),
  if Nick_exists ->
      Channels;
  true ->
      Old_channels=lists:delete(Channel, Channels),
      New_channel=Channel#channel_st{list_of_nicks_in_channel=Channel#channel_st.list_of_nicks_in_channel++[Nick_to_add]},
        [New_channel | Old_channels]
  end.

nick_exists([Nick|_], Nick)-> true;
nick_exists([_|List_of_Nicks], Nick)-> nick_exists(List_of_Nicks, Nick);
nick_exists( _, _)->false.

%Creates a empty channel, which Nick is a part of.
create_channel(Channel_name, Nick) ->
  #channel_st{channel_name=Channel_name, list_of_nicks_in_channel=[Nick]}.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom),
    ok.
