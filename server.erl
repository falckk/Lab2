-module(server).
-export([start/1, stop/1, handle_server/2, handle_channel/2]).

% This record defines the structure of the state of a server.
% list_of_Channels is a list of channel names.
% list_of_nicks is a list of nicks.
-record(server_st, {
    list_of_Channels,
    list_of_nicks
}).

% This record defines the structure of the state of a channel.
-record(channel_st, {
    list_of_nicks_and_Pids_in_channel
}).

% The server manages join, leave and change nick requests and create channels.
% The channels start processes for sending messages and keep track of the users in the channel.

%-----------------------------------------------------Server-----------------------------------

initial_server_state() ->
    #server_st{
        list_of_Channels = [],
        list_of_nicks = []
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, initial_server_state() , fun handle_server/2). %starts a genserver, with the initial state of a server and returns its Pid.


handle_server(St, {join, Channel_name, Nick, Pid}) ->   %join handle function
  Channel=list_to_atom(Channel_name),   %change incoming string to atom
  Nick_exists=lists:member(Nick, St#server_st.list_of_nicks), %check if the nick already exists
  if Nick_exists ->
      New_list_of_nicks = St#server_st.list_of_nicks;   %if it exists, new list is the same as the old one
    true ->
      New_list_of_nicks = [Nick| St#server_st.list_of_nicks] %if not, add it to the list
  end,

  Channels=St#server_st.list_of_Channels, %list of channels
  Exists = lists:member(Channel, Channels),
  %if the given channel already exists
  if Exists ->
    %return user_already_joined if already joined
    Nick_exists_in_channel=request(Channel, {check_nick_exists, Nick}),
    if Nick_exists_in_channel ->
        {reply, user_already_joined, St};
      true -> %otherwise add the nick to the channel
        request(Channel, {add_nick_to_channel, Nick, Pid}),
        New_state=St#server_st{list_of_nicks=New_list_of_nicks},
        {reply, ok, New_state}
    end;
  %if the channel does not exist
  true ->
    start_channel(Channel, Nick, Pid), %create a new channel with the given name
    New_channels=[Channel | Channels], %add it to the list
    New_state=St#server_st{list_of_Channels=New_channels, list_of_nicks=New_list_of_nicks}, %update the state
    {reply, ok, New_state}
    end;

handle_server(St, {leave, Channel_name, Nick, Pid}) ->  %leave handle function
  Channel=list_to_atom(Channel_name), %string to atom
  Channels=St#server_st.list_of_Channels, %list of channels
  Exists = lists:member(Channel, Channels),
  %if Channel exists among the existing channels
  if Exists ->
      Nick_exists_in_channel=request(Channel, {check_nick_exists, Nick}), %check if the nick exists in the channel
      if Nick_exists_in_channel->
        Ans=request(Channel, {remove_nick_from_channel, Nick, Pid}), %if it does, remove it
        {reply, Ans, St};
      true ->
          {reply, user_not_joined, St} %if nick is not in channel, reply user_not_joined
          end;
    true ->
      {reply, ok, St} %if the channel did not exist then simply reply ok (nothing happened)
  end;

handle_server(St, {shut_down_channels}) ->
  [stop_Channel(Channel) || Channel <- St#server_st.list_of_Channels],
  {reply, ok, St};

handle_server(St, {change_nick, Nick, NewNick, Pid}) -> %change nick handle function (the client calls this only method if the nick is not already taken)
  Ans=lists:member(NewNick, St#server_st.list_of_nicks),
  if Ans ->
      {reply, nick_taken, St};
    true ->
      Nicks=St#server_st.list_of_nicks, %list of nicks
      New_list_of_nicks=[NewNick| Nicks--[Nick]], %add new nick to the list and remove the old one
      [request(Channel, {change_nick, Nick, NewNick, Pid}) || Channel <- St#server_st.list_of_Channels], %change nick in all channels
      New_state=St#server_st{list_of_nicks=New_list_of_nicks}, %update the state and return it
      {reply, ok, New_state}
  end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) -> %shut down the server
    request(ServerAtom, {shut_down_channels}),

    io:fwrite("Request works~n"),

    genserver:stop(ServerAtom),

    io:fwrite("Stop server works~n"),

    ok.

  %%--------------------------------------------CHANNEL---------------------------------------------------------------------------

initial_channel_state(Nick, Pid) -> %
  #channel_st{
    list_of_nicks_and_Pids_in_channel = [{Nick, Pid}]
  }.

%Creates a empty channel, which Nick and Pid is a part of.
start_channel(Channel_name, Nick, Pid) ->
  State=initial_channel_state(Nick, Pid),
  genserver:start(Channel_name, State, fun handle_channel/2).


%handle function for checking if nick exists in channel
handle_channel(St, {check_nick_exists, Nick}) ->
  Nicks_and_Pids=St#channel_st.list_of_nicks_and_Pids_in_channel,
  Nick_exists=nick_exists(Nicks_and_Pids, Nick),
  {reply, Nick_exists, St};

%handle function for adding a nick to a channel
handle_channel(St, {add_nick_to_channel, Nick, Pid}) ->
  Nicks_and_Pids=St#channel_st.list_of_nicks_and_Pids_in_channel, %list of tuples containing coupled nicks and pids

  Nick_exists=nick_exists(Nicks_and_Pids, Nick),
  %if nick exists
  if Nick_exists ->&& erl -eval "eunit:test({test, test_client, message_throughput_test}), halt()."annel
      {reply, nick_exists, St}; %return reply nick_exists and leave the state unchanged
  true -> %otherwise
      New_nicks_and_Pids=[{Nick, Pid}|Nicks_and_Pids], %add the nick and pid to the list
      New_state=St#channel_st{list_of_nicks_and_Pids_in_channel=New_nicks_and_Pids}, %update state
      {reply, ok, New_state} %return reply, ok and the updated state
  end;

%handle function of removing a nick from the channel
handle_channel(St, {remove_nick_from_channel, Nick, Pid}) ->
  Nicks_and_Pids=St#channel_st.list_of_nicks_and_Pids_in_channel, %list of nicks and pids
  Nick_exists=nick_exists(Nicks_and_Pids, Nick),
  %if nick exists
  if Nick_exists ->
      New_nicks_and_Pids=lists:delete({Nick, Pid}, Nicks_and_Pids), %delete it from the list
      New_state=St#channel_st{list_of_nicks_and_Pids_in_channel=New_nicks_and_Pids}, %update the state
      {reply, ok, New_state}; %and return the new state
    true -> %if not&& erl -eval "eunit:test({test, test_client, message_throughput_test}), halt()."annel
      {reply, user_not_joined, St} %return reply user_not_joined and leave state as it is
  end;

%handle function of sending messages for channels
handle_channel(St, {message_send, Msg, Nick, Channel_name, Pid}) ->
  Nicks_and_Pids=St#channel_st.list_of_nicks_and_Pids_in_channel, %list of nicks and pids in channel
  Nick_exists=nick_exists(Nicks_and_Pids, Nick), %if the nick exists in channel
  if Nick_exists ->
      create_message_sender(Msg, Channel_name, Nick, Nicks_and_Pids, Pid),  %send message to everyone in the channel except the original sender
      {reply, ok, St}; %and reply
    true ->
      {reply, user_not_joined, St} %otherwise reply with user_not_joined
  end;
&& erl -eval "eunit:test({test, test_client, message_throughput_test}), halt()."annel
%handle function for changing a nick in a channel
handle_channel(St, {change_nick, Nick, NewNick, Pid}) ->
  Nicks_and_Pids=St#channel_st.list_of_nicks_and_Pids_in_channel, %list of nicks and pids
  New_nicks_and_Pids=[{NewNick, Pid}| Nicks_and_Pids--[{Nick, Pid}]], %add new nick to the list and delete the old one
  New_state=St#channel_st{list_of_nicks_and_Pids_in_channel=New_nicks_and_Pids},
  {reply, ok, New_state}. %return the new state


  %help functions to check if the nick exists in the channel
  nick_exists([{Nick, _}|_], Nick)-> true;
  nick_exists([_|List_of_Nicks], Nick)-> nick_exists(List_of_Nicks, Nick);
  nick_exists( _, _)->false.

  %Spawns a process that send the message to each client to increase preformance.
  create_message_sender(Msg, Channel_name, Nick, Nicks_and_Pids, Pid) ->
    spawn(fun() -> send_messages(Msg, Channel_name, Nick, Nicks_and_Pids, Pid) end).

  %functions that actually sends messages
  send_messages(Msg, Channel_name, Nick, Nicks_and_Pids, Pid) ->
    [send_message(Msg, Reciever, Channel_name, Nick)|| {_, Reciever} <- Nicks_and_Pids--[{Nick, Pid}]].

  send_message(Msg, Reciever, Channel_name, Sender) ->
    request(Reciever, {message_receive, Channel_name, Sender, Msg}),
    ok.

stop_Channel(Channel_atom) ->
  genserver:stop(Channel_atom),
  ok.

%-----------------------------------------------Other---------------------------------------

%Simplify code
request(Registered_name, Data) ->
  genserver:request(Registered_name, Data).
