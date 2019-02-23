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

%-----------------------------------------------------Server-----------------------------------

initial_server_state() ->
    #server_st{
        list_of_Channels = [],
        list_of_nicks = []
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, initial_server_state() , fun handle_server/2). %starts a genserver, with the initial state of a server and returns its Pid.


handle_server(St, {join, Channel_name, Nick, Pid}) ->
  Nick_exists=lists:member(Nick, St#server_st.list_of_nicks),
  if Nick_exists ->
      New_list_of_nicks = St#server_st.list_of_nicks;
    true ->
      New_list_of_nicks = [Nick| St#server_st.list_of_nicks]
  end,

  Channels=St#server_st.list_of_Channels,
  Exists = lists:member(Channel_name, Channels),
  Channel=Channel_name,
  if Exists ->
    %return user_already_joined if already joined
    Nick_exists_in_channel=request(Channel, {check_nick_exists, Nick}),
    if Nick_exists_in_channel ->
        {reply, user_already_joined, St};
      true ->
        request(Channel, {add_nick_to_channel, Nick, Pid}),
        New_state=St#server_st{list_of_nicks=New_list_of_nicks},

        io:fwrite("~p~n", [New_state]),

        {reply, ok, New_state}
    end;
  true ->
    start_channel(Channel_name, Nick, Pid),
    New_channels=[Channel | Channels],
    New_state=St#server_st{list_of_Channels=New_channels, list_of_nicks=New_list_of_nicks},

    io:fwrite("~p~n", [New_state]),

    {reply, ok, New_state}
    end;

handle_server(St, {leave, Channel_name, Nick, Pid}) ->
  Channels=St#server_st.list_of_Channels,
  Exists = lists:member(Channel_name, Channels),
  if Exists ->
      Channel=Channel_name,
      Nick_exists_in_channel=request(Channel, {check_nick_exists, Nick}),
      if Nick_exists_in_channel->
        Ans=request(Channel, {remove_nick_from_channel, Nick, Pid}),

        io:fwrite("~p~n", [St]),

        {reply, Ans, St};
      true ->
          {reply, user_not_joined, St}
          end;
    true ->
      {reply, ok, St}
  end;

handle_server(St, {check_nick_taken, Nick}) ->
  Ans=lists:member(Nick, St#server_st.list_of_nicks),
  {reply, Ans, St};

handle_server(St, {change_nick, Nick, NewNick, Pid}) ->
  Nicks=St#server_st.list_of_nicks,
  New_list_of_nicks=[NewNick| Nicks--[Nick]],
  [request(Channel, {change_nick, Nick, NewNick, Pid}) || Channel <- St#server_st.list_of_Channels],
  New_state=St#server_st{list_of_nicks=New_list_of_nicks},
  {reply, ok, New_state}.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom),
    ok.

  %%--------------------------------------------CHANNEL---------------------------------------------------------------------------

initial_channel_state(Nick, Pid) ->
  #channel_st{
    list_of_nicks_and_Pids_in_channel = [{Nick, Pid}]
  }.

%Creates a empty channel, which Nick and Pid is a part of.
start_channel(Channel_name, Nick, Pid) ->
  State=initial_channel_state(Nick, Pid),
  genserver:start(Channel_name, State, fun handle_channel/2).


handle_channel(St, {check_nick_exists, Nick}) ->
  Nicks_and_Pids=St#channel_st.list_of_nicks_and_Pids_in_channel,
  Nick_exists=nick_exists(Nicks_and_Pids, Nick),
  {reply, Nick_exists, St};

handle_channel(St, {add_nick_to_channel, Nick, Pid}) ->
  Nicks_and_Pids=St#channel_st.list_of_nicks_and_Pids_in_channel,
  Nick_exists=nick_exists(Nicks_and_Pids, Nick),
  if Nick_exists ->
      {reply, nick_exists, St};
    true ->
      New_nicks_and_Pids=[{Nick, Pid}|Nicks_and_Pids],
      New_state=St#channel_st{list_of_nicks_and_Pids_in_channel=New_nicks_and_Pids},
      {reply, ok, New_state}
  end;

handle_channel(St, {remove_nick_from_channel, Nick, Pid}) ->
  Nicks_and_Pids=St#channel_st.list_of_nicks_and_Pids_in_channel,
  Nick_exists=nick_exists(Nicks_and_Pids, Nick),
  if Nick_exists ->
      lists:delete({Nick, Pid}, Nicks_and_Pids),
      {reply, ok, St};
    true ->
      {reply, user_not_joined, St}
  end;

handle_channel(St, {message_send, Msg, Nick, Channel_name, Pid}) ->
  Nicks_and_Pids=St#channel_st.list_of_nicks_and_Pids_in_channel,
  Nick_exists=nick_exists(Nicks_and_Pids, Nick),
  if Nick_exists ->
      [send_message(Msg, Reciever, Channel_name, Nick)|| {_, Reciever} <- [Nicks_and_Pids--[{Nick, Pid}]]],
      {reply, ok, St};
    true ->
      {reply, user_not_joined, St}
  end;

handle_channel(St, {change_nick, Nick, NewNick, Pid}) ->
  Nicks_and_pids=St#channel_st.list_of_nicks_and_Pids_in_channel,
  New_nicks_and_Pids=[{NewNick, Pid}| Nicks_and_pids--[{Nick, Pid}]],
  New_state=St#channel_st{list_of_nicks_and_Pids_in_channel=New_nicks_and_Pids},
  {reply, ok, New_state}.


  nick_exists([{Nick, _}|_], Nick)-> true;
  nick_exists([_|List_of_Nicks], Nick)-> nick_exists(List_of_Nicks, Nick);
  nick_exists( _, _)->false.

  send_message(Msg, Reciever, Channel_name, Sender) ->
    request(Reciever, {message_receive, Channel_name, Sender, Msg}),
    ok.

%-----------------------------------------------Other---------------------------------------

%Simplify code
request(Registered_name, Data) ->
  genserver:request(Registered_name, Data).
