-module(backup3).
-export([start/1, stop/1, handle_backup3/2, handle_channel/2]).

% This record defines the structure of the state of a backup3.
% list_of_Channels is a list of channel names.
% list_of_nicks is a list of nicks.
-record(_st, {
    list_of_Channels,
    list_of_nicks
}).

% This record defines the structure of the state of a channel.
-record(channel_st, {
    list_of_nicks_and_Pids_in_channel
}).

%-----------------------------------------------------backup3-----------------------------------

initial_backup3_state() ->
    #backup3_st{
        list_of_Channels = [],
        list_of_nicks = []
    }.

% Start a new backup3 process with the given name
% Do not change the signature of this function.
start(backup3Atom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to backup3Atom
    % - Return the process ID
    genbackup3:start(backup3Atom, initial_backup3_state() , fun handle_backup3/2). %starts a genbackup3, with the initial state of a backup3 and returns its Pid.


handle_backup3(St, {join, Channel_name, Nick, Pid}) ->
  Channel=list_to_atom(Channel_name),
  Nick_exists=lists:member(Nick, St#backup3_st.list_of_nicks),
  if Nick_exists ->
      New_list_of_nicks = St#backup3_st.list_of_nicks;
    true ->
      New_list_of_nicks = [Nick| St#backup3_st.list_of_nicks]
  end,

  Channels=St#backup3_st.list_of_Channels,
  Exists = lists:member(Channel, Channels),
  if Exists ->
    %return user_already_joined if already joined
    Nick_exists_in_channel=request(Channel, {check_nick_exists, Nick}),
    if Nick_exists_in_channel ->
        {reply, user_already_joined, St};
      true ->
        request(Channel, {add_nick_to_channel, Nick, Pid}),
        New_state=St#backup3_st{list_of_nicks=New_list_of_nicks},

        io:fwrite("~p~n", [New_state]),

        {reply, ok, New_state}
    end;
  true ->
    start_channel(Channel, Nick, Pid),
    New_channels=[Channel | Channels],
    New_state=St#backup3_st{list_of_Channels=New_channels, list_of_nicks=New_list_of_nicks},

    io:fwrite("~p~n", [New_state]),

    {reply, ok, New_state}
    end;

handle_backup3(St, {leave, Channel_name, Nick, Pid}) ->
  Channel=list_to_atom(Channel_name),
  Channels=St#backup3_st.list_of_Channels,
  Exists = lists:member(Channel, Channels),
  if Exists ->
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

handle_backup3(St, {check_nick_taken, Nick}) ->
  Ans=lists:member(Nick, St#backup3_st.list_of_nicks),
  {reply, Ans, St};

handle_backup3(St, {change_nick, Nick, NewNick, Pid}) ->
  Nicks=St#backup3_st.list_of_nicks,
  New_list_of_nicks=[NewNick| Nicks--[Nick]],
  [request(Channel, {change_nick, Nick, NewNick, Pid}) || Channel <- St#backup3_st.list_of_Channels],
  New_state=St#backup3_st{list_of_nicks=New_list_of_nicks},
  {reply, ok, New_state}.

% Stop the backup3 process registered to the given name,
% together with any other associated processes
stop(backup3Atom) ->
    % TODO Implement function
    % Return ok
    genbackup3:stop(backup3Atom),
    ok.

  %%--------------------------------------------CHANNEL---------------------------------------------------------------------------

initial_channel_state(Nick, Pid) ->
  #channel_st{
    list_of_nicks_and_Pids_in_channel = [{Nick, Pid}]
  }.

%Creates a empty channel, which Nick and Pid is a part of.
start_channel(Channel_name, Nick, Pid) ->
  State=initial_channel_state(Nick, Pid),
  genbackup3:start(Channel_name, State, fun handle_channel/2).


handle_channel(St, {check_nick_exists, Nick}) ->
  if St==[] ->
      {reply, false, St};
    true ->
      Nicks_and_Pids=St#channel_st.list_of_nicks_and_Pids_in_channel,
      Nick_exists=nick_exists(Nicks_and_Pids, Nick),
      {reply, Nick_exists, St}
  end;

handle_channel(St, {add_nick_to_channel, Nick, Pid}) ->
  if St==[] ->
      Nicks_and_Pids=[];
    true ->
      Nicks_and_Pids=St#channel_st.list_of_nicks_and_Pids_in_channel
  end,

  Nick_exists=nick_exists(Nicks_and_Pids, Nick),
  if Nick_exists ->
      {reply, nick_exists, St};
  true ->
      New_nicks_and_Pids=[{Nick, Pid}|Nicks_and_Pids],
    if St==[] ->
          New_state=initial_channel_state(Nick, Pid);
      true ->
        New_state=St#channel_st{list_of_nicks_and_Pids_in_channel=New_nicks_and_Pids},
        {reply, ok, New_state}
    end,
  end;

handle_channel(St, {remove_nick_from_channel, Nick, Pid}) ->
  if St==[] ->
      Nicks_and_Pids=[];
    true ->
      Nicks_and_Pids=St#channel_st.list_of_nicks_and_Pids_in_channel
  end,

  Nick_exists=nick_exists(Nicks_and_Pids, Nick),
  if Nick_exists ->
      New_nicks_and_Pids=lists:delete({Nick, Pid}, Nicks_and_Pids),
      New_state=St#channel_st{list_of_nicks_and_Pids_in_channel=New_nicks_and_Pids},
      {reply, ok, New_state};
    true ->
      {reply, user_not_joined, St}
  end;

handle_channel(St, {message_send, Msg, Nick, Channel_name, Pid}) ->

  io:fwrite("~p~n", [Channel_name]),

  if St==[] ->
      Nicks_and_Pids=[];
    true ->
      Nicks_and_Pids=St#channel_st.list_of_nicks_and_Pids_in_channel
  end,

  io:fwrite("~p~n", [Nicks_and_Pids]),
  io:fwrite("~p~n", [Nick]),
  io:fwrite("~p~n", [Pid]),

  Nick_exists=nick_exists(Nicks_and_Pids, Nick),

  io:fwrite("~p~n", [Nick_exists]),

  if Nick_exists ->

      io:fwrite("~p~n", [Nick_exists]),

      [send_message(Msg, Reciever, Channel_name, Nick)|| {_, Reciever} <- Nicks_and_Pids--[{Nick, Pid}]],
      {reply, ok, St};
    true ->
      {reply, user_not_joined, St}
  end;

handle_channel(St, {change_nick, Nick, NewNick, Pid}) ->
  if St==[] ->
      Nicks_and_Pids=[];
    true ->
      Nicks_and_Pids=St#channel_st.list_of_nicks_and_Pids_in_channel
  end,
  New_nicks_and_Pids=[{NewNick, Pid}| Nicks_and_Pids--[{Nick, Pid}]],
  New_state=St#channel_st{list_of_nicks_and_Pids_in_channel=New_nicks_and_Pids},
  {reply, ok, New_state}.


  nick_exists([{Nick, _}|_], Nick)-> true;
  nick_exists([_|List_of_Nicks], Nick)-> nick_exists(List_of_Nicks, Nick);
  nick_exists( _, _)->false.

  send_message(Msg, Reciever, Channel_name, Sender) ->

    io:fwrite("~p~n", [Reciever]),
    io:fwrite("~p~n", [Sender]),

    request(Reciever, {message_receive, Channel_name, Sender, Msg}),
    ok.

%-----------------------------------------------Other---------------------------------------

%Simplify code
request(Registered_name, Data) ->
  genbackup3:request(Registered_name, Data).
