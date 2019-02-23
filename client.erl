-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    % TODO: Implement this function
    % {reply, ok, St} ;
    Ans=request(St#client_st.server, {join, Channel, St#client_st.nick, self()}),
    if Ans==user_already_joined ->
        {reply, {error, user_already_joined, "The user had already joined the channel."}, St};
      true ->
        {reply, ok, St}
    end;

% Leave channel
handle(St, {leave, Channel}) ->
    % TODO: Implement this function
    % {reply, ok, St};

      io:fwrite("Client got the message.~n"),

    Ans=request(St#client_st.server, {leave, Channel, St#client_st.nick, self()}),
    if Ans==user_not_joined ->
        {reply, {error, user_not_joined, "The user has not joined the channel."}, St};
      true ->
        {reply, ok, St}
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    % TODO: Implement this function
    % {reply, ok, St} ;
    Ans=request(Channel, {message_send, Msg, St#client_st.nick, Channel, self()}),
    if Ans==user_not_joined ->
        {reply, {error, user_not_joined, "The user has not joined the channel, so it can't send messages in it."}, St};
      true ->
        {reply, ok, St}
    end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Change nick
handle(St, {nick, NewNick}) ->
    Nick_taken=request(St#client_st.server, {check_nick_taken, NewNick}),
    if Nick_taken ->
        {reply, {error, nick_taken, "The nickname is already taken."}, St};
      true ->
        request(St#client_st.server, {change_nick, St#client_st.nick, NewNick, self()}),
        {reply, ok, St#client_st{nick = NewNick}}
    end;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .

%Simplify code
request(Registered_name, Data) ->
    genserver:request(Registered_name, Data).
