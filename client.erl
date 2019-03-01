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
    Not_registered=(whereis(St#client_st.server)==undefined), %does the server exist?
    if Not_registered ->
        {reply, {error, server_not_reached, "The server atom was unregistered."}, St}; %return error servor_not_reached
      true ->
        Ans=request(St#client_st.server, {join, Channel, St#client_st.nick, self()}), %otherwise try to join channel
        if Ans==user_already_joined -> %if it already had joined
            {reply, {error, user_already_joined, "The user had already joined the channel."}, St}; %return error user_already_joined
          true ->
            {reply, ok, St} %otherwise no problems
        end
    end;

% Leave channel
handle(St, {leave, Channel}) ->
    Ans=request(St#client_st.server, {leave, Channel, St#client_st.nick, self()}), %try to leave the channel
    if Ans==user_not_joined -> %if not joined
        {reply, {error, user_not_joined, "The user has not joined the channel."}, St}; %return error user_not_joined
      true ->
        {reply, ok, St} %otherwise no problem
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    Ans=request(list_to_atom(Channel), {message_send, Msg, St#client_st.nick, list_to_atom(Channel), self()}), %try to send messages to a channel
    if Ans==user_not_joined -> %if user hasn't joined channel
        {reply, {error, user_not_joined, "The user has not joined the channel, so it can't send messages in it."}, St}; %return error user_not_joined
      true ->
        {reply, ok, St} %otherwise no problem
    end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Change nick, updated for distinction assignment
handle(St, {nick, NewNick}) ->
        Ans=request(St#client_st.server, {change_nick, St#client_st.nick, NewNick, self()}),
        if Ans==nick_taken ->
            {reply, {error, nick_taken, "The nick is already taken."}, St};
          true ->
            {reply, ok, St#client_st{nick = NewNick}}
    end;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, atom_to_list(Channel), Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, _Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .

%Simplify code
request(Registered_name, Data) ->
    genserver:request(Registered_name, Data).
