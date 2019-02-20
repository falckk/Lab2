Definitions.

Server = [a-z][0-9a-zA-Z_]*

Channel = #[0-9a-zA-Z_]*

NickName = [a-z][0-9a-zA-Z_]*

Whites  = \s+

Connect = /connect

Join    = /join

WhoAmI  = /whoami

Nick    = /nick

Ping    = /ping

Disconnect = /disconnect

Leave = /leave

Quit = /quit

Digit  = [0-9]

Rules.

{Server} :
  {token,{server_name,TokenLine,TokenChars}}.

{Channel} :
  {token,{channel,TokenLine,TokenChars}}.

{NickName} :
  {token,{nick_name,TokenLine,TokenChars}}.

{Connect} :
  {token,{'/connect',TokenLine,TokenChars}}.

{Join} :
  {token,{'/join',TokenLine,TokenChars}}.

{WhoAmI} :
  {token,{'/whoami',TokenLine,TokenChars}}.

{Nick} :
  {token,{'/nick',TokenLine,TokenChars}}.

{Ping} :
  {token,{'/ping',TokenLine,TokenChars}}.

{Disconnect} :
  {token,{'/disconnect',TokenLine,TokenChars}}.

{Leave} :
  {token,{'/leave',TokenLine,TokenChars}}.

{Quit} :
  {token,{'/quit',TokenLine,TokenChars}}.

{Whites} : skip_token.

{Digit} :
  {token, {digit, TokenLine, TokenChars}}.

\. :
  {token, {dot, TokenLine, TokenChars}}.

\{ :
  {token, {'{', TokenLine, TokenChars}}.

\} :
  {token, {'}', TokenLine, TokenChars}}.

\, :
  {token, {',', TokenLine, TokenChars}}.

\@ :
  {token, {'@', TokenLine, TokenChars}}.



Erlang code.
