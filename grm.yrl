Header "%% Copyright (C)"
       "%% @private"
       "%% @Author Alejandro Russo".


Nonterminals line ipnumber unit.

Terminals '/connect' '/join' '/whoami' '/nick' '/ping' '/disconnect' '/leave' '/quit'
          server_name channel nick_name digit dot '{' '}' '@' ',' .

Endsymbol '$end'.

Rootsymbol line.

line -> '/leave' : leave.

line -> '/disconnect' : disconnect.

line -> '/connect' server_name : {connect, ff('$2')}.

line -> '/connect' '{' server_name ','
                       server_name '@' ipnumber
                    '}' : {connect_remote, ff('$3'),
                                           ff('$5')++"@"++'$7'}.

line -> '/join' channel   : {join, ff('$2')}.

line -> '/leave' channel   : {leave, ff('$2')}.

line -> '/quit' : quit.

line -> '/whoami' : whoami.

line -> '/nick' server_name : {nick, ff('$2')}.

line -> '/ping' server_name : {ping, ff('$2')}.

ipnumber -> unit dot unit dot unit dot unit : '$1'++"."++'$3'++"."++'$5'++"."++'$7'.

unit -> digit : ff('$1').
unit -> digit digit : ff('$1')++ff('$2').
unit -> digit digit digit : ff('$1')++ff('$2')++ff('$3').

Erlang code.

ff({_,_,Content}) -> Content.
