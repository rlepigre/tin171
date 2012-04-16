README - CHINESE CHECKERS SERVER

Req:
Erlang
rabar

NOTE:
THE STATE OF THE SERVER ARE CURRENTLY UNKNOWN.
WILL ADD SOME UNIT-TESTS AND QUICKCHECKS IN COMING DAYS.
WILL ALSO TEST IT OUT WITH MY CLI THAT I'VE WRITTEN.

Setup:
(I HOPE THE REBARSCRIPT IS FUNCTIONAL, OTHERWISE REBAR IS LOCATED AT: https://github.com/basho/rebar)
In project-dir run:
./rebar compile
cd ebin && erl

To start the application in the VM:
application:start(cc).

Configuration:
To configure the port there is a configure file located in ebin called cc.app.
In it there is a tuple called env that defines the application environment.
To alter the port number just change the port tuple.
