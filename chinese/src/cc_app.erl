%%%-------------------------------------------------------------------
%%% @author Emil Falk <emil.falk.1988@gmail.com>
%%% @copyright (C) 2012, Emil Falk
%%% @doc Top level application for the chinese checkers server.
%%% @end
%%% Created : 11 Apr 2012 by Emil Falk <emil.falk.1988@gmail.com>
%%%-------------------------------------------------------------------
-module(cc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc Start the chinese checkers application.
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    cc_sup:start_link().

%%--------------------------------------------------------------------
%% @doc Stop the chinese checkers application.
%% @end
%%--------------------------------------------------------------------w
stop(_State) ->
    ok.
