%%%-------------------------------------------------------------------
%%% @author Emil Falk <emil.falk.1988@gmail.com>
%%% @copyright (C) 2012, Emil Falk
%%% @doc The user supervisor. The user supervisor creates the listening socket
%%% and starts 20 user-process waiting for incoming connections.
%%%
%%% @end
%%% Created : 11 Apr 2012 by Emil Falk <emil.falk.1988@gmail.com>
%%%-------------------------------------------------------------------
-module(cc_user_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_socket/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc Used by a top-level supervisor to start this supervisor as part
%% of a supervisor tree.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc Initialization. Create a listening socket on the application-
%% defined port. Then spawn 20 empty listeners.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, Port} = application:get_env(port),
    {ok, ListenSocket} = gen_tcp:listen(Port, [{reuseaddr, true}, {active, true}, {packet, line}]),
    spawn_link(fun empty_listeners/0),
    {ok, {{simple_one_for_one, 60, 3600},
          [{users,
            {cc_user, start_link, [ListenSocket]},
            temporary, 1000, worker, [cc_user]}]}}.

%%--------------------------------------------------------------------
%% @doc Start a user process
%% @end
%%--------------------------------------------------------------------
start_socket() ->
    supervisor:start_child(?MODULE, []).

%%--------------------------------------------------------------------
%% @doc Start 20 user processes
%% @end
%%--------------------------------------------------------------------
empty_listeners() ->
    [start_socket() || _ <- lists:seq(1,20)],
    ok.
