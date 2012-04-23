%%%-------------------------------------------------------------------
%%% @author Emil Falk <emil.falk.1988@gmail.com>
%%% @copyright (C) 2012, Emil Falk
%%% @doc
%%% The user process. Accepts an incoming connection and relays
%%% messages between the connecting client and the chinese checkers application.
%%% @end
%%% Created : 28 Mar 2012 by Emil Falk <emil.falk.1988@gmail.com>
%%%-------------------------------------------------------------------

%% ADD MORE CHECKS TO USER INPUT

-module(cc_user).

-behaviour(gen_server).

-import(erl_scan).
-import(erl_parse).
-import(io_lib).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(pl, {name ,
             st = disconnected,
             socket
            }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init(Socket) ->
    gen_server:cast(self(), accept),
    {ok, #pl{socket = Socket}}.

%%--------------------------------------------------------------------
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(accept, S=#pl{socket=LSock}) ->
    {ok, ASock} = gen_tcp:accept(LSock),
    cc_user_sup:start_socket(),
    {noreply, S#pl{socket=ASock}};
handle_cast({GamePid, GS={game_start,_,_,_}}, S=#pl{socket = Socket}) ->
    send_term(Socket, GS),
    {noreply, S#pl{st = {in_game, GamePid}}};
handle_cast(Event, S=#pl{socket = Socket}) ->
    send_term(Socket, Event),
    {noreply, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info({tcp_closed, _Socket}, S) ->
    cc_lobby:leave(),
    {stop, normal, S#pl{st = disconnected}};
handle_info({tcp, Socket, Str}, S) ->
    case read_term(Str) of
        E={error, invalid_term} ->
            send_term(Socket, E),
            {noreply, S};
        leave ->
            cc_lobby:leave(),
            {stop, normal, S#pl{st = disconnected}};
        list_games ->
            send_term(Socket, cc_lobby:list_games()),
            {noreply, S};
        list_players ->
            send_term(Socket, cc_lobby:list_players()),
            {noreply, S};
        Term ->
            case S of
                #pl{st = disconnected} ->
                    case Term of
                        {login, PlayerName} ->
                            cc_lobby:login(PlayerName),
                            send_term(Socket, ok),
                            {noreply, S#pl{st = logged_in}};
                        _OtherWise -> 
                            send_term(Socket, {error, log_in}),
                            {noreply, S}
                    end;
                #pl{st = logged_in} ->
                    case Term of
                        {host_game, GameName} ->
                            case cc_lobby:host_game(GameName) of
                                ok ->
                                    send_term(Socket, ok),
                                    {noreply, S#pl{st = in_game}};
                                E -> 
                                    send_term(Socket, E),
                                    {noreply, S}
                            end;
                        {join_game, GameName} ->
                            case cc_lobby:join_game(GameName) of
                                waiting_for_players ->
                                    send_term(Socket, ok),
                                    {noreply, S#pl{st = in_game}};
                                E ->
                                    send_term(Socket, E),
                                    {noreply, S}
                            end;
                        {spectate, GameName} ->
                            case cc_lobby:spectate_game(GameName) of
                                ok ->
                                    send_term(Socket, ok),
                                    {noreply, S};
                                E ->
                                    send_term(socket, E),
                                    {noreply, S}
                            end;
                        _OtherWise -> 
                            send_term(Socket, {error, wrong_command}),
                            {noreply, S}
                    end;
                #pl{st = in_game} ->
                    case Term of
                        start_game ->
                            case cc_lobby:start_game() of
                                ok ->
                                    send_term(Socket, ok),
                                    {noreply, S};
                                E -> 
                                    send_term(Socket, E),
                                    {noreply, S}
                            end;
                        _Otherwise ->
                            send_term(Socket, {error, wrong_command}),
                            {noreply, S}
                    end;
                #pl{st = {in_game, GamePid}} ->
                    case Term of
                        {move, Move} ->
                            case cc_game:move(GamePid, Move) of
                                ok -> {noreply, S};
                                E -> send_term(Socket, E),
                                     {noreply, S}
                            end;
                        _Otherwise -> 
                            send_term(Socket, {error, wrong_command}),
                            {noreply, S}
                    end;
                _Otherwise ->
                    send_term(Socket, {error, wrong_command}),
                    {noreply, S}
            end
    end.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #pl{socket = Socket}) ->
    gen_tcp:close(Socket),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

read_term(String) ->
    Length = length(String),
    TermStr = lists:sublist(String, Length - 1),
    case erl_scan:string(TermStr) of
        {ok, Tokens, _End} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term}  ->
                    Term;
                _ -> {error, invalid_term}
            end;
        _ -> {error, invalid_term}
    end.                                                      

send_term(Socket, Term) ->
    gen_tcp:send(Socket, io_lib:format("~p~n", [Term])).
