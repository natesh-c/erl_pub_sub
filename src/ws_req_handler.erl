%%%-------------------------------------------------------------------
%%% @author natesh
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. May 2021 2:40 PM
%%%-------------------------------------------------------------------
-module(ws_req_handler).
-author("natesh").

%% API
-export([init/3,websocket_init/3,websocket_handle/3,websocket_info/3,websocket_terminate/3]).

-record(state,{user_state}).

init(Transport,Req,_State) ->
  io:format("Received WS trans: ~p~nReq: ~p~n",[Transport,Req]),
  {upgrade,protocol,cowboy_websocket}.

websocket_init(Transport,Req,State) ->
  NewState = pub_sub_handler:init(Transport,Req,State),
  {ok,Req,#state{user_state = NewState}}.


websocket_handle({ping, <<>>}, Req,State) ->
  io:format("Received in ping:~n"),
  {ok,Req,State};

websocket_handle({pong, <<>>}, Req,State) ->
  io:format("Received in pong:~n"),
  {ok,Req,State};

websocket_handle({text, Msg}, Req, #state{user_state = UserState}=State) ->
  io:format("Received in text: ~p~nPid: ~p~n",[Msg, self()]),
  {Response, NewState} = pub_sub_handler:handle_request(Msg, UserState),
  {reply,{text, Response},Req,State#state{user_state = NewState}};

websocket_handle({binary, Msg}, Req, #state{user_state = UserState}=State) ->
  io:format("Received in binary: ~p~nPid: ~p~n",[Msg, self()]),
  {Response, NewState} = pub_sub_handler:handle_request(Msg, UserState),
  {reply,{binary, Response},Req,State#state{user_state = NewState}};

websocket_handle(Data,Req,State) ->
  io:format("Received invalid content: ~p~n",[Data]),
  {reply,{text, <<"Invalid data">>},Req,State}.


websocket_info({error, Reason}, Req,State) ->
  {reply,{text, Reason},Req,State};

websocket_info(Msg, Req, State) ->
  io:format("SB info: ~p~n",[Msg]),
  {reply,{text, <<"Got info ", Msg/binary>>},Req,State}.

websocket_terminate({error,closed}, Req, #state{user_state = UserState}=State) ->
  io:format("Conxn error ~n"),
  NewState = pub_sub_handler:handle_terminate(UserState),
  {shutdown,Req,NewState};

websocket_terminate(Reason, Req, #state{user_state = UserState}=State) ->
  io:format("Conxn terminate ~p~n",[Reason]),
  NewState = pub_sub_handler:handle_terminate(UserState),
  {shutdown,Req,NewState}.




