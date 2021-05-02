%%%-------------------------------------------------------------------
%% @doc erl_pub_sub public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_pub_sub_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Starting Pubsub ws  ~n"),
    Dispatch = cowboy_router:compile([
        {'_',[
            {"/ws/[...]",ws_req_handler,[]}
        ]}
    ]),
    Res = cowboy:start_http(websocket,10,[{port,8585}],
        [{env,[{dispatch,Dispatch}]}]),
    case Res of
        {ok, _Pid} ->
            ets:new(pub_sub_users, [ordered_set,named_table, public, {keypos,2}]),
            ets:new(pub_sub_topics, [ordered_set,named_table, public, {keypos,2}]),
            erl_pub_sub_sup:start_link();
        {error,Reason} -> {error,Reason};
        Error -> {error,Error}
    end.


stop(_State) ->
    ok.

%% internal functions
