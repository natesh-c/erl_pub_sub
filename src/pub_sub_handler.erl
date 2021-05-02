%%%-------------------------------------------------------------------
%%% @author natesh
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. May 2021 7:31 PM
%%%-------------------------------------------------------------------
-module(pub_sub_handler).
-author("natesh").

-record(user_state,{name, status, pid}).

%% For ETS
-record(user,{name, subscribed_topics = [], status, pid}).
-record(topic,{name, subscribed_users = []}).

%% API
-export([init/3, handle_request/2, handle_terminate/1]).

init(Transport, _Req, _State) ->
  io:format("Initializing new user session ~n"),
  io:format("Transport: ~p~n",[Transport]),
  #user{}.


handle_request(<<"connect ", UserName/binary>>, State) -> connect(UserName, State);
handle_request(<<"subscribe ", Topic/binary>>, State) -> subscribe(Topic, State);
handle_request(<<"unsubscribe ", Topic/binary>>, State) -> unsubscribe(Topic, State);
handle_request(<<"publish ", TopicMessage/binary>>, State) -> publish(TopicMessage, State);
handle_request(<<"list_subscription">>, #user_state{name = UserName, status = Status}=State) -> ok;
handle_request(<<"list_all_topics">>, #user_state{name = UserName, status = Status}=State) -> ok;
handle_request(<<"disconnect">>, #user_state{name = UserName, status = Status}=State) -> ok;
handle_request(UnknownAction, State) ->
  io:format("Unknown event: ~p~nState is:~p~n",[UnknownAction, State]),
  {<<"unknown_action">>, State}.

handle_terminate(#user_state{name = UserName, status = Status}=State) -> ok;
handle_terminate(State) ->
  io:format("State is: ~p~n",[State]),
  State.

connect(ReqName, State) ->
  UserName = pub_sub_util:to_lower_case(ReqName),
  UserRec = #user{name = UserName, status = 1},
  case ets:insert_new(pub_sub_users ,UserRec) of
    false -> {<<"user already exists with this name">>, State};
    true -> {<<"user ", ReqName/binary, " has been added successfully">>, #user_state{name = UserName, status = 1}}
  end.

subscribe(ReqTopic, #user_state{name = UserName} = State) ->
  Topic = pub_sub_util:to_lower_case(ReqTopic),
  case ets:lookup(pub_sub_users, UserName) of
    [#user{name = UserName, subscribed_topics = Subscription}] ->
      case lists:member(Topic, Subscription) of
        true -> {<<"The topic ", ReqTopic/binary, " has been already subscribed by the user">>, State};
        false ->
          NewTopic = [Topic|Subscription],
          ets:update_element(pub_sub_users,UserName,[{#user.subscribed_topics, NewTopic}]),
          case ets:lookup(pub_sub_topics, Topic) of
            [#topic{name = Topic, subscribed_users = UserList}] ->
              ets:update_element(pub_sub_topics,Topic,[{#topic.subscribed_users, [UserName|UserList]}]);
            [] ->
              ets:insert(pub_sub_topics, #topic{name = Topic, subscribed_users = [UserName]})
          end,
          {<<"You have successfully subscribed the topic ", ReqTopic/binary>>, State}
      end
  end.

unsubscribe(ReqTopic, #user_state{name = UserName} = State) ->
  Topic = pub_sub_util:to_lower_case(ReqTopic),
  case ets:lookup(pub_sub_users, UserName) of
    [] -> {<<"You have not subscibed to any topic">>, State};
    [#user{name = UserName, subscribed_topics = Subscription}] ->
      case lists:member(Topic, Subscription) of
        false -> {<<"You have not subscibed to the topic ", ReqTopic/binary>>, State};
        true ->
          NewTopic = lists:delete(Topic, Subscription),
          ets:update_element(pub_sub_users,UserName,[{#user.subscribed_topics, NewTopic}]),
          [#topic{name = Topic, subscribed_users = UserList}] = ets:lookup(pub_sub_topics, Topic),
          NewUserList = lists:delete(UserName, UserList),
          ets:update_element(pub_sub_topics,Topic,[{#topic.subscribed_users, NewUserList}]);
          {<<"You have successfully unsubscribed from the topic ", ReqTopic/binary>>, State}
      end
  end.

publish(TopicAndMessage, State) ->
  [ReqTopic, Message] = binary:split(TopicAndMessage, <<" ">>),
  Topic = pub_sub_util:to_lower_case(ReqTopic),
  case ets:lookup(pub_sub_topics, Topic) of
    [#topic{name = Topic, subscribed_users = UserList}] ->
      ok;
    [] -> ok
  end.
