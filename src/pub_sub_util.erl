%%%-------------------------------------------------------------------
%%% @author natesh
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. May 2021 3:30 PM
%%%-------------------------------------------------------------------
-module(pub_sub_util).
-author("natesh").

%% API
-export([to_lower_case/1]).


%%--------------------------------------------------------------------------------------
-spec to_lower_case(Data::string() | binary()) -> ConvertedData::binary().
%%--------------------------------------------------------------------------------------
to_lower_case(Data) when is_binary(Data) -> list_to_binary(string:lowercase(binary_to_list(Data)));
to_lower_case(Data) when is_list(Data) -> list_to_binary(string:lowercase(Data)).