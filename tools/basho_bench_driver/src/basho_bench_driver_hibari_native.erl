%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2013 Hibari developers.  All rights reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%-------------------------------------------------------------------

-module(basho_bench_driver_hibari_native).

-export([new/1,
         run/4,
         terminate/2]).


%% ====================================================================
%% Types / Records / Macros
%% ====================================================================

-type operation() :: get | get_witness | add | replace | set | rename | delete.
-type key_gen() :: fun(() -> binary()).
-type value_gen() :: fun(() -> binary()).
-type table() :: atom().
-type key() :: binary().

-record(state, {
          id :: non_neg_integer(),
          table :: table(),
          key_prefix :: binary(),
          verify_read :: boolean()
         }).
-type state() :: #state{}.

-define(UTILS, basho_bench_utils_hibari).
-define(PREFIX_SEPARATOR, "/").
-define(MD5_TAG, md5).
-define(MD5_SKIP, skip).


%% ====================================================================
%% API - Callback Functions
%% ====================================================================

-spec new(non_neg_integer()) -> {ok, state()}.
new(Id) ->
    Table  = basho_bench_config:get(hibari_table, perf1),
    VerifyRead = basho_bench_config:get(verify_read, false),
    KeyPrefix = list_to_binary([integer_to_list(Id), ?PREFIX_SEPARATOR]),
    InitialState = #state{id=Id, table=Table,
                          key_prefix=KeyPrefix, verify_read=VerifyRead},
    if
        Id =:= 1 ->
            case init(Table) of
                ok ->
                    {ok, InitialState};
                {error, _}=Err ->
                    Err
            end;
        true ->
            {ok, InitialState}
    end.


-spec run(operation(), key_gen(), value_gen(), state()) ->
                 {ok, state()} | {error, term(), state()}.

%%
%% get
%%
run(get, KeyGen, _ValGen, #state{table=Table, verify_read=true}=State) ->
    case brick_simple:get(Table, prefixed_key(KeyGen, State), [get_all_attribs]) of
        {ok, _TS, Val, _Exp, Props} ->
            Md5Hash  = crypto:md5(Val),
            case proplists:get_value(?MD5_TAG, Props) of
                Md5Hash ->
                    {ok, State};
                ?MD5_SKIP ->
                    {ok, State};
                undefined ->
                    {error, md5_undefined, State};
                _ ->
                    {error, md5_mismatch, State}
            end;
        key_not_exist ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;
run(get, KeyGen, _ValGen, #state{table=Table}=State) ->
    case brick_simple:get(Table, prefixed_key(KeyGen, State)) of
        {ok, _TS, _Val} ->
            {ok, State};
        key_not_exist ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;
%%
%% get_witness
%%
run(get_witness, KeyGen, _ValGen, #state{table=Table}=State) ->
    case brick_simple:get(Table, prefixed_key(KeyGen, State), [witness]) of
        {ok, _TS} ->
            {ok, State};
        key_not_exist ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;
%%
%% add
%%
run(add, KeyGen, ValGen, #state{table=Table, verify_read=true}=State) ->
    Val = ValGen(),
    Md5Hash = crypto:md5(Val),
    case brick_simple:add(Table, prefixed_key(KeyGen, State), Val, [{?MD5_TAG, Md5Hash}]) of
        {ok, _TS}->
            {ok, State};
        {key_exists, _} ->
            {ok, State};
        Reason ->
            {error, Reason, State}
    end;
run(add, KeyGen, ValGen, #state{table=Table}=State) ->
    case brick_simple:add(Table, prefixed_key(KeyGen, State), ValGen()) of
        {ok, _TS}->
            {ok, State};
        {key_exists, _} ->
            {ok, State};
        Reason ->
            {error, Reason, State}
    end;
%%
%% replace
%%
run(replace, KeyGen, ValGen, #state{table=Table, verify_read=true}=State) ->
    Val = ValGen(),
    Md5Hash = crypto:md5(Val),
    case brick_simple:replace(Table, prefixed_key(KeyGen, State), Val, [{?MD5_TAG, Md5Hash}]) of
        {ok, _TS}->
            {ok, State};
        key_not_exist ->
            {ok, State};
        Reason ->
            {error, Reason, State}
    end;
run(replace, KeyGen, ValGen, #state{table=Table}=State) ->
    case brick_simple:replace(Table, prefixed_key(KeyGen, State), ValGen()) of
        {ok, _TS}->
            {ok, State};
        key_not_exist ->
            {ok, State};
        Reason ->
            {error, Reason, State}
    end;
%%
%% set
%%
run(set, KeyGen, ValGen, #state{table=Table, verify_read=true}=State) ->
    Val = ValGen(),
    Md5Hash = crypto:md5(Val),
    case brick_simple:set(Table, prefixed_key(KeyGen, State), Val, [{?MD5_TAG, Md5Hash}]) of
        {ok, _TS}->
            {ok, State};
        Reason ->
            {error, Reason, State}
    end;
run(set, KeyGen, ValGen, #state{table=Table}=State) ->
    case brick_simple:set(Table, prefixed_key(KeyGen, State), ValGen()) of
        {ok, _TS}->
            {ok, State};
        Reason ->
            {error, Reason, State}
    end;
%%
%% rename
%%
run(rename, KeyGen, _ValGen, #state{table=Table}=State) ->
    OldKey = prefixed_key(KeyGen, State),
    NewKey = prefixed_key(KeyGen, State),
    case brick_simple:rename(Table, OldKey, NewKey, [{?MD5_TAG, ?MD5_SKIP}]) of
        {ok, _TS}->
            {ok, State};
        key_not_exist ->
            {ok, State};
        {txn_fail, [{_, brick_not_available}]} ->
            {error, brick_not_available, State};
        Reason ->
            {error, Reason, State}
    end;
%%
%% delete
%%
run(delete, KeyGen, _ValGen, #state{table=Table}=State) ->
    case brick_simple:delete(Table, prefixed_key(KeyGen, State)) of
        ok ->
            {ok, State};
        key_not_exist ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end.

-spec terminate(term(), state()) -> ok.
terminate({'EXIT', Reason}, #state{id=Id}) ->
    lager:warn("Worker ~p crashed: ~p~n", [Id, Reason]),
    ok;
terminate(normal, _) ->
    ok.


%% ====================================================================
%% Internal Functions
%% ====================================================================

-spec init(table()) -> ok | {error, term()}.
init(Table) ->
    MyNode  = basho_bench_config:get(hibari_mynode, [basho_bench, shortnames]),
    ok = ?UTILS:start_net_kernel(MyNode),

    HibariAdminNodes = basho_bench_config:get(hibari_admin_nodes, ['hibari@127.0.0.1']),
    HibariBrickNodes = basho_bench_config:get(hibari_brick_nodes, ['hibari@127.0.0.1']),

    Cookie  = basho_bench_config:get(hibari_cookie, 'hibari'),
    case ?UTILS:set_cookie(HibariBrickNodes, Cookie) of
        ok ->
            case ?UTILS:start_apps() of
                ok ->
                    case ?UTILS:add_client_monitors(HibariAdminNodes) of
                        ok ->
                            case ?UTILS:wait_for_table(hd(HibariBrickNodes), Table) of
                                ok ->
                                    ok;
                                Err ->
                                    Err
                            end;
                        Err ->
                            Err
                    end;
                Err ->
                    Err
            end;
        Err ->
            Err
    end.

-spec prefixed_key(key_gen(), state()) -> key().
prefixed_key(KeyGen, #state{key_prefix=Prefix}) ->
    list_to_binary([Prefix, KeyGen()]).
