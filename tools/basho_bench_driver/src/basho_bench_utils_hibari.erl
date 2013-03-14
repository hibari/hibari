%%%-------------------------------------------------------------------
%%% Copyright (c) 2012-2013 Hibari developers.  All rights reserved.
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

-module(basho_bench_utils_hibari).

-export([start_apps/0,
         add_client_monitors/1,
         wait_for_table/2,
         go_async/2,
         go_sync/2,
         checkpoint/2
        ]).

-export([start_net_kernel/1,
         set_cookie/2,
         ping/1
        ]).


%% ====================================================================
%% Types / Records / Macros
%% ====================================================================

-type admin_node() :: node().
-type brick_node() :: node().
-type table() :: atom().
-type key() :: binary().

-define(RPC_TIMEOUT, 15000).
-define(POLLING_INTERVAL_MILIS, 250).
-define(POLLING_MAX_RETRY, 40).        %% 250 ms * 40 = 10 secs


%% ====================================================================
%% API
%% ====================================================================

-spec start_apps() -> ok | {error, term()}.
start_apps() ->
    case start_app(sasl) of
        ok ->
            case start_app(gmt_util) of
                ok ->
                    case start_app(gdss_client) of
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
    end.

-spec add_client_monitors([admin_node()]) -> ok.
add_client_monitors(HibariAdmins) ->
    [ {ok, _TS} = add_client_monitor(HibariAdmin, node()) || HibariAdmin <- HibariAdmins],
    ok.

-spec wait_for_table(admin_node(), table()) ->
                            ok | {error, table_not_available | brick_not_available}.
wait_for_table(HibariAdmin, Table) ->
    case gmt_loop:do_while(fun poll_table/1, {HibariAdmin, Table, ?POLLING_MAX_RETRY}) of
        ok ->
            case gmt_loop:do_while(fun poll_brick/1, {Table, <<"k">>, ?POLLING_MAX_RETRY}) of
                ok ->
                    ok;
                Err1 ->
                    {error, Err1}
            end;
        Err2 ->
            {error, Err2}
    end.

-spec go_async(admin_node(), table()) -> ok | {error, term()}.
go_async(HibariAdmin, Table) ->
    go_sync(HibariAdmin, Table, false).

-spec go_sync(admin_node(), table()) -> ok | {error, term()}.
go_sync(HibariAdmin, Table) ->
    go_sync(HibariAdmin, Table, true).

-spec checkpoint(admin_node(), table()) -> [term()].
checkpoint(HibariAdmin, Table) ->
    [rpc(Node, brick_server, checkpoint, [Brick, Node])
     || {Brick, Node} <- running_bricks(HibariAdmin, Table)].

-spec start_net_kernel(node()) -> ok | {error, term()}.
start_net_kernel(Node) ->
    case net_kernel:start(Node) of
        {ok, _} ->
            ok;
        {error, {already_started, _}} ->
            ok;
        {error, {{already_started, _}, _}} ->
            ok;
        {error, _}=Err ->
            Err
    end.

-spec set_cookie([brick_node()], atom()) -> ok.
set_cookie(HibariNodes, Cookie) ->
    [ erlang:set_cookie(Node, Cookie) || Node <- HibariNodes ],
    ok.

-spec ping(node()) -> ok | {error, string()}.
ping(Node) ->
    case net_adm:ping(Node) of
        pong ->
            ok;
        pang ->
            Message = io_lib:format("Failed to ping node ~p", [Node]),
            {error, lists:flatten(Message)}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

-spec add_client_monitor(admin_node(), node()) -> {ok, integer()} | {error, term()}.
add_client_monitor(HibariAdmin, ClientNode) ->
    rpc(HibariAdmin, brick_admin, add_client_monitor, [ClientNode]).

-spec poll_table({admin_node(), table(), non_neg_integer()}) ->
                        {false, ok | table_not_available}
                            | {true, {admin_node(), table(), non_neg_integer()}}.
poll_table({_, _, 0}) ->
    {false, table_not_available};
poll_table({HibariAdmin, Table, N}) when N > 0 ->
    TabCh = gmt_util:atom_ify(gmt_util:list_ify(Table) ++ "_ch1"),
    case rpc(HibariAdmin, brick_sb, get_status, [chain, TabCh]) of
        {ok, healthy} ->
            {false, ok};
        _ ->
            ok = timer:sleep(?POLLING_INTERVAL_MILIS),
            {true, {HibariAdmin, Table, N - 1}}
    end.

-spec poll_brick({table(), key(), non_neg_integer()}) ->
                        {false, ok | brick_not_available}
                            | {true, {table(), key(), non_neg_integer()}}.
poll_brick({_, _, 0}) ->
    {false, brick_not_available};
poll_brick({Table, Key, N}) when N > 0 ->
    case brick_simple_client:find_the_brick(Table, Key, write) of
        brick_not_available ->
            ok = timer:sleep(?POLLING_INTERVAL_MILIS),
            {true, {Table, Key, N - 1}};
        _ ->
            {false, ok}
    end.

-spec go_sync(admin_node(), table(), boolean()) -> ok | {error, term()}.
go_sync(HibariAdmin, Table, Bool) ->
    [ rpc(Node, brick_server, set_do_sync, [{Brick, Node}, Bool])
      || {Brick, Node} <- running_bricks(HibariAdmin, Table) ],
    SyncStatus = get_sync_properties(HibariAdmin, Table),
    case lists:all(fun(Sync) -> Sync =:= Bool end, SyncStatus) of
        true ->
            ok;
        false ->
            {error, SyncStatus}
    end.

-spec get_sync_properties(admin_node(), table()) -> [boolean()].
get_sync_properties(HibariAdmin, Table) ->
    BrickStatusList = [rpc(Node, brick_server, status, [Brick, Node])
                       || {Brick, Node} <- running_bricks(HibariAdmin, Table)],
    lists:map(fun({ok, BrickStatus}) ->
                      BrickImpl = proplists:get_value(implementation, BrickStatus, undefined),
                      proplists:get_value(do_sync, BrickImpl, undefined)
              end,
              BrickStatusList).

-spec running_bricks(admin_node(), table()) -> [brick_node()].
running_bricks(HibariAdmin, Table) ->
    {ok, Properties} = rpc(HibariAdmin, brick_admin, get_table_info,
                        [{global, brick_admin}, Table]),
    GHash = proplists:get_value(ghash, Properties),
    Chains = lists:usort(brick_hash:all_chains(GHash, current)
                         ++ brick_hash:all_chains(GHash, new)),
    [Brick || {_Chain, Bricks} <- Chains, Brick <- Bricks].


-spec start_app(atom()) -> ok | {error, term()}.
start_app(App) when is_atom(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        {error, _}=Err ->
            Err
    end.

-spec rpc(node(), module(), function(), list()) -> term().
rpc(Node, M, F, A) ->
    case rpc:call(Node, M, F, A, ?RPC_TIMEOUT) of
        {badrpc, Reason} ->
            Message = io_lib:format("RPC(~p:~p:~p) to ~p failed: ~p", [M, F, A, Node, Reason]),
            error({rpc_failure, lists:flatten(Message)});
        Reply ->
            Reply
    end.
