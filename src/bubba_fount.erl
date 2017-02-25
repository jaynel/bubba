%%%------------------------------------------------------------------------------
%%% @copyright (c) 2017, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Benchmark server for invoking experimental performance tests.
%%% @since v0.0.1
%%% @end
%%%------------------------------------------------------------------------------
-module(bubba_fount).

-copyright("(c) 2017, DuoMark International, Inc.  All rights reserved").
-author('Jay Nelson <jay@duomark.com>').
-license('New BSD').

-behaviour(cxy_fount).

%%% Behaviour API
-export([init/1, start_pid/2, send_msg/2]).

%%% Internally spawned functions
-export([benchmark/2]).

-include("bubba.hrl").

-record(bf_state, {}).
-type bf_state() :: #bf_state{}.


%%%===================================================================
%%% Behaviour API
%%%===================================================================

-type task_mgr()    :: cxy_fount:fount_ref().
-type task_worker() :: pid().
-type task_msg()    :: any().

-spec init      ({}) -> bf_state().
-spec start_pid (task_mgr(), bf_state()) -> task_worker() | {error, any()}.
-spec send_msg  (Worker, task_msg()) -> Worker | {error, any()}
                                            when Worker :: task_worker().

init      ({})           -> #bf_state{}.
start_pid (Fount, State) -> cxy_fount:spawn_worker(Fount, ?MODULE, benchmark, [Fount, State]).
send_msg  (Worker,  Msg) -> cxy_fount:send_msg(Worker, Msg).


%%%===================================================================
%%% Internal support functions
%%%===================================================================

-spec benchmark(cxy_fount:fount_ref(), bf_state()) -> true.

%%% Reservoir process waits to be tasked to run a benchmark.
%%% Each benchmark is independently run in a single process.
%%% A benchmark is a continuously looping execution for a default
%%% time of 30 seconds, which reports operations_per_second
%%% approximately once every second (it adjusts next iteration
%%% count based on previous iteration elapsed time).

snap_time() ->
    erlang:monotonic_time(microsecond).

benchmark(_Fount, #bf_state{}) ->
    receive
        {access, Type, Datasize, Loop_Count} -> loop_test_access(Type, Datasize, Loop_Count)
    end.

loop_test_access(Type, Datasize, Loop_Count) ->
    Args = access_arglist(Type, Datasize, Loop_Count),
    error_logger:info_msg("Access: ~p~n", [Type]),
    loop_test_access(Type, Args, snap_time(), bubba_env:get_bench_iters()).

loop_test_access(Type, Args, Start_Time, Iters) ->
    Dur       = bubba_env:get_bench_duration()*1000,   % If config changes while executing
    Duration  = Dur - 500000,
    Iter_Time = snap_time(),

    %% Quit when within 1/2 second of Duration.
    (Iter_Time - Start_Time) > Duration
        orelse begin
                   Sum = case test_access(Type, Args) of
                             [_|_] = L -> lists:sum(L);
                             Result    -> Result
                         end,
                   Elapsed = max(snap_time() - Iter_Time, 1),
                   error_logger:info_msg("  Timing: ~px [~p msecs] ~p", [Iters, Elapsed, Sum]),
                   Next_Iter_Count = adjust_iterations(Elapsed, Iters),
                   loop_test_access(Type, Args, Start_Time, Next_Iter_Count)
               end.

%%% Try to run loops with approximately 1 second of execution on each iteration.
%%% Don't run less than 10 operations, or more than 1M, in one iteration.
adjust_iterations(Elapsed, Iters) ->
    case Elapsed of
        A_Second  when A_Second >= 800000, A_Second =< 1200000 -> Iters;
        Too_Long  when Too_Long > 1200000 -> max(round(Iters * 1000000 / Elapsed),           10);
        Too_Short when Too_Short < 800000 -> min(round(Iters * 1000000 / Elapsed), 100000000000)
    end.

    
%% ==== Utilities for data access tests ==========================

%% Accessing data structures
access_arglist(tuple_inx,  DataSize, LC) -> make_tuple_args (DataSize, LC);
access_arglist(list_head,  DataSize, LC) -> make_list_args  (DataSize, LC);
access_arglist(list_h_t,   DataSize, LC) -> make_list_args  (DataSize, LC);
access_arglist(list_nth,   DataSize, LC) -> make_list_args  (DataSize, LC);
access_arglist(binary_at,  DataSize, LC) -> make_bin_args   (DataSize, LC);
access_arglist(binary_raw, DataSize, LC) ->
    {Inxs, Bin} = make_bin_args(DataSize, LC),
    [[X*8 || X <- Inxs], Bin].

make_list_args(ListSize, LoopCount) ->
    List = lists:seq(1, ListSize),
    RepeatTimes = case ListSize > LoopCount of
                      true  -> LoopCount;
                      false -> ListSize - 1
                  end,
    {RepeatTimes, List}.

make_bin_args(BinSize, LoopCount) ->    
    Bin  = make_bin(BinSize),
    Inxs = make_random_inxs(LoopCount, BinSize),
    {Inxs, Bin}.

make_tuple_args(TupleSize, LoopCount) ->
    Tuple = make_tuple(TupleSize),
    Inxs  = make_random_inxs(LoopCount, TupleSize),
    {Inxs, Tuple}.

make_bin   (Size) -> list_to_binary (lists:seq(1, Size)).
make_tuple (Size) -> list_to_tuple  (lists:seq(1, Size)).

%% Random indexes are in the range 1 - N-1 so that bin and tuple work.
make_random_inxs(Num, MaxInx) ->
    make_random_inxs(Num, MaxInx, []).

make_random_inxs(0, _MaxInx, Inxs)            -> Inxs;
make_random_inxs(N,  MaxInx, Inxs) when N > 0 -> 
    make_random_inxs(N-1, MaxInx, [rand:uniform(MaxInx) | Inxs]).

%%% All functions are 1-based, except binary_at/binary_raw so these two decrement index by 1.
-spec test_access(list_nth,   { pos_integer(),       [integer()]}) ->  integer();
                 (list_head,  { pos_integer(),       [integer()]}) ->  integer();
                 (tuple_inx,  {[pos_integer(), ...], {integer()}}) -> [integer()];
                 (binary_at,  {[pos_integer(), ...],  binary()})   -> [integer()];
                 (binary_raw, {[pos_integer(), ...],  binary()})   -> [integer()].

test_access(list_nth,   {N,  List })            ->  lists:nth(N, List);
test_access(list_head,  {0, [ H  ]})            ->  H;
test_access(list_head,  {N, [_H|T]}) when N > 0 ->  test_access(list_head, {N-1, T});
test_access(tuple_inx,  {Positions, Tuple})     -> [element(P, Tuple)   || P <- Positions];
test_access(binary_at,  {Positions,   Bin})     -> [binary:at(Bin, P-1) || P <- Positions];
test_access(binary_raw, {Positions,   Bin})     ->
    [begin << _B:P, C, _Rest/binary >> = Bin, C end || P <- Positions].

