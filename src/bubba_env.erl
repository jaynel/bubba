%%%------------------------------------------------------------------------------
%%% @copyright (c) 2017, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Environment configuration variables, plus access functions with
%%%   default values supplied. The file rel/sys.config contains the
%%%   actual runtime configured values for production.
%%%
%%% @since v0.0.9
%%% @end
%%%------------------------------------------------------------------------------
-module(bubba_env).

-copyright("(c) 2017, DuoMark International, Inc.  All rights reserved").
-author('Jay Nelson <jay@duomark.com>').
-license('New BSD').

%% External API
-export([
         get_ipv4/0,        get_port/0,
         get_bench_iters/0, get_bench_duration/0
        ]).


%%%===================================================================
%%% API functions
%%%===================================================================
-type ipv4_elem() :: 0..255.

-spec get_ipv4           () -> {ipv4_elem(), ipv4_elem(), ipv4_elem(), ipv4_elem()}.
-spec get_port           () -> 1..65535.

get_ipv4 () -> get_app_env(ipv4, {127,0,0,1}).
get_port () -> get_app_env(port,        9998).

-spec get_bench_iters    () -> pos_integer().
-spec get_bench_duration () -> pos_integer().

get_bench_iters    () -> get_app_env(bench_iters,      100).
get_bench_duration () -> get_app_env(bench_duration, 30000).
     

%%%===================================================================
%%% Support functions
%%%===================================================================
-spec get_app_env(atom(), any()) -> any().

get_app_env(Param, Default) ->
    case application:get_env(bubba, Param) of
        {ok, Val} -> Val;
        undefined ->
            case init:get_argument(Param) of
                {ok, [[FirstVal | _OtherVals] | _MoreVals]} -> FirstVal;
                error -> Default
            end
    end.
