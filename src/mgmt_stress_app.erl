%% ====================================================================
%% @author Gavin M. Roy <gavinmroy@gmail.com>
%% @copyright 2016, Gavin M. Roy
%% @doc
%% @end
%% ====================================================================
-module(mgmt_stress_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  mgmt_stress_sup:start_link(application:get_all_env(mgmt_stress)).

stop(_State) ->
  ok.
