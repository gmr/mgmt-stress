%% ====================================================================
%% @author Gavin M. Roy <gavinmroy@gmail.com>
%% @copyright 2016, Gavin M. Roy
%% @doc
%% @end
%% ====================================================================
-module(mgmt_stress_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Settings) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Settings).

init(Settings) ->
  Children = create_specs(Settings, proplists:get_value(connections, Settings),
                          []),
  {ok, {{one_for_one, 5, 10}, Children}}.

create_specs(_, Qty, Specs) when length(Specs) =:= Qty -> Specs;
create_specs(Settings, Qty, Specs) ->
  ID = list_to_atom(lists:flatten(io_lib:format("mgmt_stress_worker~p",
                                                [length(Specs)]))),
  Spec = {
    ID,
    {mgmt_stress_worker, start_link, [{ID, Settings}]},
     permanent, 1000, worker, [mgmt_stress_worker]
  },
  create_specs(Settings, Qty, lists:append(Specs, [Spec])).

