%% ====================================================================
%% @author Gavin M. Roy <gavinmroy@gmail.com>
%% @copyright 2016, Gavin M. Roy
%% @doc
%% RabbitMQ Management DB Stress Test Tool
%%
%% Runs a test for the specified duration where it consistently opens
%% and closes channels, publishing a random amount of messages per
%% channel.
%%
%% @end
%% ====================================================================
-module(mgmt_stress).

-export([main/1]).

%% escript Entry point
main(Args) ->
  setup_logging(),
  OptionSpec = option_spec(),
  case getopt:parse(OptionSpec, Args) of
    {ok, {Settings, _Values}} ->
      case display_help(Settings) of
        true ->
          getopt:usage(OptionSpec, atom_to_list(?MODULE));
        false ->
          run(Settings)
      end;
    {error, Reason} ->
      io:format("Error: ~p~n", [Reason]),
      getopt:usage(OptionSpec, atom_to_list(?MODULE))
  end,
  erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

display_help([]) -> false;
display_help([help|_]) -> true;
display_help([_|T]) -> display_help(T).


option_spec() ->
  Host = option_default_host(),
  Port = option_default_port(),
  [
    {duration,    $d,        "duration",    {integer, 300},    "Number of seconds to run test"},
    {host,        $h,        "host",        {string, Host},    "RabbitMQ host"},
    {port,        $p,        "port",        {integer, Port},   "RabbitMQ port"},
    {vhost,       $v,        "vhost",       {string, "/"},     "RabbitMQ Virtual Host"},
    {user,        $u,        "user",        {string, "guest"}, "RabbitMQ User"},
    {password,    $P,        "password",    {string, "guest"}, "RabbitMQ password"},
    {connections, $C,        "connections", {integer, 150},    "Max Connections"},
    {channels,    $c,        "channels",    {integer, 2},      "Max Channels"},
    {messages,    $m,        "messages",    {integer, 10},     "Max Messages"},
    {help,        undefined, "help",        undefined,         "Print usage"}
  ].


option_default_host() ->
  option_default_host(os:getenv("RABBITMQ_HOST")).
option_default_host(false) -> "localhost";
option_default_host(Value) -> Value.


option_default_port() ->
  option_default_port(os:getenv("RABBITMQ_PORT")).
option_default_port(false) -> 5672;
option_default_port(Value) -> list_to_integer(Value).


random_seed() ->
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed({A,B,C}).


run(Settings) ->
  random_seed(),
  Duration = proplists:get_value(duration, Settings),
  lager:info("Starting Stress Test, Duration: ~p seconds", [Duration]),
  [application:set_env(mgmt_stress, K, V) || {K, V} <- Settings],
  application:ensure_all_started(mgmt_stress),
  timer:sleep(Duration * 1000),
  lager:info("Test complete"),
  application:stop(mgmt_stress).


setup_logging() ->
  application:set_env(lager, handlers, [{lager_console_backend, info}]),
  ErrorLogger = [
    {error_logger_lager_event,
      [
        {handlers, [{lager_file_backend, [{file, "error_logger.log"}, {level, error}]}]},
        {async_threshold, 500},
        {async_threshold_window, 250}
      ]
    }
  ],
  application:set_env(lager, extra_sinks, ErrorLogger),
  application:ensure_all_started(lager).
