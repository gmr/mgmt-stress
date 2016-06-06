%% ====================================================================
%% @author Gavin M. Roy <gavinmroy@gmail.com>
%% @copyright 2016, Gavin M. Roy
%% @doc
%%
%% @end
%% ====================================================================

-module(mgmt_stress_worker).

-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {connection, channel, id, settings, timer}).

-define(LORUM_IPSUM, <<"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec laoreet pharetra magna. Pellentesque molestie ipsum ipsum, sed condimentum nulla malesuada nec. Donec pharetra auctor metus in interdum. Nullam ornare justo vel ante convallis, at dictum metus pharetra. Vestibulum rutrum posuere luctus. Pellentesque ac scelerisque ante. Pellentesque quis enim eu nunc tincidunt tincidunt. Sed efficitur molestie sem, id euismod augue. Duis sagittis faucibus tortor, id congue felis eleifend ut. Ut finibus molestie maximus. Proin malesuada vitae justo vitae pulvinar. Phasellus id ligula facilisis, ullamcorper purus eget, auctor nisi. Maecenas vel nunc ut ex suscipit sodales. Nulla aliquet sem mauris, ac tincidunt leo feugiat id.\nVivamus sit amet tortor dui. Phasellus facilisis sodales faucibus. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Suspendisse semper enim enim, at dictum turpis consectetur at. Donec maximus nisi velit, in viverra tortor finibus vel. Sed a ante vel sapien convallis suscipit eget sed nunc. Morbi lacus sem, placerat in purus et, elementum molestie arcu. Phasellus vehicula sit amet ex ac sollicitudin. Proin non ligula quis augue sollicitudin scelerisque. Donec eget dui a neque pharetra tincidunt. Pellentesque rhoncus tincidunt est, quis tristique diam maximus non. Cras suscipit mauris eu ullamcorper tempus. Duis et blandit erat. Nam ullamcorper tempus egestas.\nQuisque velit lacus, mattis vehicula ullamcorper ut, scelerisque sit amet purus. Phasellus sed aliquet erat, sed gravida risus. Vivamus et sem egestas, tincidunt ex eget, tincidunt quam. Phasellus lacinia nibh ac metus consequat fermentum et id lectus. In lacus ligula, suscipit eget luctus non, efficitur fringilla sem. Phasellus aliquet tortor ut purus malesuada cursus. Aliquam erat volutpat.\nPhasellus vestibulum auctor turpis a ullamcorper. Morbi vitae mollis justo. Interdum et malesuada fames ac ante ipsum primis in faucibus. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Maecenas ac sollicitudin ligula. Ut luctus leo felis. Sed eleifend elit ut risus blandit porttitor. Pellentesque lobortis mauris at enim maximus aliquet. Donec euismod aliquet lobortis. Donec sit amet eleifend libero. Suspendisse imperdiet tempor varius. Suspendisse condimentum dapibus sem quis fermentum. In hendrerit, est quis blandit cursus, tellus massa placerat purus, non pretium metus turpis nec mauris. Integer pretium laoreet sem a tincidunt.\nCurabitur blandit velit non dolor convallis aliquet. Phasellus nulla nibh, dignissim non nulla sed, lobortis fermentum ipsum. Mauris lobortis mollis nisi, et gravida augue euismod nec. Nam volutpat tempor dui, vel pharetra arcu tempor ac. Pellentesque aliquet posuere elit a bibendum. Nunc elit erat, tristique in lacus fermentum, bibendum egestas elit. Praesent in nunc hendrerit urna bibendum pretium. Mauris porta lobortis nisi, vel dignissim diam tempor et. Phasellus et sagittis enim. Donec sollicitudin eget lorem at iaculis. Ut magna sem, auctor vitae nunc vitae, interdum porta lacus. Pellentesque fringilla pharetra justo quis placerat. Fusce non lectus tempus, commodo urna ac, vehicula ipsum. Vestibulum a mauris rutrum, tempus massa a, fermentum sapien.\nMaecenas velit tortor, vestibulum imperdiet ante id, pharetra feugiat massa. Integer rhoncus tellus non metus consectetur feugiat. Mauris non ante a enim tempor sollicitudin nec ut eros. Morbi vel ullamcorper nibh. Nam porta dui vitae ultrices sollicitudin. In elementum ullamcorper nulla quis semper. Integer viverra condimentum nunc non tempus. Sed in nisl nisl. Morbi non ultrices dolor. Fusce molestie in dui et tristique. Nam justo ipsum, pretium ac euismod sed, consequat non odio. Donec nec mi mattis, lobortis risus vel, lobortis lorem.\nCurabitur vitae facilisis arcu, eget cursus sapien. Donec gravida elementum molestie. Sed pretium, nisi varius convallis porttitor, urna sem consequat orci, eu ultricies massa sapien non turpis. Praesent molestie rhoncus nisi eget interdum. Nam elementum id lorem vitae amet.">>).

-define(MESSAGE, #'basic.publish'{exchange = <<"amq.topic">>,
                                  routing_key = <<"test.message">>},
                                  #amqp_msg{props = #'P_basic'{app_id = <<"mgmt_stress">>,
                                                               content_type = <<"text/plain">>,
                                                               type = <<"Lorum Ipsum">>},
                                            payload = ?LORUM_IPSUM}).

start_link({ID, Settings}) ->
  gen_server:start_link({local, ID}, ?MODULE, {ID, Settings}, []).

init({ID, Settings}) ->
  {ok, Connection} = amqp_connection:start(amqp_params_network(Settings)),
  Channel = case proplists:get_value(channels, Settings) of
    1 ->
      {ok, Chan} = amqp_connection:open_channel(Connection),
      Chan;
    _ -> undefined
  end,

  {ok, Timer} = timer:apply_interval(100, gen_server, cast, [ID, process_interval]),
  {ok, #state{settings=Settings,
              connection=Connection,
              channel=Channel,
              id=ID,
              timer=Timer}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(process_interval, State) ->
  process_interval(State),
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  amqp_connection:close(State#state.connection),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% --------------------------------------------------------------------

amqp_params_network(Settings) ->
  VHost = list_to_binary(proplists:get_value(vhost, Settings)),
  Username = list_to_binary(proplists:get_value(user, Settings)),
  Password = list_to_binary(proplists:get_value(password, Settings)),
  #amqp_params_network{host = proplists:get_value(host, Settings),
                       port = proplists:get_value(port, Settings),
                       virtual_host = VHost,
                       username = Username,
                       password = Password}.

open_channels(Connection, Qty) ->
  open_channels(Connection, Qty, []).

open_channels(_, Qty, Accum) when length(Accum) =:= Qty ->
  Accum;
open_channels(Connection, Qty, Accum) ->
  {ok, Channel} = amqp_connection:open_channel(Connection),
  open_channels(Connection, Qty, lists:append(Accum, [Channel])).


process_interval(#state{settings = Settings, connection = Connection, channel = Channel}) ->
  case Channel of
    undefined ->
      Channels = open_channels(Connection, proplists:get_value(channels, Settings)),
      publish_messages(Settings, Channels),
      close_channels(Channels);
    _ ->
      publish_messages(Settings, [Channel])
  end.


close_channels([]) -> ok;
close_channels([H|T]) ->
  amqp_channel:close(H),
  close_channels(T).


publish_messages(_, []) -> ok;
publish_messages(Settings, [Channel|T]) ->
  case proplists:get_value(messages, Settings) of
    0 -> ok;
    Max ->
      Messages = lists:seq(1, random:uniform(Max)),
      publish_message(Channel, Messages),
      publish_messages(Settings, T)
  end.

publish_message(_, []) -> ok;
publish_message(Channel, [_|T]) ->
  amqp_channel:cast(Channel, ?MESSAGE),
  publish_message(Channel, T).

