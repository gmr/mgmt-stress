# RabbitMQ Stress Test Tool

The goal of the app is to cause the RabbitMQ management UI to fall over due
to a high velocity of channel turn over across a modest amount of connections.

## Build

    $ bin/rebar3 escriptize

## Usage

    Usage: mgmt_stress [-d [<duration>]] [-h [<host>]] [-p [<port>]]
                       [-v [<vhost>]] [-u [<user>]] [-P [<password>]]
                       [-C [<connections>]] [-c [<channels>]]
                       [-m [<messages>]] [--help] [-v <verbose>]
    
      -d, --duration     Number of seconds to run test [default: 300]
      -h, --host         RabbitMQ host [default: 127.0.0.1]
      -p, --port         RabbitMQ port [default: 5672]
      -v, --vhost        RabbitMQ Virtual Host [default: /]
      -u, --user         RabbitMQ User [default: guest]
      -P, --password     RabbitMQ password [default: guest]
      -C, --connections  Max Connections [default: 150]
      -c, --channels     Max Channels [default: 2]
      -m, --messages     Max Messages [default: 100]
      --help             Print usage

## Run

To run in a local Docker container:

    $ ./bootstrap
    $ . _build/test-environment
    
    # Use docker ps -a to get the port #'s to test with, it should be the ephemeral port for 5672
    
    $  _build/default/bin/mgmt_stress -d 600 -C 150 -c 2 -m 100 -p <PORT # FROM docker ps>
