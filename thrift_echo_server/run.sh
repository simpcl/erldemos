#!/bin/bash

erlc ./thrift_socket_server.erl
#erl -noshell -pa . -s thrift_socket_server start
erl -sname tss -noshell -detached -pa . -s thrift_socket_server start
