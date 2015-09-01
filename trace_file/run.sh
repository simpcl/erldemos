#!/bin/bash

erlc ./trace_file.erl
erl -noshell -run trace_file start $1 $2 
#erl -noshell -run trace_file start "/tmp/tracettt" "/tmp/b.txt"
