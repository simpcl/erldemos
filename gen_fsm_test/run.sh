#!/bin/bash

erlc ./*.erl
#erl -run npc start_link
erl -run code_lock start_link 123
