## What is mcbench
mcbench is memcached benchmark tool written in Erlang.

## Building and Installing 
  1. Install a recent version of Erlang.
  2. Get a release of mcbench from xxxxx.
  3. make
  4. make install with Environment variables for configuration.
     TARGET_DIR: Installation target directory.
     SBIN_DIR: sbin direcotry.

     e.g.
       TARGET_DIR=/user/local/mcbench SBIN_DIR=/usr/sbin/ make install 

## Running mcbench

    # benchmark of "get" command with initial 10000 datum. 10 threads and 100 commands/thread.
    % mcbench -b 10000  -t 10 -n 100 -c get     

    # benchmark of "set" command. 10 threads and 100 commands/thread.
    % mcbench -s 10.0.0.1 -p 11211 -t 10 -n 100 -c get     

## Author
Copyright (C) Cybozu Labs, Inc., written by Taro Minowa(Higepon) <higepon@labs.cybozu.co.jp>

## License
New BSD License
