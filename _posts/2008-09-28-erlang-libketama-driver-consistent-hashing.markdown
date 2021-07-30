--- 
layout: post
title: Erlang libketama driver - Consistent Hashing
permalink: /article/erlang-libketama-driver-consistent-hashing
tags: 
- programming
- erlang
- c
- memcached
- driver
- hashing
- ketama
wordpress_id: 36
---
All the data I need from memcached is assigned to servers using a consistent hashing mechanism, <a href="http://www.last.fm/user/RJ/journal/2007/04/10/rz_libketama_-_a_consistent_hashing_algo_for_memcache_clients">implemented as libketama</a> - a shared library written in C. We use a php extension to wrap this, and also have a pure java implementation. Rather than port the algorithm to Erlang, I wrote a an Erlang driver.

There are 3 things covered here:
<ul>
	<li>A small driver program written in C (using libketama)</li>
	<li>Some basic testing from the shell using Perl and xxd</li>
	<li>The Erlang gen_server that calls it</li>
</ul>
<h2>C driver program</h2>
```c
/*  Expects a one-byte length header, followed by a key (<255bytes)
 *  Returns an ip:port string with 1 byte len header
 *
 */
#include <ketama.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

typedef unsigned char byte;

int read_exact(byte *buf, int len)
{
    int i, got = 0;
    do {
        if((i=read(0,buf+got, len-got))<=0) return i;
        got += i;
    } while(got<len);
    return len;
}

int main(int argc, char **argv)
{
    if(argc==1){
        printf("Usage: %s <ketama.servers file>\n", *argv);
        return 1;
    }

    ketama_continuum c;
    ketama_roll( &c, *++argv );
    mcs *m;

    byte len;
    byte buffer[256];
    while ( 1 ) {
        if( 1 != read_exact(&len, 1) ) break;
        if( (int)len >= 255 ) break;
        read_exact((byte *)&buffer, (int)len);
        buffer[len] = '\0';
        m = ketama_get_server( (char *) &buffer, c );
        sprintf((char *)&buffer, "%s",m->ip);
        int respleni = strlen(m->ip);
        char l = (0xff & respleni);
        write(1, &l, 1);
        write(1, (char*)&buffer, respleni);
    }

    return 0;
}
```

<h2>Testing the driver with Perl and xxd</h2>

Before writing the Erlang bit, it'd be nice to know the driver program does what we expect.  Will send the driver a 1-byte length header followed by the key, and expect a 1-byte length header and the value as a response. Say we're hashing a memcached key 'user:123' to a server, we can do what the Erlang port does with a bit of perl, and the 'xxd' command to see output in binary.

```console
$ perl -e '$key="user:123"; $len=pack("C",length($key)); print $len; print $key;' \
$  | xxd -b 
0000000: 00001000 01110101 01110011 01100101 01110010 00111010  .user:
0000006: 00110001 00110010 00110011                             123
```

Note the first byte (00001000) printed before the key is the length of the key, 8. Now let's send this to the driver program and check the response (provide a valid ketama.servers file):

```console
$ perl -e '$key="user:123"; $len=pack("C",length($key)); print $len; print $key;' \
$  | ./ketama_erlang_driver /var/ketama.servers | xxd -b
0000000: 00010000 00110001 00110000 00101110 00110000 00101110  .10.0.
0000006: 00110001 00101110 00110001 00110001 00111000 00111010  1.118:
000000c: 00110001 00110001 00110010 00110001 00110001           11211
```

The first byte of the response (00010000) is 16, which is the length of the server address returned by the driver, "10.0.1.118:11211" - It does what we expect, onwards...

<h2>The Erlang bit</h2>

```erlang
-module(ketama).
-behaviour(gen_server).
-export([start_link/0, start_link/1, start_link/2, getserver/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {port}).

start_link() ->
    start_link("/web/site/GLOBAL/ketama.servers").

start_link(ServersFile) ->
    start_link(ServersFile, "/usr/bin/ketama_erlang_driver").

start_link(ServersFile, BinPath) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ServersFile, BinPath], []).

getserver(Key) ->
    gen_server:call(?MODULE, {getserver, Key}).

%%

init([ServersFile, BinPath]) ->
    Exe = BinPath ++ " " ++ ServersFile,
    Port = open_port({spawn, Exe}, [binary, {packet, 1}, use_stdio]),
    {ok, #state{port=Port}}.

handle_call({getserver, Key}, _From, #state{port=Port} = State) ->
    Port ! {self(), {command, Key}},
    receive
        {Port, {data, Data}} ->
            {reply, Data, State}
        after 1000 -> % if it takes this long, you have serious issues.
            {stop, ketama_port_timeout, State}
    end.

handle_cast(_Msg, State) ->    {noreply, State}.
handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {port_terminated, Reason}, State}.
terminate({port_terminated, _Reason}, _State) ->    ok;
terminate(_Reason, #state{port = Port} = _State) ->     port_close(Port).
code_change(_OldVsn, State, _Extra) ->     {ok, State}.
```

This code can be found in the erlang directory of the ketama source in svn:
<code>svn://svn.audioscrobbler.net/misc/ketama/</code>
