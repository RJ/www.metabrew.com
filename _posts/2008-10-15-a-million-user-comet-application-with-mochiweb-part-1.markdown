--- 
layout: post
title: A Million-user Comet Application with Mochiweb, Part 1
permalink: /article/a-million-user-comet-application-with-mochiweb-part-1
tags: 
- programming
- erlang
- kernel
- tcp
- networking
- comet
- http
- mochiweb
wordpress_id: 63
---
In this series I will detail what I found out empirically about how mochiweb performs with lots of open connections, and show how to build a comet application using mochiweb, where each mochiweb connection is registered with a router which dispatches messages to various users. We end up with a working application that can cope with a million concurrent connections, and crucially, knowing how much RAM we need to make it work. 

In part one:
<ul>
 <li>Build a basic comet mochiweb app that sends clients a message every 10 seconds.</li>
 <li>Tune the Linux kernel to handle lots of TCP connections</li>
 <li>Build a flood-testing tool to open lots of connections (ye olde C10k test)</li>
 <li>Examine how much memory this requires per connection.</li>
</ul>

Future posts in this series will cover how to build a real message routing system, additional tricks to reduce memory usage, and more testing with 100k and 1m concurrent connections. 

I assume you know your way around the Linux command line, and know a bit of Erlang.

<h2>Building a Mochiweb test application</h2>
In brief:
<ol>
	<li>Install and build Mochiweb</li>
	<li>Run: <code>/your-mochiweb-path/scripts/new_mochiweb.erl mochiconntest</code></li>
	<li><code>cd mochiconntest</code> and edit <code>src/mochiconntest_web.erl</code></li>
</ol>
This code (mochiconntest_web.erl) just accepts connections and uses chunked transfer to send an initial welcome message, and one message every 10 seconds to every client.

```erlang
-module(mochiconntest_web).
-export([start/1, stop/0, loop/2]).
%% External API
start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    % we'll set our maximum to 1 million connections. (default: 2048)
    mochiweb_http:start([{max, 1000000}, {name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                "test/" ++ Id ->
                    Response = Req:ok({"text/html; charset=utf-8",
                                      [{"Server","Mochiweb-Test"}],
                                      chunked}),
                    Response:write_chunk("Mochiconntest welcomes you! Your Id: " ++ Id ++ "\n"),
                    %% router:login(list_to_atom(Id), self()),
                    feed(Response, Id, 1);
                _ ->
                    Req:not_found()
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

feed(Response, Path, N) ->
    receive
        %{router_msg, Msg} ->
        %    Html = io_lib:format("Recvd msg #~w: '~s'<br/>", [N, Msg]),
        %    Response:write_chunk(Html);
    after 10000 ->
        Msg = io_lib:format("Chunk ~w for id ~s\n", [N, Path]),
        Response:write_chunk(Msg)
    end,
    feed(Response, Path, N+1).

%% Internal API
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
```

<h2>Start your mochiweb app</h2>

```console
$ make && ./start-dev.sh
```

By default mochiweb listens on port 8000, on all interfaces. If you are doing this on the desktop, you can test with any web browser. Just navigate to <a href="http://localhost:8000/test/foo">http://localhost:8000/test/foo</a>.<br/>
Here's the command-line test:
```console
$ lynx --source "http://localhost:8000/test/foo"
Mochiconntest welcomes you! Your Id: foo<br/>
Chunk 1 for id foo<br/>
Chunk 2 for id foo<br/>
Chunk 3 for id foo<br/>
^C
```

Yep, it works. Now let's make it suffer.

<h2>Tuning the Linux Kernel for many tcp connections</h2>

Save yourself some time and tune the kernel tcp settings before testing with lots of connections, or your test will fail and you'll see lots of <code>Out of socket memory</code> messages (and if you are masquerading, <code>nf_conntrack: table full, dropping packet.</code>)

Here are the sysctl settings I ended up with - YMMV, but these will probably do:
```console
$ cat /etc/sysctl.conf
# General gigabit tuning:
net.core.rmem_max = 16777216
net.core.wmem_max = 16777216
net.ipv4.tcp_rmem = 4096 87380 16777216
net.ipv4.tcp_wmem = 4096 65536 16777216
net.ipv4.tcp_syncookies = 1
# this gives the kernel more memory for tcp
# which you need with many (100k+) open socket connections
net.ipv4.tcp_mem = 50576   64768   98152
net.core.netdev_max_backlog = 2500
# I was also masquerading the port comet was on, you might not need this
net.ipv4.netfilter.ip_conntrack_max = 1048576
```

Put these in <code>/etc/sysctl.conf</code> then run <code>sysctl -p</code> to apply them. No need to reboot, now your kernel should be able to handle a lot more open connections, yay.

<h2>Creating a lot of connections</h2>

There are many ways to do this. <a href="http://tsung.erlang-projects.org/">Tsung</a> is quite sexy, and there and plenty of other less-sexy ways to spam an httpd with lots of requests (ab, httperf, httpload etc). None of them are ideally suited for testing a comet application, and I'd been looking for an excuse to try the Erlang http client, so I wrote a basic test to make lots of connections. 
Just because you can, doesn't mean you should.. one process per connection would definitely be a waste here. I'm using one process to load urls from a file, and another process to establish and receive messages from all http connections (and one process as a timer to print a report every 10 seconds). All data received from the server is discarded, but it does increment a counter so we can keep track of how many HTTP chunks were delivered.
<br/>

floodtest.erl
```erlang
-module(floodtest).
-export([start/2, timer/2, recv/1]).

start(Filename, Wait) ->
    inets:start(),
    spawn(?MODULE, timer, [10000, self()]),
    This = self(),
    spawn(fun()-> loadurls(Filename, fun(U)-> 
                                        This ! {loadurl, U} 
                                     end, Wait) end),
    recv({0,0,0}).

recv(Stats) ->
    {Active, Closed, Chunks} = Stats,
    receive
        {stats} -> io:format("Stats: ~w\n",[Stats])
        after 0 -> noop
    end,
    receive
        {http,{_Ref,stream_start,_X}} -> recv({Active+1,Closed,Chunks});
        {http,{_Ref,stream,_X}}       -> recv({Active, Closed, Chunks+1});
        {http,{_Ref,stream_end,_X}}   -> recv({Active-1, Closed+1, Chunks});
        {http,{_Ref,{error,Why}}}     -> io:format("Closed: ~w\n",[Why]),
                                         recv({Active-1, Closed+1, Chunks});
        {loadurl, Url}                ->
            http:request(get, {Url, []}, [], [{sync, false}, 
                                              {stream, self}, 
                                              {version, 1.1}, 
                                              {body_format, binary}]),
            recv(Stats)
    end.

timer(T, Who) ->
    receive
    after T ->
        Who ! {stats}
    end,
    timer(T, Who).

% Read lines from a file with a specified delay between lines:
for_each_line_in_file(Name, Proc, Mode, Accum0) ->
    {ok, Device} = file:open(Name, Mode),
    for_each_line(Device, Proc, Accum0).

for_each_line(Device, Proc, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> NewAccum = Proc(Line, Accum),
                    for_each_line(Device, Proc, NewAccum)
    end.

loadurls(Filename, Callback, Wait) ->
    for_each_line_in_file(Filename,
        fun(Line, List) ->
            Callback(string:strip(Line, right, $\n)),
            receive
            after Wait ->
                noop
            end,
            List
        end,
        [read], []).
```

<br/>
Each connection we make requires an ephemeral port, and thus a file descriptor, and by default this is limited to 1024. To avoid the <code>Too many open files</code> problem you'll need to modify the ulimit for your shell. This can be changed in <code>/etc/security/limits.conf</code>, but requires a logout/login. For now you can just sudo and modify the current shell (su back to your non-priv'ed user after calling ulimit if you don't want to run as root):

```console
$ sudo bash
# ulimit -n 999999
# erl
```

You might as well increase the ephemeral port range to the maximum too:
<code># echo "1024    65535" &gt; /proc/sys/net/ipv4/ip_local_port_range</code>

Generate a file of URLs to feed to the floodtest program:
```bash
( for i in `seq 1 10000`; \
  do echo "http://localhost:8000/test/$i" ; done ) > \
    /tmp/mochi-urls.txt
```

From the erlang prompt you can now compile and launch <code>floodtest.erl</code>:
<pre>erl&gt; c(floodtest).
erl&gt; floodtest:start("/tmp/mochi-urls.txt", 100).</pre>

This will establish 10 new connections per second (ie, 1 connection every 100ms).

It will output stats in the form <code>{Active, Closed, Chunks}</code> where Active is the number of connections currently established, Closed is the number that were terminated for some reason, and Chunks is the number of chunks served by chunked transfer from mochiweb. Closed should stay on 0, and Chunks should be more than Active, because each active connection will receive multiple chunks (1 every 10 seconds).

The *resident size of the mochiweb beam process with 10,000 active connections was 450MB - that's 45KB per connection*. CPU utilization on the machine was practically nothing, as expected.

<h2>Assessment so far</h2>

That was a reasonable first attempt. 45KB per-connection seems a bit high - I could probably cook something up in C using libevent that could do this with closer to 4.5KB per connection (just a guess, if anyone has experience please leave a comment). If you factor in the amount of code and time it took to do this in Erlang compared with C, I think the increased memory usage is more excusable.

In future posts I'll cover building a message router (so we can uncomment lines 25 and 41-43 in <code>mochiconntest_web.erl</code>) and talk about some ways to reduce the overall memory usage. I'll also share the results of testing with 100k and 1M connections.

#### UPDATE
Check out <a href="http://www.metabrew.com/article/a-million-user-comet-application-with-mochiweb-part-2/">Part 2</a> and <a href="http://www.metabrew.com/article/a-million-user-comet-application-with-mochiweb-part-3/">Part 3</a> for the rest of the saga.
