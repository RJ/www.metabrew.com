--- 
layout: post
published: true
title: "BigWig: A better Erlang webtool (spawnfest entry)"
permalink: /article/bigwig-erlang-webtool-spawnfest
tags: 
- programming
- erlang
- webtool
- spawnfest
- etop
- appmon
- sasl
---

This weekend, <a href="http://twitter.com/mokele">Steve</a>, <a href="http://twitter.com/skarab">Hunter</a>, <a href="http://twitter.com/puzza007">Paul</a> and I took part in <a target="new" 
href="http://spawnfest.com/">Spawnfest</a>, a 48-hour Erlang programming
event.

Our team name was SMELLS LIKE BEAM SPIRIT. Here we are on Sunday evening:

<a href="/images/bigwig/team.jpg">
    <img src="/images/bigwig/team.jpg" alt="" title="Smells like BEAM spirit" width="500"  class="aligncenter size-full" />
</a>

## What a bunch of tools...

The Erlang VM lets you inspect all aspects of a running node, examine running
processes, explore process hierarchies, and profile running systems.
This is typically done from the Erlang console, but if you're feeling
especially nostalgic, you can start Erlang's appmon (a real GUI app), or 
"webtool", a web-based version of some of the tools:


### Erlang's Webtool and Appmon

<a href="/images/bigwig/webtool-orig.png">
    <img src="/images/bigwig/webtool-orig.png" alt="" title="Erlang's Webtool" width="500"  class="aligncenter size-full" />
</a>

<br/>

<a href="/images/bigwig/appmon-orig.png">
    <img src="/images/bigwig/appmon-orig.png" alt="" title="Erlang's Appmon, and other tools" width="500"  class="aligncenter size-full" />
</a>

## BigWig

Our project, called BigWig, is a modern suite of web-based tools that we hope
will eventually supercede webtool, appmon and the rest.

### Processes (etop)

Our version of etop, a realtime-updating, sortable process list:

<a href="/images/bigwig/etop.png">
    <img src="/images/bigwig/etop.png" alt="" title="Bigwig's erlang process viewer" width="500"  class="aligncenter size-full" />
</a>


### Process Info

Click on a process ID anywhere in the app, and you get a process info dialog,
where you can see the state of the process, kill it and send messages:

<a href="/images/bigwig/pid.png">
    <img src="/images/bigwig/pid.png" alt="" title="Process info dialog" width="500"  class="aligncenter size-full" />
</a>

### Application Explorer (appmon)

Explore the application supervison-tree hierarchy in style. You can click
around and more nodes are loaded in on-demand, and changes are sent to the page
updating the graph as they happen:

<a href="/images/bigwig/appmon.png">
    <img src="/images/bigwig/appmon.png" alt="" title="Bigwig's application explorer" width="500"  class="aligncenter size-full" />
</a>

### SASL Report Browser

This one isn't quite finished yet, but is intended to be a nice way to browse
SASL reports instead of report browser (rb.erl).

We opted to read the existing SASL binary dir format, instead of installing our
own event handler that logged reports differently. This was probably a mistake,
since log_mf_h handler and rb.erl are pretty dated, and designed specifically
to render reports as text to stdout. At the moment we do install a custom event
handler, but only so we can stream new reports to the browser as they happen.

We'll tidy it up, but ultimately I think we should be installing our own
event handler and storing reports in a nicer way - possibly couch, or even just
update the code to use disk\_log, and separate out the rendering bits.

### Dashboard

Shows the list of installed and loaded applications, which releases are
installed, according to release\_handler, and details about the VM. 

We ran out of time at the weekend, so you just see the JSON for now:

<a href="/images/bigwig/dashboard.png">
    <img src="/images/bigwig/dashboard.png" alt="" title="Bigwig's dashboard" width="500"  class="aligncenter size-full" />
</a>

## BigWig Architecture

We used <a target="new" href="https://github.com/extend/cowboy">cowboy</a> as our webserver, and
make extensive use of websockets. For example, those sparkline graphs and
metrics in the left sidebar are pushed to the page, and updated every couple of
seconds.

At the moment, BigWig only inspects the node it is currently running on. The
plan is to make it start up as a hidden node, and let you specify which nodes
to monitor by providing a nodename and cookie.

## Source Code

All on github here: <a href="https://github.com/beamspirit/bigwig">BigWig for
Erlang</a>. Licensed the same as Erlang/OTP.

Check the readme - installing and running is easy!






