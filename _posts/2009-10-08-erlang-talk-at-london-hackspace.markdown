--- 
layout: post
title: Erlang talk at London Hackspace
permalink: /article/erlang-talk-at-london-hackspace
tags: 
- uncategorized
- erlang
- playdar
- london
- hackspace
- talk
wordpress_id: 268
---
Last night I gave an "Intro to Erlang" talk at a London Hackspace meetup. I did a quick audience survey first: About 75% did "web programming" (ruby,python,php,etc).  Around 30% admitted to regularly using C/C++/Java or desktop/mobile app development.  Less than 10% had much experience with functional programming.

I wanted to impress upon the audience that Erlang is a practical language, built by Ericsson with a specific purpose in mind. You use Erlang to build useful, scalable and reliable distributed systems in the real world. This was worth pointing out because when many people hear "functional programming" they immediately think of eccentric bearded academics proving the validity of their Haskell code and comparing Monads.

I skipped through the basics of sequential programming in Erlang pretty quickly and tried to spend most of the time showing how you handle processes and send messages. I built a basic Erlang server process that kept a count of how many operations it had done, explaining how it passes state to itself on every loop. Hopefully this helped some people grok how you can build servers that keep a global state by using recursion. I also showed off hot code reloading. We added another feature to the server and upgraded it without stopping it.

You can download the code I used (see link at the end) if you want to try out the examples from last night yourself. The last code I showed was an example of doing the same thing using gen_server, so hopefully if you followed along you'll have a good understanding of what gen_server is and why it exists.
<h2>Hot code reloading example</h2>
I can't write a post about Erlang without including some code, so here's the basic example I used showing how hot code reloading works:


```erlang
-module(ex09).
-export([start/0, loop/2, client/3]).

start() -> spawn(?MODULE, loop, [0,0]).

loop(Ops,Wtfs) ->
 receive
   {Client, double, Num} ->
     Client ! Num * 2,
     loop(Ops+1, Wtfs);

   {Client, square, Num} ->
     Client ! Num * Num,
     loop(Ops+1, Wtfs);

   {Client, _, _Num} ->
     Client ! wtf,
     loop(Ops, Wtfs+1);

   reload ->
     io:format("Reloading~n"),
     ?MODULE:loop(Ops, Wtfs);

   stats ->
     io:format("Ops: ~p, Wtfs: ~p ~n", [Ops, Wtfs]),
     loop(Ops, Wtfs)
 end.

% basic client API:

client(Pid, Cmd, Num) ->
 Pid ! {self(), Cmd, Num},
 receive
   Ans -> Ans
 after 1000 -> timeout
 end.
```

And if you were following along you saw something like this:
```console
1> c(ex09).
{ok,ex09}
erl> Pid = ex09:start().
<0.38.0>
3> Pid ! stats.
Ops: 0, Wtfs: 0
stats
4> ex09:client(Pid, double, 10).
20
5> ex09:client(Pid, triple, 10).
wtf
```

At this point we added support for "triple" to the example and showed how the fully-qualified call to loop (using the modulename:fun() instead of fun() syntax) causes the newest version of the module to be used:

```console
6> c(ex09).
{ok,ex09}
7> ex09:client(Pid, triple, 10).
wtf
8> Pid ! reload.
Reloading
reload
9> ex09:client(Pid, triple, 10).
30
10> Pid ! stats.
Ops: 2, Wtfs: 2
stats
```

You can see from the stats at the end that the global state was kept - the server process staying running during the code upgrade.

<h2>Download</h2>
The slides, example code and basic mochiweb comet project we saw last night can be downloaded <a title="Slides and example code" href="http://www.metabrew.com/misc/erlang-hackspace-talk.tar.gz">here</a>. I should warn you that unless you saw my talk and the various explanations and disclaimers that went along with the code, it's probably not a good place to start or learn from. Have a look at <a href="http://www.learnyousomeerlang.com/" target="_blank">www.learnyousomeerlang.com</a> or get one of the two excellent Erlang books.
<h2>London Hackspace</h2>
If you live in London you should know about this. <a href="http://russ.garrett.co.uk/" target="_blank">Russ</a> and <a href="http://jonty.co.uk/" target="_blank">Jonty</a> (who I worked with at Last.fm for years) started <a href="http://london.hackspace.org.uk/" target="_blank">London Hackspace</a>: "<strong>We run a dedicated space for people to learn and build things in London."</strong> There are workshops at hackspace meetups on topics ranging from Arduino and electronics hacking, to iPhone development, to Erlang and beyond. Their unofficial slogan could be "Beer &amp; Hacking" - it's a great place to meet people doing interesting things in London, and to learn new things.

<a href="http://london.hackspace.org.uk/" target="_blank">http://london.hackspace.org.uk/</a>
<h2>Playdar</h2>
<a href="http://www.playdar.org/" target="_blank">Playdar</a> is my pet project at the moment. I talked about this last night too. I wrote it in C++ using Boost, mainly as an excuse to do something serious in C++. I've <a href="http://twitter.com/metabrew/status/4494402561" target="_blank">since seen the error of my masochistic ways</a> and in the last week I've tossed out the 10,000 lines of C++ and rewritten it in Erlang. I'm not quite finished, but once I have feature parity between the two codebases I'll write an article comparing the two.  As you might expect, the Erlang codebase is far superior in almost every way.

<a href="http://www.playdar.org/" target="_blank">http://www.playdar.org/</a>
