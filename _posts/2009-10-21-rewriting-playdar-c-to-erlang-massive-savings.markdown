--- 
layout: post
title: "Rewriting Playdar: C++ to Erlang, massive savings"
permalink: /article/rewriting-playdar-c-to-erlang-massive-savings
tags: 
- programming
- erlang
- c
- playdar
- rewrite
wordpress_id: 279
---
<p>
I've heard many anecdotes and claims about how many lines of code are saved when you write in Erlang instead of [C++/other language]. I'm happy to report that I now have first-hand experience and some data to share.
</p>

<p>
I initially wrote Playdar in C++ (using Boost and Asio libraries), starting back in February this year. I was fortunate to be working with some experienced developers who helped me come to terms with C++. There were three of us hacking on it regularly up until a few months ago, and despite being relatively new to C++, I'll say that we ended up with a well designed and robust codebase, all things considered.
</p>

<h2>On Feeling Smug</h2>

<p>
I'll admit I felt rather smug making it all work in C++ with Boost and ASIO. Getting it to build on all three platforms and dynamically load extensions (DLLs etc) at runtime in a cross-platform way was also quite satisfying (I had plenty of help with that side of things).  I learned a lot about C++, Boost, ASIO and CMake. But, as the codebase grew, I began to seriously question my decision to use C++. 
</p>
<p>
My initial reasons for choosing C++ were twofold:
<ul>
<li>Distribution - shipping the Erlang VM didn't sound like fun</li>
<li><a href="http://developer.kde.org/~wheeler/taglib.html" target="taglib">Taglib</a>  - *the* library to read metadata from audio files (mp3, m4a, ogg etc) is C++</li>
</ul>
It turns out Playdar is naturally a good fit for Erlang - it does lots in parallel, and lots of stuff it does is asynchronous  and event based. Even with all the stuff you get with Boost, multithreaded stuff in C++ is inelegant, to put it kindly.
</p>

<h2>SLOCed and Loaded</h2>

<p>
Anyway, a couple of weeks ago I sat down to re-implement Playdar from scratch in Erlang. I thrashed out the guts of it in a couple of days, and by the end of the week I almost had it 1:1 features with the C++ codebase. There's still a bit of C++ left - code to interface with taglib.

Using the SLOCcount tool (SLOC=source lines of code) I counted the lines of code in various modules from both codebases, here are the results:
<br/>
<style type="text/css">
#matrix td{ font-size:90%; vertical-align:top; padding: 3px; } #matrix tr { background: #f0f0f0; } #matrix tr.odd { background: #ddd; }
#matrix td.b {font-size:100%; font-weight:bold;}
</style>
<table id="matrix" border="0">
<tbody>
<tr>
<td class="b"></td>
<td class="b">Erlang Version</td>
<td class="b">C++ Version</td>
<td class="b">Savings</td>
</tr>
<tr class="odd">
<td class="b">Core Daemon</td>
<td>1,100</td>
<td>4,491</td>
<td>75%</td>
</tr>
<tr>
<td class="b">Library + Scanner</td>
<td>197 + 167.cpp</td>
<td>1,355</td>
<td>73%</td>
</tr>
<tr class="odd">
<td class="b">LAN Resolver</td>
<td>105</td>
<td>427</td>
<td>75%</td>
</tr>
<tr>
<td class="b">P2P</td>
<td>463</td>
<td>1,762</td>
<td>74%</td>
</tr>
<tr class="odd b">
<td class="b">TOTAL</td>
<td><em>2,032</em></td>
<td><em>8,035</em></td>
<td><em>75%</em></td>
</tr>
</tbody></table>
<strong>
75% less lines of code using Erlang compared to C++ to implement the same thing - not too shabby :)
</strong>
The second time around writing in Erlang I knew exactly what I was building, so it's unfair to compare development time of the two codebases, but given how fast I can type I reckon I saved a good few hours of just pounding the keyboard to input the code (and countless hours of debugging: Erlang tends to work first time, really). Well I'm not sure if "saved" is the right word, considering It was working in C++ already, but it's my time to waste :)
</p>
<p>
If you count the third party code bundled with both codebases (excluding boost/asio!) then the erlang codebase saves a whopping 92%. I'm more interested in the savings in code I had to write, however.
</p>
<h2>Memory and CPU Usage</h2>

<p>
I've done some preliminary comparisons between both projects, when it comes to CPU and memory usage both projects are pretty similar. The Erlang codebase uses slightly more memory than C++ at the moment, but I'm convinced I can get that down to at least as low as the C++ project was. I picked up a few optimization tricks from my three-part <a href="http://www.metabrew.com/article/a-million-user-comet-application-with-mochiweb-part-1/">Million-user comet experiment</a> in Erlang earlier this year. I'll post more about this if I learn any new tricks.
</p>
<p>
One thing I've realised about the Erlang codebase is that I've used processes to encapsulate state (active queries, specifically)  where I didn't really need to. It seemed sensible at the time, but it's probably just a waste of memory. I'm going to change it to spawn processes to get the work done (ie, a process that runs the query) but not necessarily just to maintain state.
</p>

<h2>Distribution to the desktop</h2>

<h3>C++</h3>
<p>
You just have to make sure that you build everything and ship with any DLLs along with checks in the installer for system libraries needed (runtime dlls). Oh, and make sure you don't change the plugin binary interface in the main app, or new plugins will crash and burn when you load them. Add a check for that. Oh and be careful about compiling taglib and stuff with mingw and the rest with VC++, or things might mysteriously crash. Also I heard a horror story about allocating memory in plugin code but deallocating it in the main app when the plugin was compiled against a different stdlib than the main app. This is all par for the course, and the experienced C++ developers I asked for help had no trouble making it work. <strong>Size of installable pacakge: 2.5MB</strong>
</p>
<h3>Erlang</h3>
<p>
Compiling, and building/loading plugins in the Erlang codebase is straightforward on all platforms, as is often the way with VMs. I was against shipping the Erlang VM originally because I figured it would be a lot of hassle and increase the download size substantially. Packaging an Erlang app for the desktop involves taking the installed VM directory structure and stripping out all the docs, source and parts of the Erlang stdlib we don't use, then packaging it along with the compiled Playdar code. <a href="http://couchdb.apache.org/" target="cdb">CouchDB</a> does something like this too, and <a href="http://www.rabbitmq.com/" target="rabbit">RabbitMQ</a> ships the Erlang VM without stripping unneeded libs. We'll work on packaging some more (for all platforms), but to date <a href="http://twitter.com/mxcl">Max</a> has crafted a package that contains the necessary bits of the Erlang VM, a sexy Prefpane to start/stop the daemon on OS X, and the compiled Playdar code all <strong>weighing in under 10MB.</strong>
</p>
<p>
We'll put together a Windows installer soon that'll probably be around the same size. A 10MB download isn't so bad nowadays, and I expect we can optimize the packaging process some more. Linux users will get a package that depends on the erlang VM in their package manager.
Seems like shipping Erlang apps to the desktop isn't so hard after all.
</p>
<h2>tl;dr</h2>
<p>
Someone rewrote a C++ app in Erlang: 75% less lines of code for same functionality.
</p>
<p>
You should read this <a href="http://musicmachinery.com/2009/10/18/playing-with-playdar/">blog post about Playdar, by Paul Lamere</a>,  and take a look at the <a href="http://www.playdar.org/">Playdar website</a>.
</p>
<p>
<a href="http://github.com/RJ/playdar">C++ codebase (deprecated)</a>
<a href="http://github.com/RJ/playdar-core">Erlang codebase</a>
</p>
<p>
<strong>Playdar is the future, and the future is written in Erlang :)</strong>
</p>
