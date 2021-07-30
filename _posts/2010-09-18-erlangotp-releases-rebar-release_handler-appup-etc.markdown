--- 
layout: post
title: "Erlang/OTP releases: rebar, release_handler, .appup, etc"
permalink: /article/erlangotp-releases-rebar-release_handler-appup-etc
tags: 
- programming
- erlang
- hacks
- mochiweb
- irc
- irccloud
- otp
- appup
- sysops
- deployment
- irccloud
wordpress_id: 316
---

I've been building something in Erlang recently, provisionally called <a title="It's the Gmail of IRC, honest!" href="https://irccloud.com/" target="_blank">IRCCloud.com</a> (mention this post if you request an invite!) - it's an in-browser IRC client that stays connected for you all the time, so you never miss the conversation. You can reopen your browser later and still have all the backlog. <a title="IRC: still damn useful" href="http://www.metabrew.com/article/how-we-use-irc-at-lastfm" target="_blank">IRC is damn useful</a>, and <a title="James Wheare: International man of mystery" href="http://james.wheare.org/" target="_blank">James</a> and I are building IRCCloud to give you the advantages of IRC bouncer-esque functionality, with the ease of just opening a webpage.

The Erlang backend is connected to various IRC servers on behalf of our users, so it's critical that I can deploy new versions of the app without restarting. Erlang's capability to do live upgrades to running applications means it's possible to deploy new versions of the backend without disconnecting everybody. I've done a fair amount of Erlang before, but I've only recently managed to get proper live-updates to running OTP applications working.

Previous upgrade experience ranged from just copying over a new beam and running <code>l(my_module).</code> in the shell, to manually triggering code_change like this:

```erlang
sys:suspend(Pid),
{module, my_module} = code:load_file(my_module),
sys:change_code(Pid, my_module, undefined, [], 10000),
sys:resume(Pid);
```

If you want to start doing complex updates that change internal state, amongst other things, you really want to be doing it in the proper OTP way.

I started with this <a title="A good read" href="http://spawnlink.com/articles/performing-real-time-upgrades-to-an-otp-system/" target="_blank">excellent series of articles about Erlybank, on spawnlink.com</a>. I definitely recommend following along the last couple in the series to get a feel for packaging up erlang releases. I'm not going to provide a step-by-step tutorial in this post, since Mitchell already did a great job in those articles.

Unfortunately I fell at the last hurdle - I ran into the same problem as Ricardo in the comments, namely that <a href="http://spawnlink.com/articles/performing-real-time-upgrades-to-an-otp-system/#comments" target="_blank">release_hander was looking for the .appup file in the wrong place</a>. I don't know why this happens, but it seems that because I had the erlang system itself as a "release" (R13B etc), that somehow screwed things up. I found one other <a title="Annoying." href="http://erlang.2086793.n4.nabble.com/install-release-1-always-returns-enoent-error-td2093926.html" target="_blank">post about the same problem</a>. If you know what's going on here, please leave a comment and put me out of my misery.

## Upgrade to R14!

Let me get this out the way first: <strong>upgrade your Erlang to R14 right now</strong>, since R13B has a bug that prevents release_handler from figuring out which modules need updating when you install a new release. I lost some time to that, although consequently I did end up taking a nice tour of the sasl/release_handler/systools code.</p>
Oh, and also "rebar generate" (mentioned later in this post) will fail with some distro packaged versions of R13B04, with a message like: *ERROR: Unable to generate spec: read file info /usr/lib/erlang/man/man5/modprobe.d.5 failed.*

## First Target System

So I decided to roll my own <a href="http://www.erlang.org/doc/system_principles/create_target.html" target="_blank">First Target System</a>. The fine manual says:

> Often it is not desirable to use an Erlang/OTP system as is. A developer may create new Erlang/OTP compliant applications for a particular purpose, and several original Erlang/OTP applications may be irrelevant for the purpose in question. Thus, there is a need to be able to create a new system based on a given Erlang/OTP system, where dispensable applications are removed, and a set of new applications that are included in the new system. Documentation and source code is irrelevant and is therefore not included in the new system.
>
Another advantage of creating your own target system is that you get a guaranteed environment to deploy your code into - you can simply untar your system onto any machine (of the same platform/architecture) and it will run just like it ran on your test machine. Same exact version of Erlang, same exact libraries, same config.

After following along with the manual, I had a shiny new target system - but much to my dismay I had a R13B directory under releases/, and trying to do an application upgrade resulted in the same problem as before: release_handler was still looking for an appup file under releases/R13B/ instead of releases/myapp/.

I tried copying my .appup to R13B, and creating a valid blank appup, to no avail.  So..

## Rebar to the rescue... sort of

[Rebar](http://bitbucket.org/basho/rebar/wiki/Home) is an erlang build tool with a few nifty tricks (thanks <a title="Buy this man a pint." href="http://dizzyd.com/" target="_blank">Dave</a> and the gang). Documentation/examples are still a little sparse, but one thing it does have in spades is a way to build Erlang target systems in one command: *./rebar generate*

To use rebar to make a target system you'll need a working OTP style application in the <a title="OTP Design principles: follow them!" href="http://erlang.org/doc/design_principles/applications.html#id69069" target="_blank">usual layout</a> (src,priv,ebin,include) with a valid .app file. Here are the <a href="http://bitbucket.org/basho/rebar/wiki/ReleaseHandling" target="_blank">rebar release handling docs</a>.

Okay. So now we have a nice target system (courtesy of rebar); your rel/&lt;appname&gt; directory will look like this:

```console
drwxr-xr-x  2 rj rj   21 2010-09-15 02:49 bin
drwxr-xr-x  8 rj rj   70 2010-09-15 02:48 erts-5.8
drwxr-xr-x  2 rj rj   37 2010-09-15 02:49 etc
drwxr-xr-x 27 rj rj 4096 2010-09-15 02:49 lib
drwxr-xr-x  3 rj rj   17 2010-09-15 02:49 log
drwxr-xr-x  3 rj rj   43 2010-09-15 02:48 releases
```

The lib/ directory contains stdlib, sasl, kernel, your application and any other deps you specified; The releases/ directory is reassuring empty; bin/ contains a handy nodetool script to start and stop your app; sasl is configured to log to a file. Rebar just saved you a few hours of fiddling to get things up and running. But you're not out of the woods yet..

## Deploying updates to rebar-packaged target system

This was the tricky non-obvious bit. Rebar was great at getting the first system with my app deployed, however it doesn't really offer much to help you install an update to your running system.

I wrote a script <strong>regen.sh</strong> to use instead of "rebar generate", which post-processes the directory structure rebar generates, to make it more amenable to deploying upgrades:

```bash
!/bin/bash
echo "This will nuke rel/irccloud and regenerate using rebar.. [enter to continue]"
read
rm -rf rel/irccloud
set -e
./rebar compile
./rebar generate
cd rel/irccloud/lib/
echo -n "Unpacking .ez files"
for f in *.ez
do
echo -n "."
unzip $f &gt; /dev/null
rm $f
done
echo
cd ../releases/
# Get the version of the only release in the system, our new app:
VER=`find . -maxdepth 1 -type d | grep -vE '^\.$' | head -n1 | sed 's/^\.\///g'`
echo "Ver: ${VER}, renaming .rel + .boot files correctly"
cd "${VER}"
mv irccloud.boot start.boot
mv irccloud.rel "irccloud-${VER}.rel"
cd ../../../
echo "OK"
```

This does three important things:
1. Unpack the *.ez files (zip files of applications in lib/), since upgrades with release_handler didn't work otherwise;
2. rename irccloud.boot to start.boot, <a title="Erldocs.com is great" href="http://erldocs.com/R14A/sasl/systools.html?i=5&amp;search=systools#make_tar/1" target="_blank">since systools has "start.boot" hardcoded</a> as the correct name of this file for packages;
3. rename irccloud.rel to irccloud-&lt;VERSION&gt;.rel, since this is the standard layout for future packages.

_NB: I think I also had to change the bin/irccloud script to -boot with ﻿<code>$RUNNER_BASE_DIR/releases/$APP_VSN/start</code> instead of <code>$RUNNER_BASE_DIR/releases/$APP_VSN/$SCRIPT</code>_

## Tooling up for easier releases

Now in the top level directory (alongside rel,src,ebin..) I made a releases/ directory, from which I package up new versions ready for deployment.

I'm using a bash script, and some erlang, to facilitate packaging up new releases. Note that it adds, using erl -pz, the paths to the current .app file and beams, and the previous .app file and beams.

```bash
#!/bin/bash
set -e
ERL_ROOT=$1
OLDVER=$2
VER=$3
cd ..
./rebar compile
cd -
OLDEBIN="${ERL_ROOT}/lib/irccloud-${OLDVER}/ebin/"
echo "Fetching previous rel file: irccloud-${OLDVER}.rel from ${ERL_ROOT}/releases/${OLDVER}"
cp "${ERL_ROOT}/releases/${OLDVER}/irccloud-${OLDVER}.rel" "irccloud-${OLDVER}.rel"

erl -pz ../ebin/ -pz "${OLDEBIN}" -pz ../deps/*/ebin -noshell \
-run release_helper go irccloud $VER ../ebin $OLDVER "${OLDEBIN}" \
| grep -v 'Source code not found'

echo "Release ${VER} packaged"
```

### release_helper.erl

```erlang
% Check some stuff, write the .rel file, generate boot scripts and relup, make tar
-module(release_helper).
-export([go/1]).

-define(RELAPPS, [  kernel, 
                    stdlib, 
                    sasl, 
                    crypto, 
                    ssl, 
                    inets,
                    public_key, 
                    compiler,
                    syntax_tools,
                    edoc,
                    eunit,
                    xmerl,
                    epgsql, 
                    mochiweb]).

appver(A) when is_atom(A) ->
  application:load(A),
  io:format("Version of '~p'..", [A]),
  {value, {A, _, Ret}} = lists:keysearch(A, 1, application:loaded_applications()),
  io:format("~p~n", [Ret]),
  Ret.

check_appfile_version(File, AppName, ExpectedVer) ->
  io:format("Verifying .app file contains correct version..",[]),
  {ok, [{application, AppName, Props}]} = file:consult(File),
  case proplists:get_value(vsn, Props) of
    ExpectedVer -> io:format("ok~n",[]), ok;
    FoundVer    -> io:format("FAIL~nApp file contains ver: ~s but expected: ~s~n", 
                             [FoundVer, ExpectedVer]),
                   halt(1)
  end.

go(Args) -> 
  [Name, Version, Ebin, PVersion, _PEbin] = Args,
  io:format("release_helper running for: ~p, oldversion: ~s, new version: ~s~n", 
            [Name, PVersion, Version]),
  ok        = check_appfile_version(Ebin ++ "/" ++ Name ++ ".app", 
                                    list_to_atom(Name), Version),
  Erts      = erlang:system_info(version),
  Vsn       = io_lib:format("~s-~s", [Name, Version]),
  PrevVsn   = io_lib:format("~s-~s", [Name, PVersion]),
  io:format("version: '~s'~n", [Version]),
  AppVers   = [{A,appver(A)} || A <- ?RELAPPS],
  Rel       = 
"{release, 
    {\"~s\", \"~s\"}, 
    {erts, \"~s\"}, 
    [   {~w, \"~s\"},~n        "
    ++
    string:join([io_lib:format("{~w, \"~s\"}",[A,AV]) || {A,AV} <- AppVers], 
                ",\n        ")
    ++
"\n    ]\n}.\n",
  RelText   = io_lib:format(Rel, [Name, Version, Erts, list_to_atom(Name), Version]),
  Relfile   = io_lib:format("~s.rel", [Vsn]),
  io:format("Writing ~s~n", [Relfile]),
  {ok, Fs}  = file:open(Relfile, [write]),
  io:format(Fs, RelText, []),
  file:close(Fs),
  io:format("make_script(~s)..~n", [Vsn]),
  ok = systools:make_script(Vsn),
  case PVersion of 
    "0" -> io:format("Previous version 0, not generating relup!~n", []);
    _   -> ok = systools:make_relup(Vsn, [PrevVsn], [PrevVsn])
  end,
  ok = systools:make_tar(Vsn),
  halt().
```

## Making a second release

<p>Here's how I currently make a new release that can be deployed with a live-upgrade:</p>
<ol>
<li>Fix bugs, change some stuff, add features etc;</li>
<li>Update ebin/irccloud.app to increase the version number, update the modules list if needed;</li>
<li>Create/modify ebin/irccloud.appup to tell release_handler how to upgrade from the previous version to this new version. (See: Appup Cookbook);</li>
<li><code>cd releases/</code></li>
<li><code>./release_helper.sh "../rel/irccloud" "1" "2"</code> # (where "1" was the version of the first release using rebar generate, "2" is the new one we want to package)</li>
</ol>
<p>Now I have irccloud-2.tar.gz in the releases/ directory, ready to be deployed. My packaging scripts also tag the release in git and a few other things I've omitted for clarity.</p>
<h2>Deploying the release</h2>
<ol>
<li><code>$ cp irccloud-2.tar.gz ../rel/irccloud/releases</code> # or copy to whereever you put your target system on the production box instead of ../rel/irccloud</li>
<li><code>erl&gt; release_handler:unpack_release("irccloud-2").</code></li>
<li><code>erl&gt; release_handler:install_release("2").</code></li>
<li><code>erl&gt; release_handler:which_releases().</code></li>
<li><code>erl&gt; release_handler:make_permanent("2").</code></li>
</ol>
<p>Step 3 does the upgrade, and runs the appup stuff - hope you tested on your development rig before deploying to production!<br />
If you wrote your .appup properly, it is also safe to downgrade - just do <code>release_helper:install_release("1").</code> after step 3.</p>
<p>Note, you must run erl with "-boot start_sasl" to use release_manager. I ran this from the same shell as "bin/irccloud console" which already had sasl running.</p>
<h2>Mochiweb caveat</h2>
<p>I'm using the excellent <a title="Mochiweb: I love it." href="http://github.com/mochi/mochiweb" target="_blank">Mochiweb HTTP library</a> in my application. Here's the child specification from my top level supervisor:</p>
<pre lang="erlang">{irccloud_web,
    {irccloud_web, start, [WebConfig]},
     permanent, 5000, worker, dynamic}</pre>
<p>Note that it says 'dynamic' in place of the modules list. Here's what the <a href="http://erldocs.com/R14A/stdlib/supervisor.html">fine manual</a> says:</p>
<blockquote><p><strong>Modules</strong> is used by the release handler during code replacement to determine which processes are using a certain module. As a rule of thumb Modules should be a list with one element [Module], where Module is the callback module, if the child process is a supervisor, gen_server or gen_fsm. If the child process is an event manager (gen_event) with a dynamic set of callback modules, Modules should be dynamic. See OTP Design Principles for more information about release handling.</p></blockquote>
<p>What it doesn't say is that release_handler will timeout and screw your install because it can't find the list of modules when dynamic is specified, unless you answer correctly.</p>
<p>Here's how to appease release_handler if you have a dynamic modules list: <a href="http://github.com/RJ/mochiweb/commit/931c5fb769be844c307a51596898ca6c55998219">http://github.com/RJ/mochiweb/commit/931c5fb769be844c307a51596898ca6c55998219</a></p>
<h2>In Conclusion</h2>
<p>In general, this was a rather painful experience.</p>
<p>What I'd like to be able to do is something like this:</p>
<ol>
<li>"rebar make_tar newver=&lt;newversion&gt;"</li>
<li>version in the .app file is automatically incremented for me, modules list is updated. (open $EDITOR for confirmation/additional tweaks)</li>
<li>Unless I manually wrote it already, .appup is automatically populated with {load_module, Mod} for each module that changed since last version was packaged, opened in $EDITOR so I can tweak/review.</li>
<li>irccloud-&lt;newver&gt;.tar.gz is created</li>
</ol>
<p>Maybe I've missed something, and there's a much easier way to package subsequent releases with rebar, or something else I've overlooked.<br />
How do you package and deploy your Erlang applications to do live-updates?</p>
