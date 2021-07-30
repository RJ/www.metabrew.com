--- 
layout: post
published: true
title: "Erlang rebar tutorial: generating releases and upgrades"
permalink: /article/erlang-rebar-tutorial-generating-releases-upgrades
tags: 
- programming
- erlang
- otp
- appup
- rebar
- releasehandler
---

During my experiments with rebar, I made a simple example app for testing
upgrades and releases. This article will walk you through using rebar to create
an application, lay it out properly, package and deploy it, and create and
install new versions without downtime. 

The code accompanying this article is in various branches of <a href="https://github.com/RJ/erlang_rebar_example_project">github.com/RJ/erlang_rebar_example_project</a>.

> **N.B.** The <a href="http://www.erlang.org/doc/design_principles/des_princ.html">OTP
> Design Principles</a> docs are a good place to start if you want an overview of
> the OTP approach to Erlang apps and releases. However, rebar isn't (yet) part
> of OTP, so consider that background reading. Rebar makes things much easier.


## Creating the project 

Build rebar:

```console
$ cd ~/src
$ git clone https://github.com/basho/rebar.git
Initialized empty Git repository in /tmp/rebar/.git/
remote: Counting objects: 2651, done.
remote: Compressing objects: 100% (1344/1344), done.
remote: Total 2651 (delta 1540), reused 2227 (delta 1174)
Receiving objects: 100% (2651/2651), 622.99 KiB | 495 KiB/s, done.
Resolving deltas: 100% (1540/1540), done.
$ cd rebar && make
...snip....
==> rebar (compile)
Congratulations! You now have a self-contained script called "rebar" in
your current working directory. Place this script anywhere in your path
and you can use rebar to build OTP-compliant apps.
```

Now we'll make a project directory called "dummy_proj", copy rebar into it, and use rebar to generate a skeleton application:

```console
$ mkdir -p ~/src/dummy_proj/apps
$ cd ~/src/dummy_proj/
$ cp ../rebar/rebar .
$ cd apps
$ ../rebar create-app appid=dummy_proj
==> dummy_proj (create-app)
Writing src/dummy_proj.app.src
Writing src/dummy_proj_app.erl
Writing src/dummy_proj_sup.erl
```

To the skeleton, I added a basic gen_server called dummy_proj_server, which just keeps track of the
number of times it was poked, i.e. it holds some state, for demonstration purposes.

I also renamed dummy_proj_app.erl to just dummy_proj.erl, and added a start/0 function, which is useful when starting the application during developement, when not running from a generated release.


### Compiling with rebar

You need a rebar.conf, place this in the top-level project directory:

```erlang
{sub_dirs, [
            "apps/dummy_proj",
            "rel"
           ]}.
{erl_opts, [debug_info, fail_on_warning]}.

{require_otp_vsn, "R14"}.
```

And now to compile, you do:

```console
$ ./rebar compile
==> dummy_proj (compile)
Compiled src/dummy_proj_sup.erl
Compiled src/dummy_proj.erl
Compiled src/dummy_proj_server.erl
==> rel (compile)
==> dummy_proj (compile)
```

Note that you now have .beam files in apps/dummy_proj/ebin/, and the .app.src generated apps/dummy_proj/ebin/dummy_proj.app for you, with a complete modules list.

> **N.B.** I made a simple Makefile that calls 'rebar compile', because I'm too used to typing make. Find it in the git repo.


### Running your app (development)

Here's how you can start the application (and sasl, for nice error reporting):

```console
$ erl -pa apps/*/ebin -boot start_sasl -s dummy_proj
...snip...
=INFO REPORT==== 16-Mar-2011::14:17:04 ===
Starting dummy_proj application...

=PROGRESS REPORT==== 16-Mar-2011::14:17:04 ===
          supervisor: {local,dummy_proj_sup}
             started: [{pid,<0.45.0>},
                       {name,dummy_proj_server},
                       {mfargs,{dummy_proj_server,start_link,[]} },
                       {restart_type,permanent},
                       {shutdown,5000},
                       {child_type,worker}]

=PROGRESS REPORT==== 16-Mar-2011::14:17:04 ===
         application: dummy_proj
          started_at: nonode@nohost
Eshell V5.8.1  (abort with ^G)
1> dummy_proj_server:num_pokes().
0
2> dummy_proj_server:poke().     
{ok,1}
3> dummy_proj_server:poke().
{ok,2}
4> dummy_proj_server:num_pokes().
2
5>  
```

Now you have a nice sensibly structured Erlang project that you can compile with rebar. Exit the VM with q(). and let's use rebar to package it up, so you can deploy it on a production box.

## Generating your first release

When you generate a release with rebar, and indeed if you use the erlang tools manually (not recommended, just use rebar), you end up with the whole Erlang VM and required libraries packaged up under one directory. 

This means you have a self-contained environment containing Erlang, the OTP libraries you need, and all your application code and dependencies. You can just tar it up, ship it over to another machine (of the same architecture, eg GNU/Linux 64-bit), and run it there.

### Creating a node config

Use rebar to create a default node configuration in a rel subdirectory:

```console
$ mkdir rel
$ cd rel/
$ ../rebar create-node nodeid=dummynode
==> rel (create-node)
Writing reltool.config
Writing files/erl
Writing files/nodetool
Writing files/dummynode
Writing files/app.config
Writing files/vm.args
```

You need to edit reltool.config a little; point to to your apps directory, and
make sure the version number matches your .app.src file.
You should also add dummy_app to the list of applications that are started as part of the
release. 
Here's <a href="https://github.com/RJ/erlang_rebar_example_project/blob/v1/rel/reltool.config" target="v1rel">reltool.conf from my v1 tag</a>

### Generating the release

Back in the top level directory, just run:

```console
$ ./rebar generate
==> rel (generate)
```

Now have a look in rel/dummynode. This is the release directory containing everything you need to run your application.

We are going to be creating more releases later, so rename rel/dummynode to rel/dummynode_first, and then launch it using the handy script that rebar created for us:

```console
$ cd rel/dummynode_first
$ ./bin/dummynode console
...snip...
Erlang R14B (erts-5.8.1) [source] [64-bit] [smp:8:8] [rq:8] [async-threads:5] [hipe] [kernel-poll:true
=INFO REPORT==== 16-Mar-2011::13:29:59 ===
Starting dummy_proj application...
Eshell V5.8.1  (abort with ^G)
(dummynode@127.0.0.1)1> 
(dummynode@127.0.0.1)1> dummy_proj_server:num_pokes().
0
(dummynode@127.0.0.1)2> dummy_proj_server:poke().     
{ok,1}
(dummynode@127.0.0.1)3> dummy_proj_server:poke().
{ok,2}
(dummynode@127.0.0.1)4> dummy_proj_server:num_pokes().
2
(dummynode@127.0.0.1)5> 
```

Now the release is running, we never want to have to restart it ever again, so open up another console because we want to leave that running whilst we work on version 2.

> **N.B.** In a production environment, you would start with "./bin/dummynode start" so it runs in the background, and use "dummynode attach" to get a console.

Check the <a href="https://github.com/RJ/erlang_rebar_example_project/blob/v1/">'v1 branch'</a> on github for code up to this point.

## Upgrading to Version 2

Add the poke_twice() function to dummy_proj_server.

Change the version from "1" to "2", in both apps/dummy_proj.app.src and rel/reltool.conf.

Here's the github <a href="https://github.com/RJ/erlang_rebar_example_project/compare/v1...v2#diff-3">diff between v1...v2</a>

Erlang application version numbers can be any string - I tend to use a date format with letter: "20110316a", but you can use any scheme you want. 
I tag releases in git with the same version as the erlang application. 
We'll just use "1", "2", "3" here for simplicity.

> **N.B.** If you use <code>{vsn, git}</code> as the version in your .app.src, rebar will get the version string from the closest git tag.

Now build the new version:
```console
$ ./rebar compile
$ ./rebar generate
```

So now you have rel/dummy_proj, containing a full release (VM included) of version 2. 
If you don't care about online-upgrades, you could just kill your version 1 VM, and start version 2 from this new release directory.

### Writing the .appup upgrade instructions

In order to make an upgrade, you must have a valid .appup file. 
This tells the erlang release_handler how to upgrade and downgrade between specific versions of your application.

Rebar has a (relatively new) command called 'generate-appups'. 
I'll show how it works, but ultimately we'll write our .appup manually, and keep it in our project directory (in git).

```console
$ ./rebar generate-appups previous_release=dummynode_first
==> rel (generate-appups)
Generated appup for dummy_proj
Appup generation complete
$ cat ./rel/dummynode/lib/dummy_proj-2/ebin/dummy_proj.appup
%% appup generated for dummy_proj by rebar ("2011/03/16 13:37:43")¬                                   
{"2", [{"1", [{update,dummy_proj_server,{advanced,[]}}]}], [{"1", []}]}.¬
```

Get rid of the autogenerated one, and create the appup file manually, in apps/dummy_proj/ebin/dummy_proj.appup:

```erlang
{"2", 
    %% Upgrade instructions from 1 to 2
    [{"1", [
        {load_module, dummy_proj_server}    
    ]}], 
    %% Downgrade instructions from 2 to 1
    [{"1",[
        {load_module, dummy_proj_server}    
    ]}]
}.
```

This .appup contains instructions for upgrading and downgrading between versions "2" and "1".
Typically the downgrade instructions are the reverse of the upgrade instructions. 
Since we just added a function to our server process, without changing any internal state, we can just use load_module instructions. The Appup Cookbook explains the various upgrade instructions in depth.

Now generate again, overwriting the previous version 2. This will just make sure the .appup is part of the release directory:

```console
$ ./rebar generate -f
```

And now, create the upgrade package:

```console
$ ./rebar generate-upgrade previous_release=dummynode_first
==> rel (generate-upgrade)
dummynode_2 upgrade package created
```

The generate-upgrade command will look for rel/dummynode as the current version, and rel/dummynode_first as the previous version. 
It should have created the upgrade .tar.gz in rel:

```console
$ ls -lh rel/
total 15M
drwxr-xr-x 8 rj rj 4.0K 2011-03-16 13:42 dummynode
drwxr-xr-x 8 rj rj 4.0K 2011-03-16 13:29 dummynode_first
-rw-r--r-- 1 rj rj  14M 2011-03-16 13:45 dummynode_2.tar.gz
drwxr-xr-x 2 rj rj 4.0K 2011-03-16 13:11 files
-rw-r--r-- 1 rj rj  922 2011-03-16 13:36 reltool.config
```

### Installing the upgrade package

You should still have the VM running from dummynode_first. 
Make sure you called poke(), so the internal state is something other than the default. 
This will help illustrate that the upgrade worked seamlessly.

Copy the upgrade package to the releases directory of the running release:

```console
$ cp rel/dummynode_2.tar.gz rel/dummynode_first/releases
```

Now, at the Erlang console where version 1 is running, we use release_handler to check which releases are currently available, and install our new one:

```console
(dummynode@127.0.0.1)5> release_handler:which_releases().
[{"dummynode","1",[],permanent}]
(dummynode@127.0.0.1)6> release_handler:unpack_release("dummynode_2").
{ok,"2"}
(dummynode@127.0.0.1)7> release_handler:install_release("2").
{ok,"1",[]}   
(dummynode@127.0.0.1)8> dummy_proj_server:num_pokes().
2
(dummynode@127.0.0.1)9> dummy_proj_server:poke_twice().
{ok,4}
(dummynode@127.0.0.1)10> dummy_proj_server:num_pokes(). 
4
(dummynode@127.0.0.1)11> release_handler:which_releases().
[{"dummynode","2",
  ["kernel-2.14.1","stdlib-1.17.1","dummy_proj-2",
   "sasl-2.1.9.2","compiler-4.7.1","crypto-2.0.1",
   "syntax_tools-1.6.6","edoc-0.7.6.7","et-1.4.1","gs-1.5.13",
   "hipe-3.7.7","inets-5.5","mnesia-4.4.15","observer-0.9.8.3",
   "public_key-0.8","runtime_tools-1.8.4.1","ssl-4.0.1",
   "tools-2.6.6.1","webtool-0.8.7","wx-0.98.7","xmerl-1.2.6"],
  current},
 {"dummynode","1",[],permanent}]
```

The upgrade worked; you can see that the num_pokes() was preserved, and that the new poke_twice() function is available.

release_handler shows our version 2 as "current", and the original version 1 as "permanent".
This means that although version 2 is running right now, if you restart the VM, version "1" will be booted up.

If you are happy with the upgrade, make it permanent, meaning it will boot instead of version 1 if you restart the VM:

```console
(dummynode@127.0.0.1)12> release_handler:make_permanent("2").
```

Check the <a href="https://github.com/RJ/erlang_rebar_example_project/blob/v2/">'v2 branch'</a> on github for code up to this point.

## Version 3 and beyond

The upgrade from v1 to v2 was simple: we just added a fun without changing the
internal #state{} record.

Erlang .appup files can do all sorts of clever stuff, allowing you to rewire
your running applications during the upgrade process. 

The <a
href="http://www.erlang.org/doc/design_principles/appup_cookbook.html">Appup
Cookbook</a> details the various commands you can put in your .appup. 

Let's do an upgrade with a more complex appup - we'll change the #state record in the dummy_proj_server process.

For version 3, we'll track prods as well as pokes, which will require another
field in the state record.

Here's the github <a href="https://github.com/RJ/erlang_rebar_example_project/compare/v2...v3#diff-2">diff between v2...v3</a>.

Check out the addition to .appup for this release:

```erlang
{"3", 
    %% Upgrade instructions
    [{"2", [
        {update,dummy_app_server,{advanced,[from2to3]}}
    ]}], 
    %% Downgrade instructions
    [{"2",[
        {update,dummy_app_server,{advanced,[from3to2]}}
    ]}]
}.
```

This {update..} directive will result in the code_change function being called
on the dummy_app_server. The purpose of code_change is to change the State from
the old (v2) format, to the new (v3) format.

Although it's not strictly necessary, I pass 'from2to3' as the 'Extra' field in
the code_change call. This can be pattern matched on, and makes it clear in
your code_change code exactly what version upgrade is expected.

### Packaging and upgrading to v3

Move the generated release dir for v2:
```console
$ mv rel/dummynode rel/dummynode_2
```

Compile and generate for v3, then create the upgrade package:
```console
$ ./rebar compile
$ ./rebar generate
$ ./rebar generate-upgrade previous_release=dummynode_2
```

> **N.B.** You need to provide the full, standalone generated release dir
> as the previous_release, you can't use dummynode_first, even though that
> contains version 2 of your release.

As before, copy the upgrade package to the releases directory of the running release:

```console
$ cp rel/dummynode_3.tar.gz rel/dummynode_first/releases
```

Now, at the Erlang console where you upgraded v1 to v2:

```console
(dummynode@127.0.0.1)12> release_handler:unpack_release("dummynode_3").
{ok,"3"}
(dummynode@127.0.0.1)13> release_handler:install_release("3").
{ok,"2",[]}
```

### Congratulations

Now you can deploy hot-code-upgrades the proper OTP way. Ideal for complex or
large upgrades that change internal state or do require special upgrade hooks.
Read the Appup Cookbook a few times, and **test your upgrade packages in
a staging environment before deploying**. You can just tar up and copy the live
environment to your staging box, to get an exact clone of the production system
to test upgrades against.


> **Warning: a current issue with downgrades**
> 
> To downgrade, you just install a previous release. However, there is currently
> a bug where release_handler chokes during downgrades to the first version,
> because of a discrepancy in the naming of .boot files in the release.
> release_handler has start.boot hardcoded, but rebar will generate appname.boot,
> with start.boot as a symlink. If you need to do downgrades, test this carefully
> before deploying; you may need to manually rename the boot file.

## Cowboying out quick fixes

Appup files and generating releases is rather heavyweight. Here's an overview
of the process I'm using on <a href="https://www.irccloud.com/">IRCCloud</a> at the moment

#### Complex upgrades are proper releases, with .appup

No way around it; bit of a pain to create and test, but glorious when you pull off a complex upgrade with zero downtime.
Releases are tagged in git with the datetime and letter version, eg:
v20110324a.
If I do a second release that day, v20110324b.

#### 'Hotfixes' preserve my sanity

If I make a quick fix that simply requires reloading a module with no risk of
instability, I do the following:

1. Reset code to currently deployed tag, egv 20110324a
2. Write the fix
3. Commit as v200110324a-hotfix1
4. Build this version of the specific module that's changed, and copy it into
   the production environment, eg to:
   /somewhere/dummyapp/lib/dummyapp-20110324a/ebin/
5. Reload the module, eg by using l(module_name). at the shell.

It's of vital importance to have a repeatable process, so you know exactly which version of
code (ie, the git tag) is currently running in production. If you can't be
sure, then it's much harder to write successful upgrade code later on.

This process gives a reasonable balance between periodic 'proper' releases,
during which any complex changes are made that rewire internal state, and quick
fixes that just require a module reload.

## Pitfalls to avoid

In the IRCCloud app, there's one place that I don't use a supervisor, but wish
I had; the user process acts as a sort of supervisor for connection processes, 
because I needed exponential backoff / more control over restarting crashed children.

Don't do that. release_handler isn't aware of the child processes I spawn
myself, so I can't use the normal appup process to call code_change.



