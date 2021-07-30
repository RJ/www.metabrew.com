--- 
layout: post
title: "ssh hack: connect directly to machine via a firewall box"
permalink: /article/ssh-hack-connect-directly-to-machine-via-a-firewall-box
tags: 
- hacks
- hack
- ssh
wordpress_id: 196
---

It's common to have to ssh to firewall / gateway machine, then ssh to the machine you want to work on within a server network.

Typically you'd do this from your local machine:

```console
$ ssh firewall.example.com
Password:
$ ssh my-private-host
```

I finally got bored of doing this, and created the following file:
*/usr/bin/sssh*
```bash
#!/bin/bash
ssh -oproxycommand="ssh -q firewall.example.com nc -q0 %h %p" $*
```

Now I can use the <code>sssh</code> command to connect to hosts using the firewall machine as a proxy. Like most good hacks, this uses netcat.

Eg:
```console
$ sssh 10.1.2.3 
```

Will connect me directly to a machine on the server network, via the firewall box. Seeing as it passes all parameters to ssh (the <code>$*</code> bit) you can do port forwards and X-forwarding as usual too:

```console
$ sssh -L 5432:localhost:5432 my-vm
```

This lets me tunnel the port for a PostgreSQL running on my development vm (<code>my-vm</code>) in a single command. I have all my keys installed, so no passwords needed - I estimate this will save me about 60 seconds every day.
