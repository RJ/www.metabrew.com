--- 
layout: post
title: How we use IRC at Last.fm
permalink: /article/how-we-use-irc-at-lastfm
tags: 
- programming
- lastfm
- irc
- java
wordpress_id: 207
---
Everyone that works at Last.fm is typically connected to our IRC server. We have different channels per team, as well as a company-wide channel, and a few channels dedicated to automated monitoring.

Sometimes it makes much more sense to discuss / ask questions on IRC instead of email, and it's useful to be able to raise people who are not in the office. That said, the main reason I'm writing this post is to mention the dev-support bot we use: irccat.

### IRCCat - Development support bot

The irccat bot joins all your channels, and waits for messages on a specified ip:port on your internal network. Anything you send to that port will be sent to IRC by the bot. IRCCat - as in, `cat` to IRC.

Using netcat, you can easily send events to irc from shell scripts:

```bash
echo "Something just happened" | nc -q0 somemachine 12345
```

That will send to the default channel only (first in the config file). You can direct messages to specific combinations of channels (#) or users (@) like so:


```bash
echo "#syschan Starting backup job" | nc -q0 somemachine 12345
```

or:

```bash
echo "#musicteam,#legal,@alice New album uploaded: ..." | nc -q0 somemachine 12345
```

Some of the things we automatically send to appropriate IRC channels:
<ul>
	<li>SVN commits</li>
	<li>JIRA issue tracker updates</li>
	<li>Nagios alerts for monitored hosts and services</li>
	<li>Deployment notices to testing/staging/production</li>
	<li>Results of automated tests if something bad happens</li>
	<li>Links to pics from security camfeed when someone opens the office door out of hours</li>
</ul>

We also post messages from automated backup jobs etc, which helps correlate such events with any unusual load spikes or glitches in usually-smooth graphs.

In addition to providing a cat-to-irc conduit, irccat will also hand off commands to a script you can provide. We use this to expose lookup tools and some admin functions to our support staff and developers. The handler script we use is PHP, and has access to our core website libs. Typing "?pokereleasenode", "?lookup user RJ" or "?uncache artist Radiohead" is faster than writing a throw-away script, more accessible to non-developers, less hassle than a web interface and creates a public log so people can see what's going on.

The bot is written in Java, it's easy to build and configure, all the deps are included:

<a title="IRCcat source on GitHub" href="http://github.com/RJ/irccat/tree/master" target="_blank">http://github.com/RJ/irccat/tree/master</a>
