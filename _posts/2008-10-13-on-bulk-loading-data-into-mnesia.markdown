--- 
layout: post
title: On bulk loading data into Mnesia
permalink: /article/on-bulk-loading-data-into-mnesia
tags: 
- programming
- erlang
- hacks
- mnesia
wordpress_id: 82
---
<i>Consider this a work-in-progress; I will update this post if I find a 'better' way to do fast bulk loading</i>

The time has come to replace my ets-based storage backend with something non-volatile. I considered a dets/ets hybrid, but I really need this to be replicated to at least a second node for HA / failover. Mnesia beckoned. 

The problem:
<ul>
	<li>15 million [fairly simple] records</li>
	<li>1 Mnesia table: bag, disc_copies, just 1 node, 1 additional index</li>
	<li>Hardware is a quad-core 2GHz CPU, 16GB Ram, 8x 74Gig 15k rpm scsi disks in RAID-6</li>
	<li>Takes ages* to load and spews a load of "Mnesia is overloaded" warnings</li>
</ul>

> *My definition of 'takes ages':* Much longer than PostgreSQL <code>\copy</code> or MySQL <code>LOAD DATA INFILE</code>

At this point all I want is a quick way to bulk-load some data into a disc_copies table on a single node, so I can get on with running some tests.

Here is the table creation code:
```erlang
mnesia:create_table(subscription,
               [
               {disc_copies, [node()]},
               {attributes, record_info(fields, subscription)},
               {index, [subscribee]}, %index subscribee too
               {type, bag}
               ]
               ).
```

The <code>subscription</code> record is fairly simple: 
<code>{subscription, subscriber={resource, user, 123}, subscribee={resource, artist, 456}}</code>

I'm starting erlang like so: 
<code>erl +A 128 -mnesia dir '"/home/erlang/mnesia_dir"' -boot start_sasl</code>

The interesting thing there is really the <code>+A 128</code> - this spreads the cpu load better between the 4 cores.

<h3>Attempt 0) 'by the book' one transaction to rule them all</h3>

Something like this:
```erlang
mnesia:transaction(fun()-> [ mnesia:write(S) || S <- Subs ] end)
```

* Time taken: <b>Too long, I gave up after 12 hours</b>
* Number of "Mnesia overloaded" warnings: <b>lots</b>
* Conclusion: <b>Must be a better way</b>
* TODO: actually run this test and time it.

<h3>Attempt 1) dirty_write</h3>

There isn't really any need to do this in a transaction, so I tried dirty_write. 

```erlang
[ mnesia:dirty_write(S) || S <- Subs ]
```

And here's the warning in full:

```
=ERROR REPORT==== 13-Oct-2008::16:53:57 ===
Mnesia('mynode@myhost'): ** WARNING ** Mnesia is overloaded: {dump_log, write_threshold}
```


* Time taken: <b>890 secs</b>
* Number of "Mnesia overloaded" warnings: <b>lots</b>
* Conclusion: <b>Workable, but nothing to boast about. Those warnings are annoying</b>

<h3>Attempt 2) dirty_write, defer index creation</h3>

A common trick with traditional RDBMS would be to bulk load the data into the table and add the indexes afterwards. In some scenarios you can avoid costly incremental index update operations. If you are doing this in one gigantic transaction it shouldn't matter, and I'm not really sure how mnesia works under the hood (something I plan to rectify if I end up using it for real). 

I tried a similar approach by commenting out the <code>{index, [subscribee]}</code> line above, doing the load, then using <code>mnesia:add_table_index(subscriber, subscribee)</code> afterwards to add the index once all the data was loaded. Note that mnesia was still building the primary index on the fly, but that can't be helped.

* Time taken: <b>883 secs</b> (679s load + 204s index creation)
* Number of "Mnesia overloaded" warnings: <b>lots</b>
* Conclusion: <b>Insignificant, meh</b>

<h3>Attempt 3) mnesia:ets() trickery</h3>

This is slightly perverted, but I tried it because I was suspicious that incrementally updating the on-disk data wasn't especially optimal. The idea is to make a ram_only table and use the mnesia:ets() function to write directly to the ets table (doesn't get much faster than ets). The table can then be converted to disc_copies. There are caveats - to quote The Fine Manual:

> Call the Fun in a raw context which is not protected by a transaction. The Mnesia function call is performed in the Fun are performed directly on the local ets tables on the assumption that the local storage type is ram_copies and the tables are not replicated to other nodes. Subscriptions are not triggered and checkpoints are not updated, but it is extremely fast.

I can live with that. I don't mind if replication takes a while to setup when I put this into production - I'll gladly take any optimisations I can get at this stage (testing/development).

Loading a list of <code>subscriptions</code> looks like this:
```erlang
mnesia:ets(fun()-> [mnesia:dirty_write(S) || S <- Subs] end).
```

And to convert this into disc_copies once data is loaded in:
```erlang
mnesia:change_table_copy_type(subscription, node(), disc_copies).
```

* Time taken: <b>745 secs</b> (699s load + 46s convert to disc_copies)
* Number of "Mnesia overloaded" warnings: <b>none!</b>
* Conclusion: <b>Fastest yet, bit hacky</b>

<h2>Summary</h2>

At least the ets() trick doesn't spew a million warnings. I also need to examine the output of <code>mnesia:dump_to_textfile</code> and see if loading data from that format is any faster.

#### TODO:
<ul>	
<li>Examine / test using the dum_to_textfile method</li>
<li>Run full transactional load and time it</li>
<li>Try similar thing with PostgreSQL</li>
</ul>

