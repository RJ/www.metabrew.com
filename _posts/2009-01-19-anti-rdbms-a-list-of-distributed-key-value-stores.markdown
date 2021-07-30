--- 
layout: post
title: "Anti-RDBMS: A list of distributed key-value stores"
permalink: /article/anti-rdbms-a-list-of-distributed-key-value-stores
tags: 
- programming
- erlang
- hashing
- java
- databases
- dht
- nosql
wordpress_id: 209
---
*Please Note: this was written January 2009 - see the comments for updates and additional information. A lot has changed since I wrote this.*

<hr/>

Perhaps you're considering using a dedicated key-value or document store instead of a traditional relational database. Reasons for this might include:
<ol>
	<li>You're suffering from Cloud-computing Mania.</li>
	<li>You need an excuse to 'get your Erlang on'</li>
	<li>You heard CouchDB was cool.</li>
	<li>You hate MySQL, and although PostgreSQL is much better, it still doesn't have decent replication. There's no chance you're buying Oracle licenses.</li>
	<li>Your data is stored and retrieved mainly by primary key, without complex joins.</li>
	<li>You have a non-trivial amount of data, and the thought of managing lots of RDBMS shards and replication failure scenarios gives you the fear.</li>
</ol>
Whatever your reasons, there are a lot of options to chose from. At Last.fm we do a lot of batch computation in Hadoop, then dump it out to other machines where it's indexed and served up over HTTP and <a href="http://developers.facebook.com/thrift/">Thrift</a> as an internal service (stuff like 'most popular songs in London, UK this week' etc). Presently we're using a home-grown index format which points into large files containing lots of data spanning many keys, similar to the Haystack approach mentioned in <a href="http://perspectives.mvdirona.com/2008/06/30/FacebookNeedleInAHaystackEfficientStorageOfBillionsOfPhotos.aspx">this article about Facebook photo storage</a>. It works, but rather than build our own replication and partitioning system on top of this, we are looking to potentially replace it with a distributed, resilient key-value store for reasons 4, 5 and 6 above.

This article represents my notes and research to date on distributed key-value stores (and some other stuff) that might be suitable as RDBMS replacements under the right conditions. I'm expecting to try some of these out and investigate further in the coming months.

#### Glossary and Background Reading
<ul>
<li>
 <a href="http://en.wikipedia.org/wiki/Distributed_hash_table">Distributed Hash Table (DHT)</a> 
 and algorithms such as Chord or Kadmelia
</li>

<li>
  <a href="http://www.allthingsdistributed.com/2007/10/amazons_dynamo.html">Amazon's Dynamo Paper</a>, 
  and <a href="http://www.readwriteweb.com/archives/amazon_dynamo.php">this ReadWriteWeb article about Dynamo</a> which explains why such a system is invaluable
</li>

<li>
    <a href="http://aws.amazon.com/simpledb/">Amazon's SimpleDB Service</a>, 
    and <a href="http://gigaom.com/2007/12/14/amazon-simple-db/">some</a>
    <a href="http://www.satine.org/archives/2007/12/13/amazon-simpledb/">commentary</a>
</li>
<li><a href="http://labs.google.com/papers/bigtable.html">Google's BigTable paper</a></li>
<li><a href="http://en.wikipedia.org/wiki/Paxos_algorithm">The Paxos Algorithm</a> - read this page in order to appreciate that knocking up a Paxos implementation isn't something you'd want to do whilst hungover on a Saturday morning.</li>
</ul>

### The Shortlist
Here is a list of projects that could potentially replace a group of relational database shards. Some of these are much more than key-value stores, and aren't suitable for low-latency data serving, but are interesting none-the-less.

<style type="text/css">
#matrix td{ font-size:90%; vertical-align:top; padding: 3px; } #matrix tr { background: #f0f0f0; } #matrix tr.odd { background: #ddd; } 
#matrix td.bigger {font-size:100%;}
</style>
<table id="matrix" border="0">
<tbody>
<tr class="odd" style="font-weight:bold;">
<td class="bigger">Name</td>
<td>Language</td>
<td>Fault-tolerance</td>
<td>Persistence</td>
<td>Client Protocol</td>
<td>Data model</td>
<td>Docs</td>
<td>Community</td>
</tr>
<tr>
<td style="font-weight:bold" class="bigger"><a class="external text" title="http://project-voldemort.com/" href="http://project-voldemort.com/">Project Voldemort</a></td>
<td>Java</td>
<td>partitioned, replicated, read-repair</td>
<td>Pluggable: BerkleyDB, Mysql</td>
<td>Java API</td>
<td>Structured / blob / text</td>
<td>A</td>
<td>Linkedin, no</td>
</tr>
<tr class="odd">
<td style="font-weight:bold" class="bigger"><a class="external text" title="http://github.com/tuulos/ringo/tree/master" href="http://github.com/tuulos/ringo/tree/master">Ringo</a></td>
<td>Erlang</td>
<td>partitioned, replicated, immutable</td>
<td>Custom on-disk (append only log)</td>
<td>HTTP</td>
<td>blob</td>
<td>B</td>
<td>Nokia, no</td>
</tr>
<tr>
<td style="font-weight:bold" class="bigger"><a class="external text" title="http://code.google.com/p/scalaris/" href="http://code.google.com/p/scalaris/">Scalaris</a></td>
<td>Erlang</td>
<td>partitioned, replicated, paxos</td>
<td>In-memory only</td>
<td>Erlang, Java, HTTP</td>
<td>blob</td>
<td>B</td>
<td>OnScale, no</td>
</tr>
<tr class="odd">
<td style="font-weight:bold" class="bigger"><a class="external text" title="http://kai.wiki.sourceforge.net/" href="http://kai.wiki.sourceforge.net/">Kai</a></td>
<td>Erlang</td>
<td>partitioned, replicated?</td>
<td>On-disk Dets file</td>
<td>Memcached</td>
<td>blob</td>
<td>C</td>
<td>no</td>
</tr>
<tr>
<td style="font-weight:bold" class="bigger"><a class="external text" title="http://github.com/cliffmoon/dynomite/tree/master" href="http://github.com/cliffmoon/dynomite/tree/master">Dynomite</a></td>
<td>Erlang</td>
<td>partitioned, replicated</td>
<td>Pluggable: couch, dets</td>
<td>Custom ascii, Thrift</td>
<td>blob</td>
<td>D+</td>
<td>Powerset, no</td>
</tr>
<tr class="odd">
<td style="font-weight:bold" class="bigger"><a class="external text" title="http://memcachedb.org/" href="http://memcachedb.org/">MemcacheDB</a></td>
<td>C</td>
<td>replication</td>
<td>BerkleyDB</td>
<td>Memcached</td>
<td>blob</td>
<td>B</td>
<td>some</td>
</tr>
<tr>
<td style="font-weight:bold" class="bigger"><a class="external text" title="http://code.google.com/p/thrudb/" href="http://code.google.com/p/thrudb/">ThruDB</a></td>
<td>C++</td>
<td>Replication</td>
<td>Pluggable: BerkleyDB, Custom, Mysql, S3</td>
<td>Thrift</td>
<td>Document oriented</td>
<td>C+</td>
<td>Third rail, unsure</td>
</tr>
<tr class="odd">
<td style="font-weight:bold" class="bigger"><a class="external text" title="http://couchdb.apache.org/" href="http://couchdb.apache.org/">CouchDB</a></td>
<td>Erlang</td>
<td>Replication, partitioning?</td>
<td>Custom on-disk</td>
<td>HTTP, json</td>
<td>Document oriented (json)</td>
<td>A</td>
<td>Apache, yes</td>
</tr>
<tr>
<td style="font-weight:bold" class="bigger"><a class="external text" title="http://code.google.com/p/the-cassandra-project/" href="http://code.google.com/p/the-cassandra-project/">Cassandra</a></td>
<td>Java</td>
<td>Replication, partitioning</td>
<td>Custom on-disk</td>
<td>Thrift</td>
<td>Bigtable meets Dynamo</td>
<td>F</td>
<td>Facebook, no</td>
</tr>
<tr class="odd">
<td style="font-weight:bold" class="bigger"><a class="external text" title="http://hadoop.apache.org/hbase/" href="http://hadoop.apache.org/hbase/">HBase</a></td>
<td>Java</td>
<td>Replication, partitioning</td>
<td>Custom on-disk</td>
<td>Custom API, Thrift, Rest</td>
<td>Bigtable</td>
<td>A</td>
<td>Apache, yes</td>
</tr>
<tr>
<td style="font-weight:bold" class="bigger"><a class="external text" title="http://hypertable.org/" href="http://hypertable.org/">Hypertable</a></td>
<td>C++</td>
<td>Replication, partitioning</td>
<td>Custom on-disk</td>
<td>Thrift, other</td>
<td>Bigtable</td>
<td>A</td>
<td>Zvents, Baidu, yes</td>
</tr>
</tbody></table>
<br/>

### Why 5 of these aren't suitable

What I'm really looking for is a low latency, replicated, distributed key-value store. Something that scales well as you feed it more machines, and doesn't require much setup or maintenance - it should just work. The API should be that of a simple hashtable: set(key, val), get(key), delete(key). This would dispense with the hassle of managing a sharded / replicated database setup, and hopefully be capable of serving up data by primary key efficiently.

Five of the projects on the list are far from being simple key-value stores, and as such don't meet the requirements - but they are definitely worth a mention. 

#### Hadoop 
We're already heavy users of Hadoop, and have been experimenting with Hbase for a while. It's much more than a KV store, but latency is too great to serve data to the website. We will probably use Hbase internally for other stuff though - we already have stacks of data in HDFS.

#### Hypertable
Hypertable provides a similar feature set to Hbase (both are inspired by Google's Bigtable). They recently announced a new sponsor, Baidu - the biggest Chinese search engine. Definitely one to watch, but doesn't fit the low-latency KV store bill either.

#### Cassandra
Cassandra sounded very promising when the source was released by Facebook last year. They use it for inbox search. It's Bigtable-esque, but uses a DHT so doesn't need a central server (one of the Cassandra developers previously worked at Amazon on Dynamo). Unfortunately it's languished in relative obscurity since release, because Facebook never really seemed interested in it as an open-source project. From what I can tell there isn't much in the way of documentation or a community around the project at present.

#### CouchDB
CouchDB is an interesting one - it's a "distributed, fault-tolerant and schema-free document-oriented database accessible via a RESTful HTTP/JSON API". Data is stored in 'documents', which are essentially key-value maps themselves, using the data types you see in JSON.  Read the <a href="http://couchdb.apache.org/docs/overview.html">CouchDB Technical Overview</a> if you are curious how the web's trendiest document database works under the hood. This article on the <a href="http://push.cx/2009/rules-of-database-app-aging">Rules of Database App Aging</a> goes some way to explaining why document-oriented databases make sense. CouchDB can do full text indexing of your documents, and lets you express views over your data in Javascript. I could imagine using CouchDB to store lots of data on users: name, age, sex, address, IM name and lots of other fields, many of which could be null, and each site update adds or changes the available fields. In situations like that it quickly gets unwieldly adding and changing columns in a database, and updating versions of your application code to match. Although many people are using CouchDB in production, their FAQ points out they may still make backwards-incompatible changes to the storage format and API before version 1.0.

#### ThruDB
ThruDB is a document storage and indexing system made up for four components: a document storage service, indexing service, message queue and proxy. It uses Thrift for communication, and has a pluggable storage subsystem, including an Amazon S3 option. It's designed to scale well horizontally, and might be a better option that CouchDB if you are running on EC2. I've heard a lot more about CouchDB than Thrudb recently, but it's definitely worth a look if you need a document database. It's not suitable for our needs for the same reasons as CouchDB. 

### Distributed key-value stores

The rest are much closer to being 'simple' key-value stores with low enough latency to be used for serving data used to build dynamic pages. Latency will be dependent on the environment, and whether or not the dataset fits in memory. If it does, I'd expect sub-10ms response time, and if not, it all depends on how much money you spent on spinning rust.

#### MemcacheDB

Essentially just memcached that saves stuff to disk using a Berkeley database. As useful as this may be for some situations, it doesn't deal with replication and partitioning (sharding), so it would still require a lot of work to make it scale horizontally and be tolerant of machine failure. Other memcached derivatives such as <a href="http://repcached.lab.klab.org/">repcached</a> go some way to addressing this by giving you the ability to replicate entire memcache servers (async master-slave setup), but without partitioning it's still going to be a pain to manage.

#### Project Voldemort

Looks _awesome_. Go and read the <a href="http://project-voldemort.com/">rather splendid website</a>, which explains how it works, and includes pretty diagrams and a good description of how consistent hashing is used in the Design section. (If consistent hashing butters your muffin, check out <a href="http://www.last.fm/user/RJ/journal/2007/04/10/rz_libketama_-_a_consistent_hashing_algo_for_memcache_clients">libketama - a consistent hashing library</a> and the <a href="http://www.metabrew.com/article/erlang-libketama-driver-consistent-hashing/">Erlang libketama driver</a>). Project-Voldemort handles replication and partitioning of data, and appears to be well written and designed. It's reassuring to read in the docs how easy it is to swap out and mock different components for testing. It's non-trivial to add nodes to a running cluster, but according to the mailing-list this is being worked on. It sounds like this would fit the bill if we ran it with a Java load-balancer service (see their Physical Architecture Options diagram) that exposed a Thrift API so all our non-Java clients could use it.

#### Scalaris

Probably the most face-meltingly awesome thing you could build in Erlang. CouchDB, Ejabberd and RabbitMQ are cool, but Scalaris packs by far the most impressive collection of sexy technologies. Scalaris is a key-value store - it uses a modified version of the Chord algorithm to form a DHT, and stores the keys in lexicographical order, so range queries are possible. Although I didn't see this explicitly mentioned, this should open up all sorts of interesting options for batch processing - map-reduce for example.  On top of the DHT they use an improved version of <a href="http://en.wikipedia.org/wiki/Paxos_algorithm">Paxos</a> to guarantee ACID properties when dealing with multiple concurrent transactions. So it's a key-value store, but it can guarantee the ACID properties and do proper distributed transactions over multiple keys. 

Oh, and to demonstrate how you can scale a webservice based on such a system, the Scalaris folk implemented their own version of Wikipedia on Scalaris, loaded in the Wikipedia data, and benchmarked their setup to prove it can do more transactions/sec on equal hardware than the classic PHP/MySQL combo that Wikipedia use. Yikes. 

From what I can tell, Scalaris is only memory-resident at the moment and doesn't persist data to disk. This makes it entirely impractical to actually run a service like Wikipedia on Scalaris for real - but it sounds like they tackled the hard problems first, and persisting to disk should be a walk in the park after you rolled your own version of Chord and made Paxos your bitch. Take a look at this presentation about Scalaris from the Erlang Exchange conference: <a href="http://video.google.com/videoplay?docid=6981137233069932108&amp;ei=caB0SaPUNIW0iALk-9CMBQ">
    Scalaris presentation video
  </a>

#### The Rest

The reminaing projects, *Dynomite*, *Ringo* and *Ka* are all, more or less, trying to be Dynamo. Of the three, *Ringo* looks to be the most specialist - it makes a distinction between small (less than 4KB) and medium-size data items (<100MB). Medium sized items are stored in individual files, whereas small items are all stored in an append-log, the index of which is read into memory at startup. From what I can tell, Ringo can be used in conjunction with the Erlang map-reduce framework Nokia are working on called <a href="http://discoproject.org">Disco</a>.

I didn't find out much about *Kai* other than it's rather new, and some mentions in Japanese. You can chose either Erlang ets or dets as the storage system (memory or disk, respectively), and it uses the memcached protocol, so it will already have client libraries in many languages.

*Dynomite* doesn't have great documentation, but it seems to be more capable than Kai, and is under active development. It has pluggable backends including the storage mechanism from CouchDB, so the 2GB file limit in dets won't be an issue. Also I heard that Powerset are using it, so that's encouraging. 

### Summary 

Scalaris is fascinating, and I hope I can find the time to experiment more with it, but it needs to save stuff to disk before it'd be useful for the kind of things we might use it for at Last.fm. 

I'm keeping an eye on Dynomite - hopefully more information will surface about what Powerset are doing with it, and how it performs at a large scale. 

Based on my research so far, Project-Voldemort looks like the most suitable for our needs. I'd love to hear more about how it's used at LinkedIn, and how many nodes they are running it on. 

### What else is there?

Here are some other related projects:
<ul>
	<li><a href="http://www.hazelcast.com/">Hazelcast</a> - Java DHT/clustering library</li>
	<li><a href="http://blitiri.com.ar/p/nmdb/">nmdb</a> - a network database (dbm-style)</li>
	<li><a href="http://open-chord.sourceforge.net/">Open Chord</a> - Java DHT</li>
</ul>

If you know of anything I've missed off the list, or have any feedback/suggestions, please post a comment. I'm especially interested in hearing about people who've tested or are using KV-stores in lieu of relational databases.

*UPDATE 1:* Corrected table: memcachedb does replication, as per BerkeleyDB.
