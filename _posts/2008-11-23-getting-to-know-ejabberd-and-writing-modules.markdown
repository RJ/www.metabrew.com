--- 
layout: post
title: Getting to know ejabberd and writing modules
permalink: /article/getting-to-know-ejabberd-and-writing-modules
tags: 
- programming
- erlang
- mnesia
- ejabberd
- xmpp
- thrift
- yaws
wordpress_id: 203
---
I started poking around in the ejabberd source code to see what I could learn. I couldn't find much in the way of high level documentation that talks about how the various bits of ejabberd talk to each other, so I'm starting to piece it together myself.

After compiling ejabberd I made a php script I could use with the external authentication system. Here's a version that supports just two hardcoded users:

ejabberd.cfg:
```erlang
{auth_method, external}.
{extauth_program, "/tmp/auth.php"}.
```

auth.php:
```php
#!/usr/bin/php
<?
$fh  = fopen("php://stdin", 'r');
if(!$fh){
    die("Cannot open STDIN\n");
}
$users = array('user1'=>'password1', 'user2'=>'password2');

do{
    $lenBytes = fgets($fh, 3);
    $len = unpack('n', $lenBytes);
    $len = $len[1];
    if($len<1) continue;
    $msg = fgets($fh, $len+1);
    $toks=explode(':',$msg);
    $method = array_shift($toks);
    switch($method){
        case 'auth':
            list($username, $server, $password) = $toks;
            if(@$users[$username] == $password){
                print pack("nn", 2, 1); // ok
            }else{
                print pack("nn", 2, 0); // fail
            }
            break;

        case 'isuser':
            list($username, $server) = $toks;
            if(isset($users[$username])){
                print pack("nn", 2, 1); // yes
            }else{
                print pack("nn", 2, 0); // nope
            }
            break;

        default:
            print pack("nn", 2, 0);// fail
    }
}while(true);
```

I stripped down the ejabberd config to just load what I considered the bare essentials. Here is the modules section I'm testing with:

From ejabberd.cfg:
```erlang
{modules,
 [
  {mod_caps,     []},
  {mod_disco,    []},
  {mod_roster,   []},
  {mod_pubsub,   [ % requires mod_caps
                  {access_createnode, pubsub_createnode},
                  {plugins, ["default", "pep"]}
                 ]},
  {mod_mnesiaweb,     []},
  {mod_thriftctl,     []}
 ]}.
 ```

*mod_disco* deals with discovery, so clients can find out what the server supports. mod_roster deals with rosters (buddy lists etc) using mnesia. mod_pubsub is enabled because I want to use <a href="http://xmpp.org/extensions/xep-0118.html">User Tune</a>, an extension that lets you broadcast the name of the song you are playing to all everyone in your roster. mod_caps provides <a href="http://xmpp.org/extensions/xep-0115.html">XEP-115</a> - an extension for broadcasting and dynamically discovering client, device, or generic entity capabilities. mod_caps is a requirement of mod_pubsub.

I've removed the module that allows users to register, although I made a few accounts first whilst testing. The last two modules, mod_mnesiaweb and mod_thriftctl are modules I wrote.

## mod_mnesiaweb

To help figure out what's going on inside of ejabberd, it's useful to be able to easily browse the mnesia database. <a href="http://yaws.hyber.org/">Yaws</a> comes with an appmod that does this, called ymnesia. This ejabberd module will start yaws in embedded mode and run this appmod, enabling you to explore the mnesia database from a web browser.

<i><b>Yaws observation:</b> yaws didn't appear to build ymnesia by default, I edited the Makefile in src and added "ymnesia" to the module list. Also, if ./configure fails, the package you are probably missing is libpam0g-dev</i>

mod_mnesiaweb:
```erlang
% Ejabberd module that runs yaws in embedded mode,
% and loads the ymnesia appmod for browsing mnesia.
-module(mod_mnesiaweb).
-author('rj@last.fm').

-include("/usr/local/lib/yaws/include/yaws.hrl").

-behaviour(gen_mod).
-export([start/2, stop/1]).

start(_Host, Opts) ->
    Port = gen_mod:get_opt(port, Opts, 8001),
    code:add_path("/usr/local/lib/yaws/ebin"),
    application:set_env(yaws, embedded, true),
    application:start(yaws),
    GC = yaws_config:make_default_gconf(false,"yawstest"),
    SC = #sconf{
        port = Port,
        servername = "ejabnesia",
        listen = {0,0,0,0},
        appmods = [{"showdb", ymnesia}],
        docroot = "wwwroot"
        },
    yaws_api:setconf(GC, [[SC]]),
    ok.

stop(_Host) ->
    application:stop(yaws),
    ok.
```

To compile it:
```console
erlc -pa ${EJAB_SRC} -I ${EJAB_SRC} mod_mnesiaweb.erl
```

where EJAB_SRC is the ejabberd-2.X.X/src directory, after you've compiled from source (so the beams are there too).

Copy the resulting mod_mnesiaweb.beam to /var/lib/ejabberd/ebin so ejabberd finds it, and it should work. Hit up http://localhost:8001/showdb/ in your browser and you can explore the mnesia database.

Use the match syntax to filter tables. For example to find everyone in my roster, I use this in the input box next to roster:

```erlang
{roster,{"RJ",'_', {'_','_',[]}}, '_','_','_','_','_','_','_','_'}
```

Not pretty, but it gets the job done. You can just view the entire table, copy a record then replace fields with '_' to build queries.

## mod_thriftctl
Next up I wanted to try the Erlang <a href="http://incubator.apache.org/thrift/">Thrift</a> bindings (written by the folks at <a href="http://amiest-devblog.blogspot.com/2008/01/alternative-erlang-bindings-for-thrift.html">Amie St.</a>), and expose some useful functionality for controlling the server.

If you aren't familiar with Thrift, I recommend reading about it first. In a nutshell, you write your API using an IDL (a .thrift file) and the thrift compiler creates client libraries, and server code in various different languages. It's an RPC mechanism, and useful in a mixed environment.

mod_thriftctl.thrift:
```php
#!/usr/local/bin/thrift -php -erl

struct JabberUser {
    1: string name,
    2: string server
}

service Ejabthrift {
    /* add ruser to roster of luser, and visa-versa. also routes presence to users if online  */
    void add_friend(        1: JabberUser luser,
                            2: JabberUser ruser
                            ),

    /* remove ruser from luser's roster */
    void remove_friend(    1: JabberUser luser, 2: JabberUser ruser ),

    /* make it look like fromuser sent a message to touser */
    void spoof_message( 1: JabberUser fromuser, 2: JabberUser touser, 3: string message, 4: string subject ),
    /* .. or a chat message */
    void spoof_chat(    1: JabberUser fromuser, 2: JabberUser touser, 3: string message, 4: string thread ),

    /* sends PEP usertune message, see http://xmpp.org/extensions/xep-0118.html */
    void publish_np ( 1: JabberUser fromuser, 2: string artist, 3: string album, 4: string track, 5: i32 tracklength, 6: i32 tracknum )
}
```

Run that .thrift file, and you get gen-php and gen-erl directories, with php client code, and erlang files needed to build a server. 

Here's the ejabberd module, which starts a thrift server:

mod_thriftctl:
```erlang
%
% A module to control ejabberd with a thrift interface.
%
-module(mod_thriftctl).
-author('rj@last.fm').

% ejabberd headers:
-include("ejabberd.hrl").
-include("mod_roster.hrl").
-include("jlib.hrl").

% thrift server headers:
-include("thrift.hrl").
-include("transport/tSocket.hrl").
-include("protocol/tBinaryProtocol.hrl").
-include("server/tErlServer.hrl").
-include("transport/tErlAcceptor.hrl").

% we are an ejabberd module:
-behaviour(gen_mod).
-export([start/2, stop/1]).

% our thrift service:
-include("ejabthrift_thrift.hrl").
-include("mod_thriftctl_types.hrl").
-export([   add_friend/2, remove_friend/2,
            spoof_message/4, spoof_chat/4,
            publish_np/6
        ]).

% convert thrift Jabberuser into ejabberd jid
ju2jid(Jabberuser) when is_record(Jabberuser, jabberUser) ->
    #jid{ user=Jabberuser#jabberUser.name, server=Jabberuser#jabberUser.server, resource="",
          luser=Jabberuser#jabberUser.name, lserver=Jabberuser#jabberUser.server, lresource=""
        }.

spoof_message( FromU, ToU, Msg, Subject ) ->
    F = ju2jid(FromU),
    T = ju2jid(ToU),
    XmlBody = {xmlelement, "message",
               [
                {"from", jlib:jid_to_string(F)},
                {"to", jlib:jid_to_string(T)}
               ],
               [
               {xmlelement, "subject", [], [{xmlcdata, Subject}]},
               {xmlelement, "body", [], [{xmlcdata, Msg}]}
               ]
              },
    ejabberd_router:route(F, T, XmlBody).

spoof_chat( FromU, ToU, Msg, Thread ) ->
    F = ju2jid(FromU),
    T = ju2jid(ToU),
    XmlBody = {xmlelement, "message",
               [{"type", "chat"},
                {"from", jlib:jid_to_string(F)},
                {"to", jlib:jid_to_string(T)}
               ],
               [
               {xmlelement, "thread", [], [{xmlcdata, Thread}]},
               {xmlelement, "body", [], [{xmlcdata, Msg}]}
               ]
              },
    ejabberd_router:route(F, T, XmlBody).

publish_np( FromU, ArtistS, AlbumS, TrackS, LengthI, TrackNumI ) ->
    From = ju2jid(FromU),
    % The usertune message must contain binaries, not strings or ints
    FromStr     = jlib:jid_to_string(From),
    Artist      = list_to_binary(ArtistS),
    Album       = list_to_binary(AlbumS),
    Track       = list_to_binary(TrackS),
    Length      = list_to_binary(io_lib:format("~w",[LengthI])),
    TrackNum    = list_to_binary(io_lib:format("~w",[TrackNumI])),
    Xml = {xmlelement,"iq",
                [{"from", FromStr},
                 {"type","set"},
                 {"id","pub1"}],
                [{xmlcdata,<<"\n  ">>},
                 {xmlelement,"pubsub",
                  [{"xmlns","http://jabber.org/protocol/pubsub"}],
                  [{xmlcdata,<<"\n    ">>},
                   {xmlelement,"publish",
                    [{"node","http://jabber.org/protocol/tune"}],
                    [{xmlcdata,<<"\n      ">>},
                     {xmlelement,"item",[],
                      [{xmlcdata,<<"\n        ">>},
                       {xmlelement,"tune",
                        [{"xmlns","http://jabber.org/protocol/tune"}],
                        [{xmlcdata,<<"\n          ">>},
                         {xmlelement,"artist",[],
                          [{xmlcdata, Artist}]},
                         {xmlcdata,<<"\n          ">>},
                         {xmlelement,"length",[],[{xmlcdata, Length}]},
                         {xmlcdata,<<"\n          ">>},
                         {xmlelement,"source",[],
                          [{xmlcdata, Album}]},
                         {xmlcdata,<<"\n          ">>},
                         {xmlelement,"title",[],
                          [{xmlcdata, Track}]},
                         {xmlcdata,<<"\n          ">>},
                         {xmlelement,"track",[],[{xmlcdata, TrackNum}]},
                         {xmlcdata,<<"\n        ">>}]},
                       {xmlcdata,<<"\n      ">>}]},
                     {xmlcdata,<<"\n    ">>}]},
                   {xmlcdata,<<"\n  ">>}]},
                 {xmlcdata,<<"\n">>}]},
    % PEP means you act as a pubsub node yourself,
    % so it's addressed to yourself and is broadcast to your friends automatically:
    ejabberd_router:route(From, From, Xml),
    ok.

% adds bi-directional friend relationship immediately for both users.
add_friend(     #jabberUser{name=LU, server=LS},
                #jabberUser{name=RU, server=RS}) ->
    AskMessage = "",
    Group = "",
    Subtype = both,
    subscribe(LU, LS, RU, RS, RU, Group, Subtype, AskMessage),
    subscribe(RU, RS, LU, LS, LU, Group, Subtype, AskMessage),
    route_rosteritem(LU, LS, RU, RS, RU, Group, Subtype),
    route_rosteritem(RU, RS, LU, LS, LU, Group, Subtype),
    ok.

remove_friend( #jabberUser{name=LU, server=LS}, #jabberUser{name=RU, server=RS} ) ->
    unsubscribe(LU, LS, RU, RS),
    unsubscribe(RU, RS, LU, LS),
    route_rosteritem(LU, LS, RU, RS, "", "", "remove"),
    route_rosteritem(RU, RS, LU, LS, "", "", "remove"),
    ok.

unsubscribe(LocalUser, LocalServer, RemoteUser, RemoteServer) ->
    Key = {{ '{{' }}LocalUser,LocalServer,{RemoteUser,RemoteServer,[]}}, {LocalUser,LocalServer}},
    mnesia:transaction(fun() -> mnesia:delete(roster, Key, write) end).

route_rosteritem(LocalUser, LocalServer, RemoteUser, RemoteServer, Nick, Group, Subscription) ->
    LJID = jlib:make_jid(LocalUser, LocalServer, ""),
    RJID = jlib:make_jid(RemoteUser, RemoteServer, ""),
    ToS = jlib:jid_to_string(LJID),
    ItemJIDS = jlib:jid_to_string(RJID),
    GroupXML = {xmlelement, "group", [], [{xmlcdata, Group}]},
    Item = {xmlelement, "item",
        [{"jid", ItemJIDS},
         {"name", Nick},
         {"subscription", Subscription}],
        [GroupXML]},
    Query = {xmlelement, "query", [{"xmlns", ?NS_ROSTER}], [Item]},
    Packet = {xmlelement, "iq", [{"type", "set"}, {"to", ToS}], [Query]},
    ejabberd_router:route(LJID, LJID, Packet).


subscribe(LocalUser, LocalServer, RemoteUser, RemoteServer, Nick, Group, Subscription, Xattrs) ->
    R = #roster{usj = {LocalUser,LocalServer,{RemoteUser,RemoteServer,[]}},
                us = {LocalUser,LocalServer},
                jid = {RemoteUser,RemoteServer,[]},
                name = Nick,
                subscription = Subscription, % none, to=you see him, from=he sees you, both
                ask = none, % out=send request, in=somebody requests you, none
                groups = [Group],
                askmessage = Xattrs, % example: [{"category","conference"}]
                xs = []
               },
    mnesia:transaction(fun() -> mnesia:write(R) end).

start(Host, Opts) ->
    ?INFO("mod_ejabthrift start().",[]),
    %% get options
    Port = gen_mod:get_opt(port, Opts, 9000),

    spawn(fun()-> thrift:start() end),
    ?INFO("mod_ejabthrift thrift:start().",[]),

    Handler   = ?MODULE,
    Processor = ejabthrift_thrift,

    TF = tBufferedTransportFactory:new(),
    PF = tBinaryProtocolFactory:new(),

    ServerTransport = tErlAcceptor,
    ServerFlavor    = tErlServer,

    Server = oop:start_new(ServerFlavor, [Port, Handler, Processor, ServerTransport, TF, PF]),

    case ?R0(Server, effectful_serve) of
    ok    ->
        ?INFO("mod_ejabthrift: Thrift server (~s) listening on port ~w",[Host, Port]),
        % put Server into process dictionary (needed for clean stop)
        put(thrift_server_reference, Server),
        ok;
    Error ->
        ?ERROR_MSG("mod_ejabthrift: Error starting thrift server: ~w", [Error]),
        Error
    end.

stop(_Host) ->
    ?C0(get(thrift_server_reference), stop),
    ok.
```

To build, first build the gen-erl code:

```console
erlc -pa ${EJAB_SRC} -I ${EJAB_SRC} -I ${ERL_THRIFT}/include -I ./gen-erl -o ./gen-erl ./gen-erl/*.erl
```

Where ERL_THRIFT is the lib/erl directory from the amiethrift code, git://repo.or.cz/amiethrift.git

Then compile the module:

```console
erlc -pa ${EJAB_SRC} -I ${EJAB_SRC} -I ${ERL_THRIFT}/include -I ./gen-erl *.erl
```

To install, copy all the beam files to the ejabberd ebin dir:

```console
sudo cp *.beam gen-erl/*.beam /var/lib/ejabberd/ebin/
```

This is inspired by mod_xmlrpc, which is in ejabberd-modules. As you can see from the start function, that's what it takes to start a thrift server. It's now trivial to call into ejabberd from other languages. For example, if you started listening to a song using a flash player on the website, a php webservice could make a user tune announcement on your behalf, or spoof messages from you boasting how much you love listening to Paris Hilton. 

If anyone knows where I can read about the ejabberd architecture / design, so I don't have to piece it all together myself, please let me know.
