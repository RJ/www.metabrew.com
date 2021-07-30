--- 
layout: post
title: Reading Serialized PHP Objects from Erlang
permalink: /article/reading-serialized-php-objects-from-erlang
tags: 
- programming
- erlang
- php
wordpress_id: 15
---
I started writing some Erlang recently. The vast majority of data I need to access from Erlang resides in cached, serialized php objects. Here's what I came up with to turn a serialized php object into a sort of nested Erlang proplist thing. 


```php
<?php
$s = array(123, 'hello', 3.14, array('a'=>'foo', 'b'=>'bar'));
print serialize($s);
?>
```



This gives (wrapped for clarity):

<pre>
a:4:{i:0;i:123;i:1;s:5:"hello";i:2;d:3.14;i:3;a:2:
{s:1:"a";s:3:"foo";s:1:"b";s:3:"bar";}}
</pre>

It's not hard to see how the (relatively undocumented) PHP serialization format works.
Here's what it becomes in Erlang:

```erlang
php:unserialize("a:4:...snip...").
```

```console
{[[{0,123},
{1,<<"hello">>},
{2,3.14},
{3,[{a,<<"foo">>},{b,<<"bar">>}]}]],
[]}
```

Here's what it does with objects:
```php
<?php
class ExampleClass {
    var $id = 123;
    var $name = "RJ";
    var $languages = array('php', 'erlang', 'etc');
}
$s = new ExampleClass();
$ser = serialize($s);
print serialize($str);
?>
```


```erlang
php:unserialize("O:12:\"ExampleClass\":3:{s:2:\"id\";i:123;s:4:\"name\";s:2:"
                "\"RJ\";s:9:\"languages\";a:3:{i:0;s:3:\"php\";i:1;s:6:"
                "\"erlang\";i:2;s:3:\"etc\";}}").

{[{class,"ExampleClass",
         [{id,123},
          {name,<<"RJ">>},
          {languages,[{0,<<"php">>},
                      {1,<<"erlang">>},
                      {2,<<"etc">>}]}]}],
 []}
```

Due to a combination of PHP's "relaxed" type system, an old database abstraction library, and munging things in and out of memcached, we sometimes end up with numeric properties, such as 'id', represented as strings by PHP. To mitigate this, I ended up with some nasty code that forces certain properties to a predefined type ("id" is always an int, etc..). Yuk. Anyway, here's the Erlang module:

```erlang
%
% Takes a serialized php object and turns it into an erlang data structure
%
-module(php).
-author('Richard Jones rj@metabrew.com').
-export([unserialize/1]).

% Usage:  {Result, Leftover} = php:unserialize(...)

unserialize(S) when is_binary(S)    -> unserialize(binary_to_list(S));
unserialize(S) when is_list(S)      -> takeval(S, 1).

% Internal stuff

takeval(Str, Num) ->
    {Parsed, Remains} = takeval(Str, Num, []),
    { lists:reverse(Parsed), Remains }.

takeval([$} | Leftover], 0, Acc)    -> {Acc, Leftover};
takeval(Str, 0, Acc)                -> {Acc, Str};
takeval([], 0, Acc)                 -> Acc;

takeval(Str, Num, Acc) ->
    {Val, Rest} = phpval(Str),
    %Lots of tracing if you enable this:
    %io:format("\nState\n Str: ~s\n Num: ~w\n Acc:~w\n", [Str,Num,Acc]),
    %io:format("-Val: ~w\n-Rest: ~s\n\n",[Val, Rest]),
    takeval(Rest, Num-1, [Val | Acc]).

%
% Parse induvidual php values.
% a "phpval" here is T:val; where T is the type code for int, object, array etc..
%

% Simple ones:
phpval([])                      -> [];
phpval([ $} | Rest ])           -> phpval(Rest);    % skip }
phpval([$N,$;|Rest])            -> {null, Rest};    % null
phpval([$b,$:,$1,$; | Rest])    -> {true, Rest};    % true
phpval([$b,$:,$0,$; | Rest])    -> {false, Rest};   % false


% r seems to be a recursive reference to something, represented as an int.
phpval([$r, $: | Rest]) ->
    {RefNum, [$; | Rest1]} = string:to_integer(Rest),
    { {php_ref, RefNum}, Rest1 };

% int
phpval([$i, $: | Rest])->
    {Num, [$; | Rest1]} = string:to_integer(Rest),
    {Num, Rest1};

% double / float
% NB: php floats can be ints, and string:to_float doesn't like that.
phpval(X=[$d, $: | Rest]) ->
    {Num, [$; | Rest1]} = case string:to_float(Rest) of
                            {error, no_float} -> string:to_integer(Rest);
                            {N,R} -> {N,R}
    end,
    {Num, Rest1};

% string
phpval([$s, $: | Rest]) ->
    {Len, [$: | Rest1]} =string:to_integer(Rest),
    S = list_to_binary(string:sub_string(Rest1, 2, Len+1)),
    {S, lists:nthtail(Len+3, Rest1)};

% array
phpval([$a, $: | Rest]) ->
    {NumEntries, [$:, ${ | Rest1]} =string:to_integer(Rest),
    {Array, Rest2} = takeval(Rest1, NumEntries*2),
    {arraytidy(Array), Rest2};

% object O:4:\"User\":53:{
phpval([$O, $: | Rest]) ->
    {ClassnameLen, [$: | Rest1]} =string:to_integer(Rest),
    % Rest1: "classname":NumEnt:{..
    Classname = string:sub_string(Rest1, 2, ClassnameLen+1),
    Rest1b = lists:nthtail(ClassnameLen+3, Rest1),
    {NumEntries, [$:, ${ | Rest2]} = string:to_integer(Rest1b),
    {Classvals, Rest3} = takeval(Rest2, NumEntries*2),
    { {class, Classname, arraytidy(Classvals)}, Rest3 }.

%%
%% Helpers:
%%

% convert [ k1,v1,k2,v2,k3,v3 ] into [ {k1,v2}, {k2,v2}, {k3,v3} ]
arraytidy(L) ->
    lists:reverse(lists:foldl(fun arraytidy/2, [], L)).

arraytidy(El, [{key___partial, K} | L]) -> [{atomize(K), El} | L];

arraytidy(El, L) -> [{key___partial, El} | L].

%% Make properties or keys into atoms
atomize(K) when is_binary(K) ->
    atomize(binary_to_list(K));
atomize(K) when is_list(K) ->
    list_to_atom(string:to_lower(K));
atomize(K) -> K.
```
