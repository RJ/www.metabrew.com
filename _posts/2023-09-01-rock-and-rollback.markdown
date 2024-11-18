--- 
layout: post
published: true
title: "Rock and Rollback"
permalink: /article/rock-and-rollback
tags: 
- gamedev
- netcode
- multiplayer
---

How hard it is to build a fun multiplayer browser game? Like asteroids, but multiplayer. Simple retro graphics, because
I'm a terrible artist/graphics programmer, but decent, modern netcode for responsive gameplay.

### Realtime multiplayer in the browser

Until relatively recently, realtime multiplayer in the browser was hamstrung by the lack of an unreliable network transport. Everything the browser did was TCP, and *the* realtime option was websockets. 

TCP has head-of-line blocking though, because it's a reliable ordered stream.

Say the server is streaming positional updates in packets like this:

* (Packet 1) Pos A = 1,7 @ t = 1
* (Packet 2) Pos A = 1,8 @ t = 2
* (Packet 3) Pos A = 1,9 @ t = 3

If Packet 2 is dropped or delayed, Packet 3, although received by the client, will be held in the network buffer and not delivered to your game until Packet 2 turns up, because TCP won't deliver packets out of order. Perhaps Packet 2 had to be retransmitted by TCP, taking valuable time.

Meanwhile, the game doesn't really care about outdated positions. It just wants the most recent value.

Realtime games use unreliable transports like UDP, which don't block the latest packet arriving if an older packet is late or dropped.

It's up to your game protocol to handle dropped packets, which is extra work, but means you can stream realtime data and not suffer the same issues as TCP. Packets are delivered to your game as soon as they arrive, regardless of any packet loss or delays.

If only there was a way to do this in the browser..

#### Browser technologies for unreliable datagrams

**WebRTC**, which is designed for video and audio calling in the browser, has an Unreliable Datachannel option (UDP). This is suitable, but there is quite a lot of machinery required to present yourself as a WebRTC server just to accept a data channel.

The **WebTransport** API, which operates over HTTP/3, supports unreliable datagrams too, and is a thinner layer. That's what we'll use to get an unreliable datagram transport for our game.

## Netcode Styles: Executive Summary

To figure out how to network multiplayer asteroids, it behooves us to examine one of the most common approaches to netcode called **Snapshot Interpolation** – used by first person shooters for decades.

### Quake, et al.

Quake, and many FPS games that came after it, can be described as:

> A bunch of players moving around a mostly static world, shooting at each other.

These games are continually sending your inputs (move forward, jump, shoot..) to the server. The server is simulating your character along with the rest of the players, then streaming snapshots back to you containing up-to-date positions at specific points in time.

Perhaps you're getting player position updates 64 times a second ("tick rate" of 64Hz). That is a lot of packets, but would still look stuttery if you just snapped players to whatever the most recent position you received was. So the game will interpolate between the last two updates you received. 

* tick = 1, pos = 1,1
* tick = 2, pos = 1,2
* tick = 3, pos = 1,3

If your client's clock is half way between ticks 2 and 3, you'll see a blended position half way between `pos = 1,2` and `pos = 1,3`.

This is the standard approach for competetive FPS. You can tune the buffers to smooth over network lag spikes, at the cost of moving remote player timelines further into the past.

#### Seeing the past

Consider that even if you snap a remote player position to the value in the most recently received packet, you're still showing a past position, because of the network travel time of that packet from server to client.

Now factor in a bit of a buffer so you have 2 snapshots to interpolate between, and you'll understand that although you're seeing nice smooth movement for remote players, on account of the interpolation, you're seeing where they were 15-150ms ago depending on ping.

#### Local-player Prediction

To further complicate matters, when you hit `W` to move forward, the game doesn't wait for a server update before moving your own player's position. **Your position is predicted**, ie. when you press forward, your local player immediately moves forward, while your `t = 123, input_pressed = forward` packet travels to the server. The server processes it, dutifully moves your player forward, and includes the new position in the next snapshot.

Your game client, which kept a record of your predicted movement and inputs for the last few ticks, will need to reconcile this with updates received from the server.  You might have to wind back your local player to correct a prediction error (perhaps you were shot just before pressing forward), and replay any stored movement commands, to project your position back to the current time.

#### Hit-verification with backwards reconciliation

Oh yeah, so since your local player position is predicted into the future compared to the server, and remote players are shown in the past due to network latency and interpolation delays, how come you don't have to lead your shots to hit moving players?

Well, when your client reports your shot and a hit at `t = 123, aimed_at = (x,y,z)`, the server winds back time and repositions everyone to what it thinks your specific client was seeing at `t = 123`, including any interpolation between two known snapshots, then verifies if the hit was a hit, then puts everyone back in their current positions and carries on.


### Ok, so.. snapshot interpolation asteroids?

What happens if we apply this model to multiplayer asteroids? We predict movement of your own ship, just like FPS, and do snapshot interpolation for remote players and asteroids.

The movement of players and asteroids would be nice and smooth, right up to the moment you collide with something.

Your ship is locally predicted ahead of the server, based on your inputs. Remote asteroids or players are shown in the past, due to snapshot interpolation. So you're essentially seeing the position of other objects but from (say) 75ms in the past.

For a game that would surely involve lots of crashing into other players and asteroids, we don't want janky collisions.


## Client Prediction & Rollback Networking

Asteroids are pretty simple, they just keep moving until they collide with something. So we'll have our client project their position forward into the same timeline as the local player. We just simulate the physics the required number of extra ticks. 

We'll store a historical record for each entity: the last few frames of position/rotation. When we get a server update about a position, we can check it against our historical record for that tick. If there were any discrepencies, we perform a rollback: wind back time to the tick from the server update, snap positions to the server-authoritative position, and fast-forward back to our current tick, resimulating the physics as we go – and in the case of mispredicted players, re-applying any stored inputs.

#### Client-prediction, single player

> This video shows client on the left, server view on the right. 
> There is at least 100ms simulated lag.
> (No audio)

 <iframe width="560" height="315" src="https://www.youtube.com/embed/ZAbuYm6MS5s?si=qmnr3lyuJ0E-UWLh" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>

If you fullscreen this video, you'll be able to see the light grey outline of each entity - that is the last received authoritative server position. The colored outlines are where the client has predicted them in the client timeline. No rollbacks or jank because with a single player, there aren't any mispredictions.

#### Timeline jank experiment

Here's an experiment I conducted testing how collisions behave differently when asteroids are predicted forward into the same timeline as the local player.

Movement looks good in isolation, however per my voiceover in the next video, remote players do get janky collisions.
This is because of the temporal disrepency between remote players and other objects. With remote players
held 6 frames in the past, which equates to around 100ms, there are janky collisions. Watch what happens to collisions when I enable input prediction for remote players, to bring them into the same timeframe as the local player and asteroids:

> Each player has 70ms simulated lag.
> (Audio commentary, unmute!)

<iframe width="560" height="315" src="https://www.youtube.com/embed/Jxetoat3wsw?si=xdB4BAlE2hz27Ugh" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>


## Predicting Remote Players

Since we don't have up-to-date inputs for remote players on our most recent ticks, we just assume they are still pressing the same buttons as the last-known input.
Players holding down forward are most likely to still be holding down forward.

This, plus smearing errors due to mispredictions over a couple of frames, can work pretty well, provided we keep player latency to a reasonable amount. 

We can also bake in 3 ticks of input delay for all players. So inputs a client presses are used for t+3, but sent to the server immediately and rebroadcast to other players. This means sometimes you do in fact have all inputs for remote players, or at least only have to guess 1 or 2 ticks worth.


## Implementation Details

These experiments really helped me understand what kind of netcode model I needed – but I ended up throwing away most of the code I'd written. My rollback implementation was kind of a mess, being very new to rust, bevy, and gamedev in general. I tried out a couple of different bevy crates, and settled on Lightyear. A new crate which includes prediction & rollback, interpolation, and WebTransport support.

I contributed a basic asteroid-esque game to the Lightyear examples, called [spaceships](https://github.com/cBournhonesque/lightyear/tree/main/examples/spaceships), which allowed me to experiment with how things like predicted bullet spawning should work.  The spaceships demo does client prediction for players and asteroids. It compiles to wasm and works in Chrome (TODO: debug firefox issue):

![/assets/images/spaceships.png](/assets/images/spaceships.png)


{% include image-caption.html imageurl="/assets/images/spaceships.png" title="Spaceships example game" caption="Spaceships Example from Lightyear" %}

## Onwards

Time to build a game! But wait, let's procrastinate a little more first by trying to figure out a good way to deploy multiplayer bevy games that use lightyear..