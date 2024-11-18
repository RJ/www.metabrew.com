--- 
layout: post
published: false
draft: true
title: "Home Automation Journey"
permalink: /article/uk-home-automation-fettling
tags: 
- homeautomation
---

## HA DRAFT

and everything has to talk locally to it. most HA stuff lives on a vlan which default denies internet access.
expose local ntp server.
allowlist for a couple of devices to get to the web

## Light Switches (UK)

As far as I can tell, all smart light switches available in the UK are terrible. The least offensive looking is probably LightwaveRF, but their latest gen doesn't have a legit local-only API. So the best option is to keep your existing switches, and install a smart relay module in the backbox behind the switch.

Many years ago I dabbled with zwave modules from Fibaro, but the walls and floors in my house are extremely solid, and I never quite had enough zwave devices to form a reliable mesh network. I got pretty sick of trying to debug zwave latency. I've since ripped them out and replaced with various Shelly relays, which are the same size but use WiFi, and are very well supported for offline operation.


### Yeah.. no
<style type="text/css">
figure.tiled > img {
    width: 200px;
}
</style>
<figure class="tiled">
    <img src="https://m.media-amazon.com/images/I/519M++w-rcL._AC_SL1300_.jpg" alt="Doorbird installation" />
    <img src="https://m.media-amazon.com/images/I/51MBIE-WIHL._AC_SL1001_.jpg" />
    <img src="https://m.media-amazon.com/images/I/71x7Z-P6F5L._SL1000_.jpg" />
    <img src="https://m.media-amazon.com/images/I/61Y6KGDrxwL._SL1500_.jpg" />
    <img src="https://m.media-amazon.com/images/I/61F1J6G0wOL._AC_SL1500_.jpg" />
    <img src="https://m.media-amazon.com/images/I/41kuSg+GW-L._AC_SL1500_.jpg" />

</figure>


shellies ftw

### Honourable mention to LiFx bulbs in a few places

inconvenient switch position, only used when at table.
outside lights, which have a shelly too, but for colour (good for food delivery)

## Occupancy / Motion

* PIR sensors from hardwired alarm system via Konnected.io
* battery powered shelly PIRs
* motion events from cameras

## Roller Blinds

quoted several 100 quid for smart nonsense. used a cheap shelly 2.5 with concealed rocker switch.

## Airflow MVHR

extensive back and forth with airflow tech people, who were very helpful in sending the modbus register data and answering my questions. 

https://github.com/favalex/modbus-cli/pull/6

RS485 to WiFi bridge (Elfin-EW11) (£20) to get modbus tco to hass.

## Solar Inverter

SMA sunny boy. has modbus interface.

## Multiple circuit energy monitoring

was using oldthing, but only up to 14 chans and a bit laggy reporting to hass.
now: Brultech GEM 3-phase.

## Wall-mounted tablet

ProDVX 22-inch android tablet, PoE. 

## Gate relay activation

In the days before wifi shellies, i used this cheap USB relay attached to my router near the gate:

* https://gist.github.com/RJ/7acba5b06a03c9b521601e08d0327d56

then I installed a Doorbird, which also needs to be connected to the relay. So the old usb relay is not needed any more, I just tell doorbird to trigger the gates as needed.

{% include image-caption.html imageurl="/images/ha/doorbird.png" title="Doorbird installation" caption="Installing a Doorbird gate station" %}

Doorbird works locally too, and the Home Assistant integration happily sends events for motion detection, doorbell button pressed, and a few others. When someone rings ours, as well as triggering announcements, I switch the kitchen tablet dashboard to a temporary live streaming view of various cameras.

## Garage Doors

Was ESP, lost config after power cut, replaced with 2x shelly 1s.

### Garage door controls

#### Hand-wave sensor

https://community.home-assistant.io/t/fibaro-swipe-gesture-sensor-fggc001-behind-picture-frame/53729

#### Shelly i3 clicky buttons



## Voice control

Ceiling mount google home. dissatisfied with G. want open source.



