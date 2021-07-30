--- 
layout: post
title: Transcoding HTTP mp3 streaming proxy in bash
permalink: /article/transcoding-http-mp3-streaming-proxy-in-bash
tags: 
- programming
- hacks
- bash
- netcat
- hack
- streaming
wordpress_id: 58
---
Here's how to make a proxy for streaming mp3s. It transcodes on-the-fly to 64kpbs MP3 using lame. When transcoding is finished, it calls the ./posthandler.sh script, which can either just delete the file, or potentially archive it so you don't need to transcode it again.

```bash
#!/bin/bash
read method url version

method="${method%$CR}"
url="${url%$CR}"
version="${version%$CR}"

echo -ne "HTTP/1.0 200 OK\r\nContent-type: audio/mpeg\r\n\r\n"

BR=64 #birate to transcode to.
PIPE="/tmp/$$.pipe"
mkfifo "$PIPE"

OUTFILE="./tmp.$$.$BR.mp3"
rm $OUTFILE
url=`echo "$url" | sed 's/\///'`
echo "** GET $url" >&2

nohup lynx --source "$url" \
    | (lame --preset cbr $BR --mp3input - - 2>/dev/null \
      && (echo "** Finished transcoding $url" >&2 ; \
          ./posthandler.sh "$OUTFILE"&))\
    | tee -i "$PIPE" > $OUTFILE &

cat < $PIPE
rm $PIPE
```

One interesting limitation seems to be the buffer size of a fifo pipe in linux. Even though the transcoding step is pretty quick, if a client is connected the transcoding only manages to fill the pipe a couple of hundred k ahead of what is being read. 

The -i flag to `tee` means it ignores interrupts, and will finish transcoding the file and call the posthandler even if the client disconnects.

Run is like this:

```console
$ while [ 1 ]; do nc -vlp 8080 -c './transstreamer.sh' ; done
```

Then hit up a url of your choice using your awesome new proxy:

```console
$ URL="http://freedownloads.last.fm/download/105468518/Letters%2BFrom%2BThe%2BBoatman.mp3"
$ mpg321 "http://localhost:8080/$URL"
```

Not the most scalable solution, but a mildly amusing quick hack.
