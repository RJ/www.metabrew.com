--- 
layout: post
title: Updated bash PS1
permalink: /article/updated-bash-ps1
tags: 
- hacks
- bash
wordpress_id: 72
---
Made a minor tweak to my <code>.bashrc</code> after browsing <a href="http://www.dotfiles.org/">dotfiles.org</a> for some ideas. One neat trick I gleaned was detecting when the exit code of the last command (<code>$?</code>) was non-zero and altering the prompt. This will be useful for quickly seeing at a glance if some enormous load of output from make was successful or not.

_Note the prompt goes red on failure_
<a href="/images/2008/10/ps1.png">
<img class="size-full wp-image-73" title="Bash prompt" src="/images/2008/10/ps1.png" alt="Note the prompt goes red on failure" width="398" height="285" />
</a>

Here are the bits from my updated <code>.bashrc</code>:
```bash
# define useful aliases for color codes
sh_norm="\[\033[0m\]"
sh_black="\[\033[0;30m\]"
sh_darkgray="\[\033[1;30m\]"
sh_blue="\[\033[0;34m\]"
sh_light_blue="\[\033[1;34m\]"
sh_green="\[\033[0;32m\]"
sh_light_green="\[\033[1;32m\]"
sh_cyan="\[\033[0;36m\]"
sh_light_cyan="\[\033[1;36m\]"
sh_red="\[\033[0;31m\]"
sh_light_red="\[\033[1;31m\]"
sh_purple="\[\033[0;35m\]"
sh_light_purple="\[\033[1;35m\]"
sh_brown="\[\033[0;33m\]"
sh_yellow="\[\033[1;33m\]"
sh_light_gray="\[\033[0;37m\]"
sh_white="\[\033[1;37m\]"

case `hostname` in
    "livehost"|"production_server"|"sauron")
        HOSTCOLOUR=${sh_red}
        ;;
    "staging-node")      HOSTCOLOUR=${sh_yellow} ;;
    *)              HOSTCOLOUR=${sh_green} ;;
esac

export PROMPT_COMMAND='if [ $? -ne 0 ];then ERROR_FLAG=1;else ERROR_FLAG=;fi; '
export PS1=${sh_white}'\u@'${HOSTCOLOUR}'\h'${sh_norm}' \w\n'${sh_norm}'
           ${ERROR_FLAG:+'${sh_light_red}'}\$${ERROR_FLAG:+'${sh_norm}'} '
```


I'm also using the hostname to decide what colour the host appears in the prompt. My home directory, and thus .bashrc, is mounted on most hosts I log in to, and this serves as a reminder if I'm logged in to a production host. Green is the default, and it's overridden for various special hosts.

