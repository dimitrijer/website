---
title: TIL that clear also nukes scrollback
htmltitle: TIL that clear also nukes scrollback
---

I often use `clear` to, well, clear my terminal screen. It's in my muscle
memory. The other day, after typing `clear` in tmux, I wanted to take another
look at some output from a couple of minutes ago. I was surprised to find out
that tmux scrollback buffer was gone.

Lo and behold, [the manual page](https://manpages.debian.org/testing/ncurses-bin/clear.1.en.html) does
state that `clear` also clears terminal scrollback buffer. It will look up
information about your current `TERM` in `terminfo` database to figure out how
to perform screen clearing, and it will clear the scrollback buffer as well,
but **only** for terminals with extended (E3) capability. Simply emitting a
sequence such as `printf '\033[3J'` will clear the scrollback buffer in
terminals with this capability.

It appears that I have used `screen-256color` as `TERM` on my personal laptop,
whereas on this machine it was set to `xterm-256color`, the latter having E3
capability.

In order to avoid scrollback clearing, either use `screen-256color` for tmux,
or simply use `clear -x` to clear only visible content of the screen. Also, it
seems that `clear` on OS X does not have this behaviour, as it is a part of
ncurses distribution that predates [this patch](https://invisible-island.net/ncurses/NEWS.html#t20130622).
