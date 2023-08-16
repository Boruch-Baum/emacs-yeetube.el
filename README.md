# Yeetube.el
[![builds.sr.ht status](https://builds.sr.ht/~thanosapollo/yeetube.el.svg)](https://builds.sr.ht/~thanosapollo/yeetube.el?)

Available via
[![MELPA](https://melpa.org/packages/yeetube-badge.svg)](https://melpa.org/#/yeetube)

## About 
- YouTube & Invidious Front-End for Emacs.

This package provides the ability to scrape YouTube or any Invidious
instance, with the results displayed in a read-only org-mode buffer.

Key features:
 - Search video query
 - Play video URL, *by default with MPV*
 - Save video URL with a custom name/label
 - Download video, *this package serves also as a front-end for
   yt-dlp, thus supporting platforms beyond YouTube/Invidious.*



## Installation 
You can install it via [MELPA](https://melpa.org/#/yeetube)

*Or directly from source*
- Download the latest tar-archive or clone the repo 

``` shell
git clone https://git.sr.ht/~thanosapollo/yeetube.el
```

- Load `yeetube.el`

``` emacs-lisp
(load-file "~/path/to/yeetube.el")
```


### Dependencies
- [mpv](https://mpv.io/): default multimedia player 
- [yt-dlp](https://github.com/yt-dlp/yt-dlp): download functionality 
- socat: control mpv *e.g pause/play*

*Debian/Ubuntu*
``` shell
sudo apt install mpv yt-dlp socat
```

## Contributing 

- [Mailing list](https://lists.sr.ht/~thanosapollo/yeetube.el)
- [Tickets](https://todo.sr.ht/~thanosapollo/yeetube.el)
