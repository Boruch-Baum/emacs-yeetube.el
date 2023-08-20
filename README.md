# Yeetube.el
Tests: [![builds.sr.ht status](https://builds.sr.ht/~thanosapollo/yeetube.el.svg)](https://builds.sr.ht/~thanosapollo/yeetube.el?)

Repo: [![Sourcehut](https://img.shields.io/badge/Master-sourcehut-lightgrey.svg?logo=sourcehut)](https://sr.ht/~thanosapollo/yeetube.el)

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

*Debian/Ubuntu*
``` shell
sudo apt install mpv yt-dlp
```

### Configuration 
#### Media Player 
Changing `yeetube-player` example: 

``` emacs-lisp
(setq yeetube-player 'vlc)
```

You can also use [mpv.el](https://github.com/kljohann/mpv.el) or other
similar packages with yeetube by simple redefining `yeetube-play-url`
to something like this:

``` emacs-lisp
  (defun yeetube-play-url (url)
    "Open URL using mpv-play-url."
    (when (string-prefix-p "http" url)
      (mpv-play-url url)))
```

## Contributing 

- [Mailing list](https://lists.sr.ht/~thanosapollo/yeetube.el)
- [Tickets](https://todo.sr.ht/~thanosapollo/yeetube.el)
