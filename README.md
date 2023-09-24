# Yeetube.el
 [![MELPA](https://melpa.org/packages/yeetube-badge.svg)](https://melpa.org/#/yeetube) [![builds.sr.ht status](https://builds.sr.ht/~thanosapollo/yeetube.el.svg)](https://builds.sr.ht/~thanosapollo/yeetube.el?)

## About 
- YouTube & Invidious Front-End for Emacs.

This package provides the ability to scrape YouTube or any Invidious
instance, with the results displayed in a read-only org-mode buffer.

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
By default `yeetube-player` is set to `yeetube-mpv`, you can use
[mpv.el](https://github.com/kljohann/mpv.el), [GNU/Emms](https://www.gnu.org/software/emms/) or other similar packages like so:

``` emacs-lisp
(setq yeetube-player 'emms-play-url)
```
