[![self-host-badge](https://img.shields.io/badge/Repository-Self_hosted-black?logo=git&logoColor=red)](https://git.thanosapollo.org/yeetube/about)
[![MELPA](https://melpa.org/packages/yeetube-badge.svg)](https://melpa.org/#/yeetube)

# Yeetube | Emacs' Youtube Front End

## About 

This package provides the ability to scrape YouTube, with the results
displayed in a proced-like buffer.


Package functionality includes:

- Query YouTube
- Play video url *by default using mpv*
- Bookmark/Save video url
- Download video *using yt-dlp*
- A minimal yt-dlp front-end, *which is independent of the rest
  YouTube functionality*, to download multiple urls.

*Inspired by [ytfzf](https://github.com/pystardust/ytfzf) &
[ytel](https://github.com/grastello/ytel)*

*This package does not use Invidious or YouTube's API, just parses
html & json.*


## Installation 
This package is available via [MELPA](https://melpa.org/#/yeetube)

### Straight.el

``` emacs-lisp
(straight-use-package 
 '(yeetube :type git
	       :host nil
	       :repo "https://git.thanosapollo.org/yeetube"))
```


### Manual
``` shell
$ git clone https://git.thanosapollo.org/yeetube
```

*Add this to your emacs configuration:*

``` emacs-lisp
   (add-to-list 'load-path "/path/to/yeetube")
   (load-file "~/path/to/yeetube.el")
   (require 'yeetube)
```

### Dependencies
- [mpv](https://mpv.io/): default multimedia player
- [yt-dlp](https://github.com/yt-dlp/yt-dlp): download functionality

*Debian/Ubuntu*
``` shell
$ sudo apt install mpv yt-dlp
```

## Configuration 
### Media Player 
By default `yeetube-player` is set to `yeetube-mpv-play`, you can
use [mpv.el](https://github.com/kljohann/mpv.el),
[GNU/Emms](https://www.gnu.org/software/emms/) or other similar
packages like so:

``` emacs-lisp
(setf yeetube-player #'emms-play-url)
```

Make sure that the media player of your choice can directly play
youtube urls.

### FAQ
#### Feature request: display thumbnails
Formatting images in a text buffer is not something that I found easy
or fun to do. Feel free to submit a patch if you manage to make an
appealing version of it


