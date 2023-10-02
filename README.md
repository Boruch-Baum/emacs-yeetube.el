# Yeetube

## About 
- Emacs YouTube Front-End

This package provides the ability to scrape YouTube, with the results
displayed in a proced-like buffer. 

*Inspired by [ytfzf](https://github.com/pystardust/ytfzf) & [ytel](https://github.com/grastello/ytel)*


*This package does not use Invidious or YouTube's API*


## Installation 

### Straight.el

``` emacs-lisp
(straight-use-package 
 '(yeetube :type git
	       :host nil
	       :repo "https://git.thanosapollo.com/yeetube"))
```


### Manual
``` shell
$ git clone https://git.thanosapollo.com/yeetube
```

Add this to your emacs configuration
``` emacs-lisp
(add-to-list 'load-path "/path/to/yeetube")
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
(setf yeetube-player 'emms-play-url)
```
