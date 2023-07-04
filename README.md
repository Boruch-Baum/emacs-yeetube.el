# Yeetube.el

## About 
- Watch & Download Videos from Emacs.

*For now the search feature is designed to work with YouTube*

### But why?

Because Emacs.


Differences with other front-ends:
- No YouTube/Invidious API
- Includes simple front-end for `yt-dlp` *not forcing you to use youtube urls* :)

## Installation

- Clone repo 
``` bash
git clone https://git.sr.ht/~thanosapollo/yeetube.el 
```

- Add the following to your `init.el`

``` emacs-lisp
(load-file "/path/to/yeetube.el"))
```


### Customization 
- `yeetube-results-prefix` Default is "+"

- `yeetube-query-url` Default is YouTube URL, soon there will be more sites
supported

- `yeetube-download-audio-format` Default is `nil`, you can use it to
specify your downloads to be only (audio) FORMAT, e.g "m4a"

- `yeetube-display-info-keys` Default is `t`, displays default keybindings

- `yeetube-player` Default is "mpv"

#### Tips & Tricks

You can easily define functions to execute your preferred
`yeetube-player` while incorporating different user flags. 

For instance, a function to switch between `mpv` and `mpv --no-video`
that allows for audio-only playback when desired.

``` emacs-lisp
  (defun yeetube-switch-mpv ()
    "Switching mpv from/to only audio."
    (interactive)
    (if (equal yeetube-player "mpv")
	    (setq yeetube-player "mpv --no-video")
      (setq yeetube-player "mpv")))
```
Add it as a keybinding to `yeetube-mode-map`

``` emacs-lisp
(define-key yeetube-mode-map (kbd "c") 'yeetube-switch-mpv)
```
#### Downloading non-Youtube links 

`yeetube-download-videos` is not specific to YouTube, it's just a
front end for `yt-dlp`. Open a Dired buffer and navigate to your
desired *PATH* and run `yeetube-download-videos` interactively.


## Contributing 

- [Mailing list](https://lists.sr.ht/~thanosapollo/yeetube.el)
  - Development takes places on `dev` branch  
- [Issue Tracker](https://todo.sr.ht/~thanosapollo/yeetube.el)



