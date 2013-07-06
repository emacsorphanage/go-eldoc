# go-eldoc.el

## Introduction
`go-eldoc.el` provides eldoc for go language.


## Screenshot

![go-eldoc1](image/go-eldoc1.png)


## Dependency

* [gocode](https://github.com/nsf/gocode)
* [go-mode](https://code.google.com/p/go/)
* [go-autocomplete](https://github.com/nsf/gocode)

You can install `go-mode` and `go-autocomplete` with package.el from MELPA.
And you can install `gocode` by `go get` as below.

```
% go get -u github.com/nsf/gocode
```


## Installation

You can install `go-eldoc.el` from MELPA with package.el.

```
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(package-refresh-contents)
```

and `M-x package-install go-eldoc`.


## Setup
Call `go-eldoc-setup` function at `go-mode-hook`

```elisp
(require 'go-eldoc) ;; Don't need to require, if you install by package.el
(add-hook go-mode-hook 'go-eldoc-setup)
```

## customize
You can change current argument index face by setting
`eldoc-highlight-function-argument` face as below.

```elisp
(set-face-attribute 'eldoc-highlight-function-argument nil
                     :underline t :foreground "green"
                     :weight 'bold)
```
