# go-eldoc.el [![travis badge][travis-badge]][travis-link] [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

## Introduction

`go-eldoc.el` provides eldoc for Go language. `go-eldoc.el` shows type information
for variable, functions and current argument position of function.


## Screenshot

### Argument Type of Function

![Screenshot of showing argument type of function](image/go-eldoc1.png)

### Variable Type Information

![Screenshot of showing variable type information](image/go-eldoc2.png)

### Return Value Type

![Screenshot of showing return value type](image/go-eldoc3.png)


## Dependency

* [gocode](https://github.com/nsf/gocode)
* [go-mode](https://github.com/dominikh/go-mode.el)

You can install `go-mode` with package.el from [MELPA](https://melpa.org/).
And you can install `gocode` by `go get` as below.

```
% go get -u github.com/nsf/gocode
```


## Installation

You can install `go-eldoc.el` from [MELPA](https://melpa.org/) with package.el.

```lisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)
```

evaluate this code and <kbd>M-x package-install go-eldoc</kbd>.


## Setup
Call `go-eldoc-setup` function at `go-mode-hook`

```lisp
(require 'go-eldoc) ;; Don't need to require, if you install by package.el
(add-hook 'go-mode-hook 'go-eldoc-setup)
```

## Customize
You can change face of current argument position by `eldoc-highlight-function-argument`
like this.

```lisp
(set-face-attribute 'eldoc-highlight-function-argument nil
                    :underline t :foreground "green"
                    :weight 'bold)
```

##### `go-eldoc-gocode`(Default: `gocode`)

`gocode` location. You need not to set this variable if you set (`$GOPATH/bin`) to PATH environment variable correctly.

##### `go-eldoc-gocode-args`(Default: `nil`)

Arguments of `gocode` command. Type of this variable is list of strings.

[travis-badge]: https://travis-ci.org/syohex/emacs-go-eldoc.svg
[travis-link]: https://travis-ci.org/syohex/emacs-go-eldoc
[melpa-link]: https://melpa.org/#/go-eldoc
[melpa-stable-link]: https://stable.melpa.org/#/go-eldoc
[melpa-badge]: https://melpa.org/packages/go-eldoc-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/go-eldoc-badge.svg
