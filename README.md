# go-eldoc.el

## Introduction
`go-eldoc.el` provides eldoc for go language.


## Screenshot

![go-eldoc1](image/go-eldoc1.png)


## Setup
Call `go-eldoc-setup` function at `go-mode-hook`

```elisp
(require 'go-eldoc)
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
