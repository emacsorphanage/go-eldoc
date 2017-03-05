;;; go-eldoc.el --- eldoc for go-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-go-eldoc
;; Version: 0.30
;; Package-Requires: ((emacs "24.3") (go-mode "1.0.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `go-eldoc.el' provides eldoc for Go language. `go-eldoc.el' shows type information
;; for variable, functions and current argument position of function.

;; To use this package, add these lines to your init.el file:
;;
;;     (require 'go-eldoc)
;;     (add-hook 'go-mode-hook 'go-eldoc-setup)
;;

;;; Code:

(require 'cl-lib)

(require 'eldoc)
(require 'go-mode)
(require 'thingatpt)

(defgroup go-eldoc nil
  "Eldoc for golang"
  :group 'go
  :prefix "go-eldoc-")

(defcustom go-eldoc-gocode "gocode"
  "gocode path"
  :type 'string)

(defcustom go-eldoc-gocode-args nil
  "Additional arguments to pass to `gocode'"
  :type '(repeat string))

(defvar go-eldoc--builtins
  '(("append"  . "append,,func(slice []Type, elems ...Type) []Type")
    ("close"   . "close,,func(c chan<- Type)")
    ("delete"  . "delete,,func(m map[Type]Type1, key Type)")
    ("panic"   . "panic,,func(v interface{})")
    ("recover" . "recover,,func() interface{}")
    ("complex" . "complex,,func(r, i FloatType) ComplexType")
    ("imag"    . "imag,,func(c ComplexType) FloatType")
    ("real"    . "real,,func(c ComplexType) FloatType")
    ("new"     . "new,,func(Type) *Type")
    ("cap"     . "cap,,func(v Type) int")
    ("copy"    .  "copy,,func(dst, src []Type) int")
    ("len"     . "len,,func(v Type) int")))

(defun go-eldoc--current-arg-index (curpoint)
  (save-excursion
    (let ((count 1)
          (start-level (go-paren-level)))
      (while (search-forward "," curpoint t)
        (when (and (not (go-in-string-or-comment-p))
                   (= start-level (1- (go-paren-level))))
          (cl-incf count)))
      count)))

(defun go-eldoc--count-string (str from to)
  (goto-char from)
  (cl-loop while (search-forward str to t)
           unless (go-in-string-or-comment-p)
           counting 1))

(defun go-eldoc--inside-funcall-p (from to)
  (save-excursion
    (let ((left-paren (go-eldoc--count-string "(" from to))
          (right-paren (go-eldoc--count-string ")" from to)))
      (> left-paren right-paren))))

(defsubst go-eldoc--goto-opening-parenthesis ()
  (and (ignore-errors (backward-up-list) t)
       (eql (char-after) ?\()))

(defun go-eldoc--inside-anon-function-p (from to)
  (save-excursion
    (goto-char to)
    (when (go-eldoc--goto-opening-parenthesis)
      (when (char-equal (char-after) ?\{)
        (let ((func-start (point))
              (case-fold-search nil))
          (goto-char from)
          (re-search-forward "\\<func\\s-*(" func-start t))))))

(defun go-eldoc--make-type ()
  (save-excursion
    (let ((cur (point)))
      (when (re-search-forward "[,)]" (line-end-position) t)
        (backward-char 1)
        (skip-chars-backward "[:space:]")
        (buffer-substring-no-properties (1+ cur) (point))))))

(defun go-eldoc--make-signature (type index)
  (when (or (not type) (string= type ""))
    (setq type "Type"))
  (if (= index 3)
      (format "make,,func(%s, size IntegerType, capacity IntegerType) %s" type type)
    (format "make,,func(%s, size IntegerType) %s" type type)))

(defun go-eldoc--search-builtin-functions (symbol curpoint)
  (if (string= symbol "make")
      (let ((index (go-eldoc--current-arg-index curpoint)))
        (go-eldoc--make-signature (go-eldoc--make-type) index))
    (assoc-default symbol go-eldoc--builtins)))

(defun go-eldoc--match-candidates (candidates cur-symbol curpoint)
  (when (and candidates (stringp candidates))
    (let* ((cands (if (string= candidates "")
                      (go-eldoc--search-builtin-functions cur-symbol curpoint)
                    candidates))
           (regexp (format "^\\(%s,,\\(?:func\\|type\\).+\\)$" cur-symbol))
           (case-fold-search nil))
      (when (and cands (string-match regexp cands))
        (match-string-no-properties 1 cands)))))

(defun go-eldoc--begining-of-funcall-p ()
  (and (= (char-after) ?\()
       (looking-back (concat go-identifier-regexp "\\s-*") nil)
       (not (string= "func" (thing-at-point 'word)))))

(defun go-eldoc--goto-beginning-of-funcall ()
  (cl-loop with old-point = (point)
           with retval = nil
           while (and (go-eldoc--goto-opening-parenthesis)
                      (not (bobp))
                      (not (= old-point (point)))
                      (progn
                        (setq retval (go-eldoc--begining-of-funcall-p))
                        (not retval)))
           do
           (setq old-point (point))
           finally return retval))

(defun go-eldoc--invoke-autocomplete ()
  (let ((temp-buffer (get-buffer-create "*go-eldoc*"))
        (gocode-args (append go-eldoc-gocode-args
                             (list "-f=emacs"
                                   "autocomplete"
                                   (or (buffer-file-name) "")
                                   (concat "c" (int-to-string (- (point) 1)))))))
    (unwind-protect
        (progn
          (apply #'call-process-region
                 (point-min)
                 (point-max)
                 go-eldoc-gocode
                 nil
                 temp-buffer
                 nil
                 gocode-args)
          (with-current-buffer temp-buffer
            (buffer-string)))
      (kill-buffer temp-buffer))))

(defsubst go-eldoc--assignment-index (lhs)
  (1+ (cl-loop for c across lhs
               when (= c ?,)
               sum 1)))

(defsubst go-eldoc--has-paren-same-line-p ()
  (save-excursion
    (re-search-forward "[({\\[]" (line-end-position) t)))

(defun go-eldoc--goto-last-funcall (limit)
  (let ((level (car (syntax-ppss)))
        pos)
    (save-excursion
      (while (re-search-forward "[[:word:][:multibyte:]]\\s-*+(" limit t)
        (when (= level (1- (car (syntax-ppss))))
          (setq pos (point)))))
    (when pos
      (goto-char pos))))

(defun go-eldoc--goto-statement-end ()
  (let ((limit (line-end-position)))
    (if (re-search-forward ")\\s-*;" limit t)
        (goto-char (match-beginning 0))
      (when (and (go-eldoc--has-paren-same-line-p)
                 (go-eldoc--goto-last-funcall limit))
        (go-eldoc--goto-opening-parenthesis)
        (forward-list)
        (goto-char (1- (point)))))))

(defun go-eldoc--lhs-p (curpoint)
  (save-excursion
    (let ((limit (line-end-position)))
      (when (search-forward ";" limit t)
        (setq limit (1- (point))))
      (goto-char curpoint)
      (and (re-search-forward ":?=" limit t)
           (not (go-in-string-or-comment-p))))))

(defun go-eldoc--assignment-p (curpoint)
  (when (and (not (looking-at-p "\\s-+")) (go-eldoc--lhs-p curpoint))
    (let ((lhs (buffer-substring-no-properties (line-beginning-position) curpoint)))
      (when (go-eldoc--goto-statement-end)
        (- (go-eldoc--assignment-index lhs))))))

(defun go-eldoc--get-funcinfo ()
  (save-excursion
    (let ((curpoint (point))
          assignment-index)
      (if (go-in-string-or-comment-p)
          (go-goto-beginning-of-string-or-comment)
        (when (setq assignment-index (go-eldoc--assignment-p curpoint))
          (setq curpoint (point))))
      (when (go-eldoc--goto-beginning-of-funcall)
        (when (and (go-eldoc--inside-funcall-p (1- (point)) curpoint)
                   (not (go-eldoc--inside-anon-function-p (1- (point)) curpoint)))
          (let ((matched (go-eldoc--match-candidates
                          (go-eldoc--invoke-autocomplete) (thing-at-point 'symbol)
                          curpoint)))
            (when (and matched
                       (string-match "\\`\\(.+?\\),,\\(.+\\)$" matched))
              (let ((funcname (match-string-no-properties 1 matched))
                    (signature (match-string-no-properties 2 matched)))
                (list :name funcname :signature signature
                      :index (or assignment-index
                                 (go-eldoc--current-arg-index curpoint)))))))))))

(defsubst go-eldoc--no-argument-p (arg-type)
  (string-match-p "\\`\\s-+\\'" arg-type))

(defconst go-eldoc--argument-type-regexp
  (concat
   "\\([]{}[:word:][:multibyte:]*.[]+\\)" ;; $1 argname
   (format "\\(?: %s%s\\)?"
           "\\(\\(?:\\[\\]\\)?\\(?:<-\\)?chan\\(?:<-\\)? \\)?" ;; $2 channel
           "\\(?:\\([]{}[:word:][:multibyte:]*.[]+\\)\\)?") ;; $3 argtype
   ))

(defun go-eldoc--extract-type-name (chan sym)
  (when sym
    (if (or (not chan) (string= chan ""))
        sym
      (concat chan sym))))

(defun go-eldoc--split-types-string (arg-type)
  (with-temp-buffer
    (set-syntax-table go-mode-syntax-table)
    (insert arg-type)
    (goto-char (point-min))
    (let ((name-types nil))
      (while (re-search-forward go-eldoc--argument-type-regexp nil t)
        (let* ((name (match-string-no-properties 1))
               (type (go-eldoc--extract-type-name
                      (match-string-no-properties 2)
                      (match-string-no-properties 3)))
               (name-type (if type
                              (concat name " " type)
                            name))
               (end (match-end 0)))
          (when (or (string= type "func") (and (not type) (string= name "func")))
            (forward-list)
            (cond ((looking-at (concat "\\s-*" go-eldoc--argument-type-regexp))
                   (goto-char (match-end 0)))
                  ((looking-at-p "\\s-*(")
                   (skip-chars-forward " \t")
                   (forward-list)))
            (setq name-type (concat name-type
                                    (buffer-substring-no-properties end (point)))))
          (push name-type name-types)))
      (reverse name-types))))

(defsubst go-eldoc--has-spaces (str)
  (string-match-p "[[:space:]]" str))

(defun go-eldoc--wrap-parenthesis (str len rettype)
  ;; Don't wrap if return value is only one
  (if (and rettype (<= len 1) (not (go-eldoc--has-spaces str)))
      str
    (concat "(" str ")")))

(defun go-eldoc--highlight-index-position (types-str index &optional rettype-p)
  (cl-loop with types = (go-eldoc--split-types-string types-str)
           with highlight-done = nil
           with len = (length types)
           with last-index = (1- len)
           for i from 0 below len
           for type in types
           if (and (not highlight-done)
                   (or (= i (1- index))
                       (and (= i last-index)
                            (string-match-p "\\.\\{3\\}" type))))
           collect
           (progn
             (setq highlight-done t)
             (propertize type 'face 'eldoc-highlight-function-argument)) into args

           else
           collect type into args
           finally return (go-eldoc--wrap-parenthesis
                           (mapconcat 'identity args ", ") len rettype-p)))

(defun go-eldoc--highlight-argument (signature index)
  (let* ((arg-type (plist-get signature :arg-type))
         (ret-type (plist-get signature :ret-type)))
    (if (go-eldoc--no-argument-p arg-type)
        (concat "() " ret-type)
      (if (> index 0)
          (let ((highlighed-args (go-eldoc--highlight-index-position arg-type index)))
            (concat highlighed-args " " ret-type))
        (let ((highlighed-rets (go-eldoc--highlight-index-position ret-type (- index) t)))
          (concat "(" arg-type ") " highlighed-rets))))))

(defun go-eldoc--analyze-func-signature ()
  (let (arg-start arg-end)
    (when (search-forward "func(" nil t)
      (setq arg-start (point))
      (backward-char 1)
      (when (ignore-errors (forward-list) t)
        (setq arg-end (1- (point)))
        (skip-chars-forward " \t")
        (list :type 'function
              :arg-type (buffer-substring-no-properties arg-start arg-end)
              :ret-type (buffer-substring-no-properties (point) (point-max)))))))

(defun go-eldoc--analyze-type-signature ()
  (when (search-forward "type " nil t)
    (list :type 'type
          :real-type (buffer-substring-no-properties (point) (point-max)))))

(defun go-eldoc--analyze-signature (signature)
  (with-temp-buffer
    (set-syntax-table go-mode-syntax-table)
    (insert signature)
    (goto-char (point-min))
    (let ((word (thing-at-point 'word)))
      (cond ((string= "func" word)
             (go-eldoc--analyze-func-signature))
            ((string= "type" word)
             (go-eldoc--analyze-type-signature))))))

(defun go-eldoc--format-signature (funcinfo)
  (let ((name (plist-get funcinfo :name))
        (signature (go-eldoc--analyze-signature (plist-get funcinfo :signature)))
        (index (plist-get funcinfo :index)))
    (when signature
      (cl-case (plist-get signature :type)
        (function
         (format "%s: %s"
                 (propertize name 'face 'font-lock-function-name-face)
                 (go-eldoc--highlight-argument signature index)))
        (type
         (format "%s: %s"
                 (propertize name 'face 'font-lock-type-face)
                 (plist-get signature :real-type)))))))

(defun go-eldoc--retrieve-type (typeinfo symbol)
  (let ((case-fold-search nil))
    (cond ((string-match (format "^%s,,var \\(.+\\)$" symbol) typeinfo)
           (match-string-no-properties 1 typeinfo))
          ((string-match-p (format "\\`%s,,package\\s-*$" symbol) typeinfo)
           "package")
          ((string-match (format "^%s,,\\(func.+\\)$" symbol) typeinfo)
           (match-string-no-properties 1 typeinfo))
          ((string-match (format "^%s,,\\(.+\\)$" symbol) typeinfo)
           (match-string-no-properties 1 typeinfo)))))

(defun go-eldoc--get-cursor-info (bounds)
  (save-excursion
    (goto-char (cdr bounds))
    (go-eldoc--retrieve-type
     (go-eldoc--invoke-autocomplete)
     (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(defun go-eldoc--retrieve-concrete-name (bounds)
  (save-excursion
    (goto-char (car bounds))
    (while (looking-back "\\." (1- (point)))
      (backward-char 1)
      (skip-chars-backward "[:word:][:multibyte:]\\[\\]"))
    (buffer-substring-no-properties (point) (cdr bounds))))

(defun go-eldoc--bounds-of-go-symbol ()
  (save-excursion
    (let (start)
      (skip-chars-backward "[:word:][:multibyte:]")
      (setq start (point))
      (skip-chars-forward "[:word:][:multibyte:]")
      (unless (= start (point))
        (cons start (point))))))

(defsubst go-eldoc--propertize-cursor-thing (bounds)
  (propertize (go-eldoc--retrieve-concrete-name bounds)
              'face 'font-lock-variable-name-face))

(defun go-eldoc--documentation-function ()
  (let ((funcinfo (go-eldoc--get-funcinfo)))
    (if funcinfo
        (go-eldoc--format-signature funcinfo)
      (let ((bounds (go-eldoc--bounds-of-go-symbol)))
        (when bounds
          (let ((curinfo (go-eldoc--get-cursor-info bounds)))
            (when curinfo
              (format "%s: %s"
                      (go-eldoc--propertize-cursor-thing bounds)
                      curinfo))))))))

;;;###autoload
(defun go-eldoc-setup ()
  "Set up eldoc function and enable eldoc-mode."
  (interactive)
  (setq-local eldoc-documentation-function #'go-eldoc--documentation-function)
  (eldoc-mode +1))

(provide 'go-eldoc)

;;; go-eldoc.el ends here
