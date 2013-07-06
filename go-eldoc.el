;;; go-eldoc.el --- eldoc for go-mode

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-go-eldoc
;; Version: 0.01
;; Package-Requires: ((go-mode "0") (go-autocomplete "0"))

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

;; To use this package, add these lines to your .emacs file:
;;
;;     (require 'go-eldoc)
;;     (add-hook go-mode-hook 'go-eldoc-setup)
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'eldoc)
(require 'go-mode)
(require 'go-autocomplete)
(require 'thingatpt)

(defgroup go-eldoc nil
  "Eldoc for golang"
  :group 'go
  :prefix "go-eldoc-")

(defun go-eldoc--current-arg-index (curpoint)
  (save-excursion
    (let ((count 1))
      (while (search-forward "," curpoint t)
        (unless (go-in-string-or-comment-p)
          (setq count (1+ count))))
      count)))

(defun go-eldoc--count-string (str from to)
  (save-excursion
    (goto-char from)
    (loop while (search-forward str to t)
          counting 1)))

(defun go-eldoc--inside-funcall-p (from to)
  (save-excursion
    (let ((left-paren (go-eldoc--count-string "(" from to))
          (right-paren (go-eldoc--count-string ")" from to)))
      (> left-paren right-paren))))

(defun go-eldoc--inside-anon-function-p (from to)
  (save-excursion
    (goto-char from)
    (re-search-forward "\\<func\\s-*(" to t)))

(defun go-eldoc--match-candidates (candidates cur-symbol)
  (when (and candidates (stringp candidates))
    (let ((regexp (format "^\\(%s,,\\(?:func\\|type\\).+\\)$" cur-symbol)))
      (when (string-match regexp candidates)
        (match-string-no-properties 1 candidates)))))

(defun go-eldoc--begining-of-funcall-p ()
  (let ((curpoint (point)))
    (save-excursion
      (skip-chars-backward "a-zA-Z0-9_ ")
      (string-match "[a-zA-Z0-9_]+\\s-*("
                    (buffer-substring-no-properties
                     (point) (1+ curpoint))))))

(defun go-eldoc--goto-beginning-of-funcall ()
  (loop with old-point = (point)
        initially (go-goto-opening-parenthesis)
        while (and (not (bobp))
                   (not (= old-point (point)))
                   (not (go-eldoc--begining-of-funcall-p)))
        do
        (setq old-point (point))
        (go-goto-opening-parenthesis)
        finally return (go-eldoc--begining-of-funcall-p)))

(defun go-eldoc--get-funcinfo ()
  (let ((curpoint (point)))
    (save-excursion
      (when (go-in-string-or-comment-p)
        (go-goto-beginning-of-string-or-comment))
      (when (go-eldoc--goto-beginning-of-funcall)
        (when (and (go-eldoc--inside-funcall-p (1- (point)) curpoint)
                   (not (go-eldoc--inside-anon-function-p (1- (point)) curpoint)))
          (let ((matched (go-eldoc--match-candidates
                          (ac-go-invoke-autocomplete) (thing-at-point 'symbol))))
            (when (and matched
                       (string-match "\\`\\(.+?\\),,\\(.+\\)$" matched))
              (list :name (match-string-no-properties 1 matched)
                    :signature (match-string-no-properties 2 matched)
                    :index (go-eldoc--current-arg-index curpoint)))))))))

(defun go-eldoc--no-argument-p (arg-type)
  (string-match "\\`\\s-+\\'" arg-type))

(defun go-eldoc--split-argument-type (arg-type)
  (with-temp-buffer
    (insert arg-type)
    (goto-char (point-min))
    (let ((name-types nil))
      (while (re-search-forward "\\([a-zA-Z0-9_]+\\) \\([]{}a-zA-Z0-9_*.[]+\\)" nil t)
        (let* ((name (match-string-no-properties 1))
               (type (match-string-no-properties 2))
               (name-type (concat name " " type))
               (end (match-end 0)))
          (when (string= type "func")
            (forward-list)
            (setq name-type (concat name-type
                                    (buffer-substring-no-properties end (point)))))
          (push name-type name-types)))
      (reverse name-types))))

(defun go-eldoc--highlight-argument (signature index)
  (let* ((arg-type (plist-get signature :arg-type))
         (ret-type (plist-get signature :ret-type))
         (types (go-eldoc--split-argument-type arg-type)))
    (if (go-eldoc--no-argument-p arg-type)
        (concat "() " ret-type)
      (loop with highlight-done = nil
            with arg-len = (length types)
            for i from 0 to arg-len
            for type in types
            if (and (not highlight-done)
                    (or (= i (1- index))
                        (and (= i (1- arg-len))
                             (string-match "\\.\\{3\\}" type))))
            collect
            (progn
              (setq highlight-done t)
              (propertize type 'face 'eldoc-highlight-function-argument)) into args

            else
            collect type into args
            finally
            return (concat "(" (mapconcat 'identity args ", ") ") " ret-type)))))

(defun go-eldoc--analyze-func-signature (signature)
  (let (arg-start arg-end)
    (when (search-forward "func(" nil t)
      (setq arg-start (point))
      (backward-char 1)
      (forward-list)
      (setq arg-end (1- (point)))
      (skip-chars-forward "[[:space:]]")
      (list :type 'function
            :arg-type (buffer-substring-no-properties arg-start arg-end)
            :ret-type (buffer-substring-no-properties (point) (point-max))))))

(defun go-eldoc--analyze-type-signature (signature)
  (when (search-forward "type " nil t)
    (list :type 'type
          :real-type (buffer-substring-no-properties (point) (point-max)))))

(defun go-eldoc--analyze-signature (signature)
  (with-temp-buffer
    (insert signature)
    (goto-char (point-min))
    (let ((word (thing-at-point 'word)))
      (cond ((string= "func" word)
             (go-eldoc--analyze-func-signature signature))
            ((string= "type" word)
             (go-eldoc--analyze-type-signature signature))))))

(defun go-eldoc--format-signature (funcinfo)
  (let ((name (plist-get funcinfo :name))
        (signature (go-eldoc--analyze-signature (plist-get funcinfo :signature)))
        (index (plist-get funcinfo :index)))
    (when signature
      (case (plist-get signature :type)
        (function
         (format "%s: %s"
                 (propertize name 'face 'font-lock-function-name-face)
                 (go-eldoc--highlight-argument signature index)))
        (type
         (format "%s: %s"
                 (propertize name 'face 'font-lock-type-face)
                 (plist-get signature :real-type)))))))

(defun go-eldoc--documentation-function ()
  (let ((funcinfo (go-eldoc--get-funcinfo)))
    (when funcinfo
      (go-eldoc--format-signature funcinfo))))

;;;###autoload
(defun go-eldoc-setup ()
  (interactive)
  (set (make-local-variable 'eldoc-documentation-function)
       'go-eldoc--documentation-function)
  (turn-on-eldoc-mode))

(provide 'go-eldoc)

;;; go-eldoc.el ends here
