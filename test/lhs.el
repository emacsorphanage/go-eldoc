;;; lhs.el --- Test for left hand side highlighting

;; Copyright (C) 2017 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

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

;;; Code:

(require 'cl-lib)

(require 'ert)
(require 'go-eldoc)

;;
;; Tests for left hand side
;;

(ert-deftest one-return-value ()
  "Function has one return value"
  (with-go-temp-buffer
    "
package main
func foo(arg int) error {
}

func main () {
        err := foo(10)
}
"
    (goto-char (point-max))
    (backward-cursor-on "err")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg int) error"))
      (should (string= got expected))

      (let ((highlighted-part (substring got -5)))
        (should (eq (get-text-property 0 'face highlighted-part)
                    'eldoc-highlight-function-argument))))))

(ert-deftest one-return-value-with-name ()
  "Function has one named return value"
  (with-go-temp-buffer
    "
package main
func foo(arg int) (ret error) {
}

func main () {
        err := foo(10)
}
"
    (goto-char (point-max))
    (backward-cursor-on "err")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg int) (ret error)"))
      (should (string= got expected)))))

(ert-deftest multiple-return-values ()
  "Function has multile return values"
  (with-go-temp-buffer
    "
package main
func foo(arg int) (int, double, error) {
}

func main () {
        bar, baz, err := foo(10)
}
"
    (goto-char (point-max))
    (backward-cursor-on "bar")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg int) (int, double, error)"))
      (should (string= got expected))
      (should (eq (get-text-property 0 'face (substring got -19 -16))
                  'eldoc-highlight-function-argument))
      (should-not (get-text-property 0 'face (substring got -14 -8)))

      (forward-cursor-on "baz")
      (let ((got (go-eldoc--documentation-function)))
        (should (string= got expected))
        (should (eq (get-text-property 0 'face (substring got -14 -9))
                    'eldoc-highlight-function-argument))
        (should-not (get-text-property 0 'face (substring got -19 -16))))

      (forward-cursor-on "err")
      (let ((got (go-eldoc--documentation-function)))
        (should (string= got expected))
        (should (eq (get-text-property 0 'face (substring got -6 -2))
                    'eldoc-highlight-function-argument))
        (should-not (get-text-property 0 'face (substring got -19 -16)))
        (should-not (get-text-property 0 'face (substring got -14 -9)))))))

(ert-deftest separated-by-semicolon ()
  "assignment expression with semicolon"
  (with-go-temp-buffer
    "
package main
func foo(arg int) error {
}

func main () {
        if err := foo(10); err != nil {
        }
}
"
    (goto-char (point-max))
    (backward-cursor-on "if")
    (forward-cursor-on "err")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg int) error"))
      (should (string= got expected))

      (let ((highlighted-part (substring got -5)))
        (should (eq (get-text-property 0 'face highlighted-part)
                    'eldoc-highlight-function-argument))))

    (forward-cursor-on "10")
    (let* ((got (go-eldoc--documentation-function))
           (highlighted-part (substring got 6 13)))
      (should (eq (get-text-property 0 'face highlighted-part)
                  'eldoc-highlight-function-argument)))))

(ert-deftest return-function-type-with-no-name ()
  "function returns function type which has no name"
  (with-go-temp-buffer
    "
package main

import \"net/http\"
import \"error\"

func FollowRedirectsCallback(howmany int) func(r *http.Request, via []*http.Request) error {
}

func main() {
        fun := FollowRedirectsCallback(10)
}
"
    (forward-cursor-on "main" 2)
    (forward-cursor-on "fun")
    (let ((got (go-eldoc--documentation-function)))
      (should (eq (get-text-property 0 'face (substring got -20))
                  'eldoc-highlight-function-argument))

      (should (eq (get-text-property 0 'face (substring got -10))
                  'eldoc-highlight-function-argument))

      (should (eq (get-text-property 0 'face (substring got -5))
                  'eldoc-highlight-function-argument)))))

(ert-deftest dont-show-next-statement ()
  "go-eldoc must not show another statement information."
  (with-go-temp-buffer
    "
package main

import \"fmt\"

func Foo(a int) {
	fmt.Println(a)
}

type Bar struct {
	aaa int
}

func main() {
	bb := Bar{cc: 10}
	aa := bb.cc
	Foo(aa)
}
"
    (forward-cursor-on "main" 2)
    (forward-cursor-on "aa")
    (let ((got (go-eldoc--documentation-function)))
      (should-not got))))

(ert-deftest regression-test-26 ()
  "Regression test of #26. Show eldoc of left hand side variable
without any exceptions."
  (with-go-temp-buffer
    "
package main

type Profile map[string]string
type Requires Profile

func Get(name string, requires Requires) (*Profile, error) {
}

func main () {
     res, err := Get(\"foo\", Requires{
     })
}
"
    (goto-char (point-min))
    (forward-cursor-on "\\bres\\b")
    (should (go-eldoc--documentation-function))))

(ert-deftest assign-operator-in-comment-or-string ()
  "Ignore assignment operator in comment or string"
  (with-go-temp-buffer
    "
package main

import \"math/big\"

func main() {
        a := big.NewInt()
        var base int64 = 2
        exponent := 1

        a.Exp(base, big.NewInt(int64(exponent)), nil) //16^1 = 16
}
"
    (forward-cursor-on "base,")
    (let ((got (go-eldoc--documentation-function)))
      (should (string-match-p "Exp" got)))))

(ert-deftest method-chanining ()
  "Method chaining"
  (with-go-temp-buffer
    "
package main

type Foo struct {
}

func (f *Foo) f1() *Foo {
	return f
}

func (f *Foo) f2(a int) (*Foo, int) {
	return f, 10+a
}

func f3() int {
        return 3
}

func main() {
	f := &Foo{}
	a, b := f.f1().f2(f3())
}
"
    (forward-cursor-on "a,")
    (let ((got (go-eldoc--documentation-function)))
      (should (string= "f2: (a int) (*Foo, int)" got)))))

(ert-deftest funcall-in-backets ()
  "funcall in brackets"
  (with-go-temp-buffer
    "
package main

func f1() int {
        return 3
}

func main() {
        var a[10]
        b := a[f1()]
}
"
    (forward-cursor-on "b")
    ;; should not error
    (should (progn (go-eldoc--documentation-function) t))))

;;; lhs.el end here
