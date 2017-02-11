;;; function.el --- Test for function signature of go-mode.el

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
;; Tests for Signature String
;;

(ert-deftest one-argument-no-retval ()
  "Function has one argument and no return value"
  (with-go-temp-buffer
    "
package main
func foo(arg int) {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg int) "))
      (should (string= got expected)))))

(ert-deftest one-argument-one-retval ()
  "Function has one argument and one return value"
  (with-go-temp-buffer
    "
package main
func foo(arg int) int {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg int) int"))
      (should (string= got expected)))))

(ert-deftest one-argument-one-retval-has-name ()
  "Function has one argument and one return value has name"
  (with-go-temp-buffer
    "
package main
func foo(arg int) (ret int) {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg int) (ret int)"))
      (should (string= got expected)))))

(ert-deftest one-argument-contains-underscore ()
  "Function has one argument contains underscore(#8)"
  (with-go-temp-buffer
    "
package main
func foo(arg_name int) (ret int) {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg_name int) (ret int)"))
      (should (string= got expected)))))

(ert-deftest one-argument-multiple-retvals ()
  "Function has one argument and multiple return values"
  (with-go-temp-buffer
    "
package main
func foo(arg int) (int, int) {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg int) (int, int)"))
      (should (string= got expected)))))

(ert-deftest multiple-arguments-one-retval ()
  "Function has multiple arguments and one return value"
  (with-go-temp-buffer
    "
package main
func foo(arg1 int, arg2 int) int {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg1 int, arg2 int) int"))
      (should (string= got expected)))))

(ert-deftest multiple-arguments-multiple-retval ()
  "Function has multiple arguments and multiple return values"
  (with-go-temp-buffer
    "
package main
func foo(arg1 int, arg2 string) (int, string) {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg1 int, arg2 string) (int, string)"))
      (should (string= got expected)))))

(ert-deftest multiple-arguments-multiple-retval-have-names ()
  "Function has multiple arguments and multiple return values have names"
  (with-go-temp-buffer
    "
package main
func foo(arg1 int, arg2 string) (ret1 int, ret2 string) {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg1 int, arg2 string) (ret1 int, ret2 string)"))
      (should (string= got expected)))))

(ert-deftest multiple-arguments-omit-type ()
  "Function has multiple argumes and multple return values but type is omitted"
  (with-go-temp-buffer
    "
package main
func foo(arg1, arg2 string) (ret1, ret2 int) {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg1, arg2 string) (ret1, ret2 int)"))
      (should (string= got expected)))))

(ert-deftest arguments-in-multiple-lines ()
  "Arguments in multiple lines"
  (with-go-temp-buffer
    "
package main
func foo(
     arg1 int,
     arg2 bool,
     arg3 string
) float {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg1 int, arg2 bool, arg3 string) float"))
      (should (string= got expected)))))

(ert-deftest channel-argument ()
  "Function has channel argument"
  (with-go-temp-buffer
    "
package main
func foo(ch <-chan string) int {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (ch <-chan string) int"))
      (should (string= got expected)))))

(ert-deftest channel-argument-and-channel-retval ()
  "Function has one argument and one return value"
  (with-go-temp-buffer
    "
package main
func foo(ch <-chan string) chan<- bool {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (ch <-chan string) (chan<- bool)"))
      (should (string= got expected)))))

(ert-deftest list-argument ()
  "Function has list argument"
  (with-go-temp-buffer
    "
package main
func foo(arg []int) int {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg []int) int"))
      (should (string= got expected)))))

(ert-deftest list-return-value ()
  "Function has channel argument"
  (with-go-temp-buffer
    "
package main
func foo(arg int) []int {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg int) []int"))
      (should (string= got expected)))))

(ert-deftest interface-argument ()
  "Function has interface argument"
  (with-go-temp-buffer
    "
package main
func foo(arg interface{}) int {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg interface{}) int"))
      (should (string= got expected)))))

(ert-deftest interface-return-value ()
  "Function has interface return value"
  (with-go-temp-buffer
    "
package main
func foo(arg int) interface{} {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg int) interface{}"))
      (should (string= got expected)))))

(ert-deftest interface-return-value ()
  "Function has interface return value"
  (with-go-temp-buffer
    "
package main
func foo(arg int) interface{} {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg int) interface{}"))
      (should (string= got expected)))))

(ert-deftest list-of-channel-argument ()
  "Function has list of channels argument"
  (with-go-temp-buffer
    "
package main
func foo(ch [2]string) int {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (ch [2]string) int"))
      (should (string= got expected)))))

(ert-deftest list-of-interface-argument ()
  "Function has list of channels argument"
  (with-go-temp-buffer
    "
package main
func foo(arg [999]interface{}) int {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg [999]interface{}) int"))
      (should (string= got expected)))))

(ert-deftest slice-of-channel-argument ()
  "Function has slice of channels argument"
  (with-go-temp-buffer
    "
package main
func foo(ch []string) int {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (ch []string) int"))
      (should (string= got expected)))))

(ert-deftest slice-of-interface-argument ()
  "Function has slice of channels argument"
  (with-go-temp-buffer
    "
package main
func foo(arg []interface{}) int {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (arg []interface{}) int"))
      (should (string= got expected)))))

(ert-deftest type-only-argument ()
  "Function has type only argument"
  (with-go-temp-buffer
    "
package main
import \"time\"
type test_interface interface{
    test_mf(arg time.Duration)
}

func main() {
	var ti test_interface
	ti.test_mf( )
}
"
    (forward-cursor-on "main")
    (forward-cursor-on "main")
    (forward-cursor-on "( )")
    (forward-char 1)

    (let ((got (go-eldoc--documentation-function))
          (expected "test_mf: (arg time.Duration) "))
      (should (string= got expected)))))

(ert-deftest type-only-argument-complex ()
  "Function has type only argument which is complex(#10)"
  (with-go-temp-buffer
    "
package main
import \"time\"
type test_interface interface{
    test_ms([]time.Duration)
}

func main() {
	var ti test_interface
	ti.test_ms( )
}
"
    (forward-cursor-on "main")
    (forward-cursor-on "main")
    (forward-cursor-on "( )")
    (forward-char 1)

    (let ((got (go-eldoc--documentation-function))
          (expected "test_ms: ([]time.Duration) "))
      (should (string= got expected)))))

(ert-deftest parsing-regexp-replace-all-func ()
  "Parsing Regexp.ReplaceAllFunc(#23)"
  (with-go-temp-buffer
    "
package main
import \"regexp\"

var re = regexp.MustCompile(`foo`)

func main() {
        re.ReplaceAllFunc( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)

    (let ((got (go-eldoc--documentation-function))
          (expected "ReplaceAllFunc: (src []byte, repl func([]byte) []byte) []byte"))
      (should (string= got expected)))))

(ert-deftest function-argument-return-channel ()
  "function argument which returns channel"
  (with-go-temp-buffer
    "
package main

func foo(bar func (a int, b int) chan<- string, baz int) int {
}

func main() {
        foo( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: (bar func(a int, b int) (chan<- string), baz int) int"))
      (should (string= got expected)))))

(ert-deftest case-sensitive-match-function ()
  "Match only case sensitive"
  (with-go-temp-buffer
    "
package main

import \"os\"

func main() {
        os.MkDIR( )
        os.Mkdir( )
}
"
    (forward-cursor-on "( )")
    (forward-char 1)
    (should-not (go-eldoc--documentation-function))

    (forward-cursor-on "( )")
    (forward-char 1)
    (should (go-eldoc--documentation-function))))

(ert-deftest array-index-issue ()
  "Bug in array bracket"
  (with-go-temp-buffer
    "
package main

import \"net/http\"

const USERS = []string{
        \"foo\"
}

func main() {
        http.HandleFunc(\"/\", func(w http.ResponseWriter, r *http.Request) {
		user := USERS[i]
	})
}
"
    (forward-cursor-on "\\[i\\]")
    (forward-char 1)
    (should-not (go-eldoc--documentation-function))))

;;; function.el end here
