;;; not-function.el --- Test for not function type, variable, package, etc

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
;; Variable name
;;

(ert-deftest local-variable ()
  "Show local variable type"
  (with-go-temp-buffer
    "
package main
func foo() {
        var v string = \"hello\"
        a := v + \" world\"
}
"
    (forward-cursor-on "\\bv\\b" 2)
    (let ((got (go-eldoc--documentation-function))
          (expected "v: string"))
      (should (string= got expected)))))

(ert-deftest local-variable-has-long-name ()
  "Show local variable type which has long name more than 1 chracter"
  (with-go-temp-buffer
    "
package main

func foo() {
        var this_is_variable float32 = 3.14
        a := this_is_variable + 1.0
        fmt.Println(this_is_variable)
}
"
    (forward-cursor-on "this_is_variable" 2)
    (let ((got (go-eldoc--documentation-function))
          (expected "this_is_variable: float32"))
      (should (string= got expected)))))

(ert-deftest argument-variable ()
  "Show argument variable"
  (with-go-temp-buffer
    "
package main
func foo(arg []byte) {
        arg = []byte{'a', 'b', 'c'}
}
"
    (forward-cursor-on "arg = ")
    (let ((got (go-eldoc--documentation-function))
          (expected "arg: []byte"))
      (should (string= got expected)))))

(ert-deftest global-variable ()
  "Show global variable with typed declaration"
  (with-go-temp-buffer
    "
package main
var global string = \"GlobalVariable\"
func foo() {
     v := global
}
"
    (goto-char (point-max))
    (backward-cursor-on "global")
    (let ((got (go-eldoc--documentation-function))
          (expected "global: string"))
      (should (string= got expected)))))

(ert-deftest function-type ()
  "Show function type variable"
  (with-go-temp-buffer
    "
package main
func foo(a int, b int) int {
     return a + b
}
func main() {
     funcvar := foo
     c := funcvar(1, 2)
}
"
    (forward-cursor-on "funcvar(")
    (let ((got (go-eldoc--documentation-function))
          (expected "funcvar: func(a int, b int) int"))
      (should (string= got expected)))))

(ert-deftest array-index ()
  "Show array index variable"
  (with-go-temp-buffer
    "
package main
func main() {
     var foo int = 2
     bar := []int{0, 1, 2}
     bar[foo]
}
"
    (forward-cursor-on "foo\\]")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo: int"))
      (should (string= got expected)))))

(ert-deftest array-element-property ()
  "Show array elemet property"
  (with-go-temp-buffer
    "
package main

type Foo struct { bar int }

func main() {
     var foo [10]Foo
     foo[9].bar
}
"
    (forward-cursor-on "\\.bar")
    (forward-char)
    (let ((got (go-eldoc--documentation-function))
          (expected "foo[9].bar: int"))
      (should (string= got expected)))))

;;
;; Method
;;

(ert-deftest method-type ()
  "Show global variable with typed declaration"
  (with-go-temp-buffer
    "
package main

type Foo struct {
}

func (f *Foo) bar(a int) int {
        return a + 10
}

func main() {
     foo := Foo{}
     foo.bar()
}
"
    (forward-cursor-on "bar()")
    (let ((got (go-eldoc--documentation-function))
          (expected "foo.bar: func(a int) int"))
      (should (string= got expected)))))

;;
;; package name
;;

(ert-deftest package-name ()
  "Show package name"
  (with-go-temp-buffer
    "
package main

import \"fmt\"

func foo() {
    fmt.Println()
}
"
    (forward-cursor-on "fmt\\.")
    (let ((got (go-eldoc--documentation-function))
          (expected "fmt: package"))
      (should (string= got expected)))))

(ert-deftest case-sensitive-match-type ()
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
    (forward-cursor-on "MkDIR")
    (should-not (go-eldoc--documentation-function))

    (forward-cursor-on "Mkdir")
    (should (go-eldoc--documentation-function))))

;;; not-function.el end here
