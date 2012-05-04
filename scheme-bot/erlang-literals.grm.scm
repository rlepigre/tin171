;; -*- coding: utf-8 -*-
;; Very limited parser for Erlang literals.
;; Copyright (C) 2012 Göran Weinholt

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require-extension lalr)
(define erlang-parser
  (lalr-parser
   ;; options
   (output: erlang-parser "erlang-literals.yy.scm")
   ;; tokens
   (atom string int openb closeb openc closec comma dot)
   ;; rules
   (exp  (atom)             : (string->symbol $1)
         (string)           : $1
         (int)              : (string->number $1)
         (exp dot)          : $1
         (openb list)       : $2
         (openc tuple)      : (list->vector $2))
   ;; lists
   (list (exp closeb)       : (list $1)
         (exp comma list)   : (cons $1 $3)
         (closeb)           : '())
   ;; tuples
   (tuple (exp closec)      : (list $1)
          (exp comma tuple) : (cons $1 $3)
          (closec)          : '())))


