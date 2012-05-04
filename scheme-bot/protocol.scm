;; -*- coding: utf-8 -*-
;; Server protocol parser for the Chinese checkers bot
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

(declare (unit protocol))

(require-extension irregex matchable)
(use (srfi 13))

(include "erlang-literals.yy.scm")

(define-record client i o)

(define (make-game-client i o)
  (make-client i o))

(define (fmt obj)
  (cond ((vector? obj)
         (string-append "{" (string-join (map fmt (vector->list obj))
                                         ",")
                        "}"))
        ((integer? obj)
         (number->string obj))
        ((string? obj)
         (fmt (map char->integer (string->list obj))))
        ((symbol? obj)
         (string-append "'"
                        (irregex-replace/all "\\\\"
                                             (symbol->string obj)
                                             "\\\\")
                        "'"))
        ((list? obj)
         (string-append "[" (string-join (map fmt obj) ",") "]"))
        (else
         (error 'fmt "Unrepresentable object" obj))))

(define (parse str errorp)
  (include "erlang-lexer.scm")
  (lexer-init 'string str)
  (erlang-parser lexer errorp))

;;; Sending literals

(define (send-literal c msg)
  (display "<= ") (write msg) (newline)
  (display (fmt msg) (client-o c))
  (display ".\n" (client-o c))
  (flush-output (client-o c)))

(define (send-literal/ok c msg)
  (send-literal c msg)
  (match (read-literal c)
    ('ok 'ok)
    (x
     (error 'send-literal/ok "Expected an ok from the server" x))))

;;; Reading litreals

(define (read-literal c)
  (define (errorp msg . args)
    (error 'get msg args))
  (let lp ((lines '()))
    (let ((line (read-line (client-i c))))
      (cond ((eof-object? line)
             (print "=> (Connection closed by server)")
             (eof-object))
            ((irregex-search ",$" line)
             ;; A terrible hack, really.
             (lp (cons line lines)))
            (else
             (let ((msg (parse (string-concatenate (reverse (cons line lines)))
                               errorp)))
               (display "=> ") (write msg) (newline)
               msg))))))

(define (read-literal/no-error c)
  (match (read-literal c)
    (#('error msg)
     (error 'read-literal/no-error "The server sent an error" msg))
    (x x)))

;; Sometimes the server will send a string as a list of integers.
(define (literal->string x)
  (cond ((string? x)
         x)
        ((list? x)
         (list->string (map integer->char x)))
        (else
         (error 'literal->string "Can't convert to string" x))))

;;; High-level protocol

(define (client-login c nickname)
  (send-literal/ok c `#(login ,nickname)))

(define (client-list-games c)
  (send-literal c 'list_games)
  (match (read-literal/no-error c)
    (#('games new running)
     (values (map literal->string new)
             (map literal->string running)))
    (x
     (error 'client-list-games "Bad result from server" x))))

(define (client-host-game c name)
  (send-literal/ok c `#(host_game ,name)))

(define (client-join-game c name)
  (send-literal/ok c `#(join_game ,name)))

(define (client-spectate c name)
  (send-literal/ok c `#(spectate ,name)))

(define (client-start-game c name)
  (send-literal/ok c `#(start_game ,name)))

(define (client-leave c)
  (send-literal c 'leave))

(define (client-move c positions)
  (send-literal/ok c `#(move ,positions)))
