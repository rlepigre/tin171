;; -*- coding: utf-8 -*-
;; Chinese checkers bot
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

(declare (uses protocol)
         (uses board))

(require-extension tcp getopt-long matchable stack (srfi 18))

(use extras data-structures)

(define (random-bot c timeout board player)
  (let ((move (car (shuffle (stack->list (all-moves board player))
                             random))))
    (client-move c move)))

(define (trivial-bot c timeout board player)
  (let* ((moves (shuffle
                 (map (lambda (move)
                        (with-move! board move
                                    (lambda ()
                                      (vector (board-eval-static-distance board player)
                                              move))))
                      (stack->list
                       (all-moves board player)))
                 random))
         (moves
          (sort! moves (lambda (x y)
                         (< (vector-ref x 0) (vector-ref y 0))))))
    (print "best: " (car moves))
    (client-move c (vector-ref (car moves) 1))))

(define (iddfs-bot c timeout board player)
  (define best-move '())
  (define best-score (- (expt 2 23)))
  (define best-depth (expt 2 23))
  (define stop-time (+ (time->seconds (current-time)) (/ timeout 1000)))
  (define (board-eval board player)
    (fx- 0 (board-eval-static-distance board player)))

  (define (recursive-dls first-move limit depth)
    (let ((score (board-eval board player)))
      (when (or (> score best-score)
                (and (= score best-score)
                     (fx< depth best-depth)))
        (print "The move " (list first-move score depth)
               " is better than " (list best-move best-score best-depth))
        (set! best-move first-move)
        (set! best-score score)
        (set! best-depth depth))
      (when (not (or (fx<= limit 0)
                     (> (time->seconds (current-time)) stop-time)))
        (stack-for-each (all-moves board player)
                        (lambda (move)
                          (with-move! board move
                                      (lambda ()
                                        (recursive-dls first-move (fx- limit 1)
                                                       (fx+ depth 1)))))))))

  (let lp ((depth 1))
    (when (or (null? best-move)
              (< (time->seconds (current-time)) stop-time))
      (stack-for-each (all-moves board player)
                      (lambda (first-move)
                        (print ";; Considering move " (reverse first-move)
                               " at depth limit " depth)
                        (with-move! board first-move
                                    (lambda ()
                                      (recursive-dls first-move depth 0)))))
      (lp (fx+ depth 1))))
  (print "best move: " best-move)
  (client-move c best-move))

(define (play c player make-move)
  (let lp ()
    (match (read-literal/no-error c)
      (#('your_turn timeout board)
       (make-move c timeout board player)
       (lp))
      (#('won who board)
       (print "A winner was announced: " who))
      (#('update who move board)
       (print-board board)
       (lp))
      (else
       (lp)))))

(define (main args)
  (define grammar
    `((server (single-char #\s)
              (value #t))
      (port (single-char #\p)
            (value #t))
      (nick (single-char #\n)
            (value #t))))

  (define (permute prefix)
    (string-append prefix (number->string (random 1000))))

  (let-values (((args opts) (getopt-long args grammar)))
    (define (option name default)
      (cond ((assq 'nick opts) => cdr)
            (else default)))

    (let ((nick (option 'nick (permute "SBot-")))
          (server (option 'server "localhost"))
          (port (string->number (option 'port "8000"))))
      (print "I am " nick)
      (let-values (((i o) (tcp-connect server port)))
        (let ((c (make-game-client i o)))
          (client-login c nick)
          (let-values (((new run) (client-list-games c)))
            (cond ((null? new)
                   (error 'bot "TODO: host games"))
                  (else
                   (print "Joining game " (car new))
                   (client-join-game c (car new))))
            (let lp ()
              (match (read-literal/no-error c)
                (#('game_start player players board)
                 (print "The game starts.")
                 (play c player iddfs-bot))
                (else
                 (lp))))))))))

(main (command-line-arguments))
