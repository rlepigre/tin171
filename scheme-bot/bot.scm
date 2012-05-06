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

(require-extension tcp getopt-long matchable stack (srfi 18)
                   (srfi 1))

(use extras data-structures)

;;; Playing the game

(define (random-bot timeout board player)
  (let ((move (car (shuffle (stack->list (all-moves board player))
                            random))))
    move))

(define (trivial-bot timeout board player)
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
    ;; (print "best: " (car moves))
    (vector-ref (car moves) 1)))

(define (iddfs-bot timeout board player)
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
  best-move)

(define (play c player make-move)
  (let lp ()
    (match (read-literal/no-error c)
      (#('your_turn timeout board)
       (client-move c (make-move timeout board player))
       (lp))
      (#('won #(id nick) board)
       (print "A winner was announced: " nick)
       (eqv? id player))
      (#('update who move board)
       (print-board board)
       (lp))
      (else
       (lp)))))

;;; Genetic algorithm

;; Simulate a game between two players.
(define (simulation p1 p2)
  (define timeout 5000)
  (define eval board-eval-static-distance) ;XXX: used in the losing phase
  (let ((board (string-copy "####1################11###############111##############1111#########             #####            ######           #######          ########         ########          #######           ######            #####             #########2222##############222###############22################2####")))
    (let lp ((id* (circular-list 1 2))
             (p* (circular-list p1 p2))
             (plies 0))
      ;; (print-board board)
      (cond ((> plies 150)
             ;; This isn't going anywhere, so restart.
             (simulation p1 p2))
            ((winning-state? board 1)
             ;; (print-board board)
             (values 1 plies (- (eval board 2))))
            ((winning-state? board 2)
             ;; (when (zero? (random 50))
             ;;   (print-board board))
             (values 2 plies (- (eval board 1))))
            (else
             (let ((move ((car p*) timeout (string-copy board) (car id*))))
               (update-board! board move)
               (lp (cdr id*) (cdr p*) (+ plies 1))))))))

(define (vector->trivial-bot vec)
  (define (bot timeout board player)
    (let* ((moves (shuffle
                   (map (lambda (move)
                          (with-move! board move
                                      (lambda ()
                                        (vector (static-distance board player vec)
                                                move))))
                        (stack->list
                         (all-moves board player)))
                   random))
           (moves (sort! moves (lambda (x y)
                                 (< (vector-ref x 0) (vector-ref y 0))))))
      ;; (print "Best move: " (car moves))
      (vector-ref (car moves) 1)))
  bot)

;; Takes a static distance vector for player 1 and plays it against
;; the trivial bot.
(define (fitness vec stats)
  (define bot (vector->trivial-bot vec))
  (define PHASE1-RUNS 2)
  (define PHASE2+-RUNS 5)
  (let lp ((i 0) (score 0))
    (cond ((and (= i PHASE1-RUNS) (< score 300))
           (/ score PHASE1-RUNS))
          ((= i PHASE2+-RUNS)
           (/ score PHASE2+-RUNS))
          (else
           (let-values (((winner plies loser-eval) (simulation bot trivial-bot)))
             (cond ((= winner 1)
                    (stats 'win)
                    (lp (+ i 1) (+ score (- 500 plies))))
                   (else
                    (stats 'loss)
                    (lp (+ i 1) (+ score loser-eval)))))))))

(define (random-vec)
  (do ((ret (make-vector BOARD-LENGTH -1))
       (i 0 (+ i 1)))
      ((= i BOARD-LENGTH)
       (for-each (lambda (x)
                   (vector-set! ret x 0))
                 (goal-positions 1))
       (for-each (lambda (x)
                   (if (< x 23)
                       (vector-set! ret x 500)
                       (vector-set! ret x 200)))
                 (goal-positions 2))
       ret)
    (vector-set! ret i (random 200))))

(define (random-population n)
  (do ((ret (make-vector n #f))
       (i 0 (+ i 1)))
      ((= i n) ret)
    (vector-set! ret i (random-vec))))

;; From the course book, with modifications.
(define (genetic-algorithm population fitness-fn)
  (define (reproduce x y)
    (do ((ret (make-vector (vector-length x)))
         (i 0 (+ i 1)))
        ((= i (vector-length ret))
         ret)
      (if (zero? (random 2))
          (vector-set! ret i (vector-ref x i))
          (vector-set! ret i (vector-ref y i)))))
  ;; (define (reproduce x y)
  ;;   (do ((cut-off (random (vector-length x)))
  ;;        (ret (make-vector (vector-length x)))
  ;;        (i 0 (+ i 1)))
  ;;       ((= i (vector-length ret))
  ;;        ret)
  ;;     (if (< i cut-off)
  ;;         (vector-set! ret i (vector-ref x i))
  ;;         (vector-set! ret i (vector-ref y i)))))
  (define (mutate! x)
    (let ((goal (goal-positions 1)))
      (do ((i 0 (+ i 1)))
          ((= i (vector-length x))
           (for-each (lambda (i)
                       (vector-set! x i (min (vector-ref x i) 5)))
                     (goal-positions 1)))
        (when (zero? (random 10))
          (let ((v (+ (- (random 50) 25)
                      (vector-ref x i))))
            ;; Mutate the weight for this position, but it is kept
            ;; positive if it's not in the goal.
            (when (or (positive? v) (memv i goal))
              (vector-set! x i v)))))))
  (define (most-fit pop generation)
    (define wins 0)
    (define losses 0)
    (define (stats outcome)
      (case outcome
        ((win) (set! wins (+ wins 1)))
        ((loss) (set! losses (+ losses 1)))))
    (let* ((n (inexact->exact (floor (* 1/5 (vector-length pop)))))
           (survivors
            (list->vector
             (take (sort (map (lambda (vec) (cons (fitness-fn vec stats) vec))
                              (vector->list pop))
                         (lambda (x y) (> (car x) (car y))))
                   n))))
      (print "Generation " generation ":"
             " Out of " (+ wins losses)
             " games the population won " wins " and lost " losses " times. "
             "That is " (* 100.0 (/ wins (+ wins losses))) "% wins. "
             "Strongest survivor: " (vector-ref survivors 0))
      (flush-output)
      survivors))
  (let lp ((pop population) (iterations 0))
    (let ((survivors (most-fit pop iterations)))
      (do ((new-pop (make-vector (vector-length pop)))
           (i 0 (+ i 1)))
          ((= i (vector-length pop))
           (lp new-pop (+ iterations 1)))
        (let ((x (cdr (vector-ref survivors (random (vector-length survivors)))))
              (y (cdr (vector-ref survivors (random (vector-length survivors))))))
          (let ((child (reproduce x y)))
            (when (zero? (random 15))
              (mutate! child))
            (vector-set! new-pop i child)))))))

(define (measure-1 vec)
  (define wins 0)
  (define losses 0)
  (define (stats outcome)
    (case outcome
      ((win) (set! wins (+ wins 1)))
      ((loss) (set! losses (+ losses 1)))))
  (do ((i 0 (+ i 1)))
      ((= i 100)
       (* 100.0 (/ wins (+ wins losses))))
    (fitness vec stats)))

;;; Program startup

(define (main args)
  (define grammar
    `((server (single-char #\s)
              (value #t))
      (port (single-char #\p)
            (value #t))
      (nick (single-char #\n)
            (value #t))
      (genetic (value #t))))

  (define (permute prefix)
    (string-append prefix (number->string (random 1000))))

  (let-values (((args opts) (getopt-long args grammar)))
    (define (option name default)
      (cond ((assq 'nick opts) => cdr)
            (else default)))
    (cond ((assq 'genetic args) =>
           (lambda (opt)
             (print "Running the genetic algorithm...")
             (genetic-algorithm (random-population
                                 (string->number (cdr opt)))
                                fitness))))
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
