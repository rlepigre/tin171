;; -*- coding: utf-8 -*-
;; Chinese checkers board library
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

(declare (unit board))

(require-extension stack (srfi 1))

(define BOARD-LENGTH (* 17 17))
(define BOARD-START 4)
(define BOARD-END 284)

(define PLAYER-IDS '#(#f #\1 #\2 #\3 #\4 #\5 #\6))

(define INVALID #\#)

(define EMPTY #\space)

(define NEIGHBORS '(+1 -1 -18 -17 +18 +17))

(define FULL-BOARD "####1################11###############111##############1111#########3333     5555#####333      555######33       55#######3        5########         ########6        4#######66       44######666      444#####6666     4444#########2222##############222###############22################2####")


(define OPPOSITES '#(#f 2 1 4 3 6 5))

(define (update-board! board move)
  (let ((from (last move))
        (to (first move)))
    (let ((tmp (string-ref board from)))
      (string-set! board from (string-ref board to))
      (string-set! board to tmp))))

(define (restore-board! board move)
  (let ((from (first move))
        (to (last move)))
    (let ((tmp (string-ref board from)))
      (string-set! board from (string-ref board to))
      (string-set! board to tmp))))

;; True if position n is inside the board.
(define (on-board? n)
  (<= 0 n BOARD-LENGTH))

(define (invalid? n)
  (eqv? (string-ref FULL-BOARD n) INVALID))

;; True if position n is a valid but occupied position.
(define (occupied? board n)
  (let ((c (string-ref board n)))
    (not (or (eqv? c INVALID) (eqv? c EMPTY)))))

;; True if position n is a valid empty position.
(define (empty? board n)
  (eqv? (string-ref board n) EMPTY))

;; True if player-id is allowed to stop at position n.
(define (valid-stop? n player-id)
  (let ((c (string-ref FULL-BOARD n)))
    (or (eqv? c (vector-ref PLAYER-IDS player-id))
        (eqv? c EMPTY)
        (eqv? c (vector-ref PLAYER-IDS (vector-ref OPPOSITES player-id))))))

;; The board positions that are adjacent to n. It is legal to move
;; from n to one of these positions if that positions is unoccupied.
(define (adjacent n)
  (let lp ((l NEIGHBORS) (ret '()))
    (if (null? l)
        ret
        (let* ((x (car l)) (pos (fx+ n x)))
          (if (and (on-board? pos) (not (invalid? pos)))
              (lp (cdr l) (cons pos ret))
              (lp (cdr l) ret))))))

;; The board positions that can be jumped to from n. Returns pairs of
;; (dst . chk). It is legal to move to a dest position if the chk
;; position is occupied by another peg.
(define (jumps n)
  (let lp ((l NEIGHBORS) (ret '()))
    (if (null? l)
        ret
        (let* ((x (car l))
               (chk (fx+ n x))
               (dst (fx+ chk x)))
          (if (and (on-board? chk)
                   (on-board? dst)
                   (not (invalid? dst)))
              (lp (cdr l) (cons (cons dst chk) ret))
              (lp (cdr l) ret))))))

;; Generates all moves for a given peg that starts at position
;; 'start'. The positions in the moves are in reverse order.
(define (push-available-moves! stk board start player)
  (define (do-jumps start jumped)
    (let ((path (cons start jumped)))
      (for-each (lambda (jump)
                  (let ((dst (car jump))
                        (chk (cdr jump)))
                    (when (and (valid-stop? dst player)
                               (empty? board dst)
                               (occupied? board chk)
                               (not (memq dst jumped)))
                      (stack-push! stk (cons dst path))
                      (do-jumps dst path))))
                (jumps start))))
  ;; First try to move without jumping
  (for-each (lambda (i)
              (when (and (empty? board i)
                         (valid-stop? i player))
                (stack-push! stk (list i start))))
            (adjacent start))
  ;; Now try jumping
  (do-jumps start '()))

;; All legal moves for all the player's pegs.
(define (all-moves board player)
  (do ((c (vector-ref PLAYER-IDS player))
       (ret (make-stack))
       (i BOARD-START (fx+ i 1)))
      ((fx> i BOARD-END)
       ret)
    (when (eqv? (string-ref board i) c)
      (push-available-moves! ret board i player))))

(define (print-board board)
  (do ((i 0 (+ i 1)))
      ((= i BOARD-LENGTH)
       (display (make-string (/ i 17) #\.))
       (newline))
    (cond ((zero? (modulo i 17))
           (display (make-string (/ i 17) #\.))
           (newline)
           (display (make-string (- 17 (/ i 17)) #\.))))
    (display #\space)
    (display (string-ref board i))))
