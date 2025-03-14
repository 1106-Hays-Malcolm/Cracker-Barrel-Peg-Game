#lang scheme

(define board '((1) (1 1) (1 1 1) (1 1 1 1) (1 1 1 1 1)))

(define board-size 5)

; Displays the board b nicely, does not return anything useful
; n is the number of spaces before the first row
(define (display-board-helper b n)
  (cond
    ((null? b) (newline))
    ((list? (car b)) (begin
                       (display-num-spaces n)
                       (display-board-helper (car b) (sub1 n))
                       (display-board-helper (cdr b) (sub1 n))))
        
    (else (begin
            (display (car b))
            (display " ")
            (display-board-helper (cdr b) n)))))

; Displays the board b nicely, returns nothing useful
(define (display-board b)
  (display-board-helper b (sub1 (length b))))

(define (display-board-list bl)
  (cond
    ((not (null? bl)) (begin (display-board (car bl)) (display-board-list (cdr bl))))))

; Displays n number of spaces, returns nothing useful
(define (display-num-spaces n)
  (if (zero? n)
      0
      (begin (display " ") (display-num-spaces (sub1 n)))))

; Returns the peg at x,y
(define (get-peg b y x)
  (get-nth (get-nth b y) x))

; Returns the nth item in list l
(define (get-nth l n)
  (if (zero? n)
      (car l)
      (get-nth (cdr l) (sub1 n))))

; Applies f to the peg at x,y in board b
(define (apply-to-peg b x y f)
  (apply-to-nth-coord b y x
                (lambda (sublist y x)
                  (apply-to-nth-coord sublist x y f))))

; Tests a condition on the peg at x,y and return t/f
(define (test-peg? b f x y)
  (test-nth? b y
             (lambda (n)
               (test-nth? n x f))))

; Tests the nth element in a list and returns t/f
(define (test-nth? l n f)
  (cond
    ((null? l) #f)
    ((zero? n) (f (car l)))
    (else (test-nth? (cdr l) (sub1 n) f))))

; Applies f to every element in list l
(define (apply-to-list l f)
  (cond
    ((null? l) '())
    (else (cons (f (car l)) (apply-to-list (cdr l) f)))))

; Applies the function f for every element in list l individually
; n is the current index, which is passed into f
; Returns each of the modified lists in a list
; Both l and n are passed into f
(define (apply-for-each-in-list l n f)
  (cond
    ((< n 0) '())
    (else (cons (f l n) (apply-for-each-in-list l (sub1 n) f)))))

; Helper function that applies the function f to every element in the board b
; It returns a list of every version of the board with the function applied to each element
(define (apply-for-each-in-board-helper orgB tempB x y f)
  (cond
    ((null? tempB) '())
    ((list? (car tempB))
     (append
      (apply-for-each-in-board-helper orgB (car tempB) 0 y f)
      (apply-for-each-in-board-helper orgB (cdr tempB) 0 (add1 y) f)))
    (else
     (cons
      (apply-to-peg orgB x y f)
      (apply-for-each-in-board-helper orgB (cdr tempB) (add1 x) y f)))))

; Applies the function f to every element in board b invididually and returns
; the list of boards
(define (apply-for-each-in-board b f)
  (apply-for-each-in-board-helper b b 0 0 f))

; Applies a function to each board in a list of boards
(define (apply-for-each-board bl f)
  (cond
    ((null? bl) '())
    (else (f bl))))

; Applies f to every element in board b and returns the resulting board
(define (apply-to-board b f)
  (cond
    ((null? b) '())
    ((list? (car b)) (cons (apply-to-list (car b) f) (apply-to-board (cdr b) f)))
    (else (cons (f (car b)) (apply-to-board (cdr b) f)))))

; Applies f to the nth element of list l
(define (apply-to-nth l n f)
  (if (zero? n)
      (cons (f (car l)) (cdr l))
      (cons (car l) (apply-to-nth (cdr l) (sub1 n) f))))

; Helper function for apply-to-nth-coord
(define (apply-to-nth-coord-helper l n x y f)
  (if (zero? n)
      (cons (f (car l) x y) (cdr l))
      (cons (car l) (apply-to-nth-coord-helper (cdr l) (sub1 n) x y f))))

; Does same thing as apply-to-nth but passes in two coordinates that the function f can use
(define (apply-to-nth-coord l x y f)
  (apply-to-nth-coord-helper l x x y f))

; Sets the peg at x,y to 0
(define (remove-peg b x y)
  (apply-to-nth b y
                (lambda (n)
                  (apply-to-nth n x (lambda (n) 0)))))

; Sets the peg at x,y to 1
(define (add-peg b x y)
  (apply-to-nth b y
                (lambda (n)
                  (apply-to-nth n x (lambda (n) 1)))))

; Checks if the peg at x,y can be moved
; Returns true if movable, false if immobilized or no peg
(define (peg-movable? b x y)
  (or
   (peg-movable-left? b x y)
   (peg-movable-right? b x y)
   (peg-movable-up-left? b x y)
   (peg-movable-up-right? b x y)
   (peg-movable-down-left? b x y)
   (peg-movable-down-right? b x y)))

; Checks if the peg can be moved left
(define (peg-movable-left? b x y)
  (and
   (peg-in-range? b (- x 2) y)
   (peg-at? b x y)
   (peg-at? b (- x 1) y)
   (not (peg-at? b (- x 2) y))))

; Checks if the peg can be moved right
(define (peg-movable-right? b x y)
  (and
   (peg-in-range? b (+ x 2) y)
   (peg-at? b x y)
   (peg-at? b (+ x 1) y)
   (not (peg-at? b (+ x 2) y))))

; Checks if the peg can be moved up and right
(define (peg-movable-up-right? b x y)
  (and
   (peg-in-range? b x (- y 2))
   (peg-at? b x y)
   (peg-at? b x (- y 1))
   (not (peg-at? b x (- y 2)))))

; Checks if the peg can be moved up and left
(define (peg-movable-up-left? b x y)
  (and
   (peg-in-range? b (- x 2) (- y 2))
   (peg-at? b x y)
   (peg-at? b (- x 1) (- y 1))
   (not (peg-at? b (- x 2) (- y 2)))))

; Checks if the peg can be moved down and left
(define (peg-movable-down-left? b x y)
  (and
   (peg-in-range? b x (+ y 2))
   (peg-at? b x y)
   (peg-at? b x (+ y 1))
   (not (peg-at? b x (+ y 2)))))

; Checks if the peg can be moved down and right
(define (peg-movable-down-right? b x y)
  (and
   (peg-in-range? b (+ x 2) (+ y 2))
   (peg-at? b x y)
   (peg-at? b (+ x 1) (+ y 1))
   (not (peg-at? b (+ x 2) (+ y 2)))))

; Checks if the space at x,y has a peg
(define (peg-at? b x y)
  (test-peg? b
             (lambda (n)
               (equal? n 1))
             x y))

; Checks if the space at x,y is out of range
(define (peg-in-range? b x y)
  (and
   (index-in-range? b y)
   (index-in-range? (get-nth b y) x)
   )
  )

; Checks if the index is in the range of list l
(define (index-in-range? l n)
  (cond
    ((null? l)
     (equal? n -1)
     )
    ((zero? n)
     (not (null? l))
     )
    (else (index-in-range? (cdr l) (sub1 n)))
    )
  )

(define (remove-peg-from-each-position board)
  (apply-for-each-in-board board (lambda (n x y) 0)))

(define (move-right b x y)
  (remove-peg
   (remove-peg
    (add-peg b (+ x 2) y)
    (+ x 1)
    y)
   x
   y))

(define (move-left b x y)
  (remove-peg
   (remove-peg
    (add-peg b (- x 2) y)
    (- x 1)
    y)
   x
   y))

(define (move-up-left b x y)
  (remove-peg
   (remove-peg
    (add-peg b (- x 2) (- y 2))
    (- x 1)
    (- y 1))
   x
   y))

(define (move-up-right b x y)
  (remove-peg
   (remove-peg
    (add-peg b x (- y 2))
    x
    (- y 1))
   x
   y))

(define (move-down-left b x y)
  (remove-peg
   (remove-peg
    (add-peg b x (+ y 2))
    x
    (+ y 1))
   x
   y))

(define (move-down-right b x y)
  (remove-peg
   (remove-peg
    (add-peg b (+ x 2) (+ y 2))
    (+ x 1)
    (+ y 1))
   x
   y))

; Helper for apply-board-for-each-position
(define (apply-board-for-each-position-helper b tempB x y f)
  (cond
    ((null? tempB) '())
    ((list? (car tempB)) (append
                          (apply-board-for-each-position-helper b (car tempB) x y f)
                          (apply-board-for-each-position-helper b (cdr tempB) x (add1 y) f)))
    (else (cons
           (f b x y)
           (apply-board-for-each-position-helper b (cdr tempB) (add1 x) y f)))))

; Applies function f to the entire board for each position in the board.
; Passes the board and the coordinates into f
(define (apply-board-for-each-position b f)
  (apply-board-for-each-position-helper b b 0 0 f))

; Starts the simulation and returns the winning list of boards in order
;(define (play-game board)
;  (let ((starterBoards (remove-peg-from-each-position board)))
;    (apply-board-for-each-position board ())))

(display-board-list (apply-board-for-each-position board (lambda (b x y) (remove-peg b x y))))
;(display-board-list (remove-peg-from-each-position board))