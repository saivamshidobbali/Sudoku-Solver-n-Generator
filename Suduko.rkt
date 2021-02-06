#lang racket

(define M 3)
(define N 3)
(define total 9)

(define board_3_2
 (vector
 (vector 6 0 3 0 4 1)
 (vector 0 0 0 0 0 5)
 (vector 0 0 0 0 2 0)
 (vector 0 4 0 0 0 0)
 (vector 5 0 0 0 0 0)
 (vector 3 1 0 5 0 4)))

(define board_3_3
 (vector
 (vector 5 3 0 0 7 0 0 0 0)
 (vector 6 0 0 1 9 5 0 0 0)
 (vector 0 9 8 0 0 0 0 6 0)
 (vector 8 0 0 0 6 0 0 0 3)
 (vector 4 0 0 8 0 3 0 0 1)
 (vector 7 0 0 0 2 0 0 0 6)
 (vector 0 6 0 0 0 0 2 8 0)
 (vector 0 0 0 4 1 9 0 0 5)
 (vector 0 0 0 0 8 0 0 7 9)))

(define board_4_4
(vector
(vector 0 2 3 0 0 6 0 0 9 0 11 0 0 0 0 0)
(vector 5 6 0 8 1 0 3 0 0 14 0 0 0 10 0 12)
(vector 0 10 11 12 13 14 0 16 1 0 0 4 5 6 0 8)
(vector 0 14 0 16 0 0 11 0 5 6 7 0 0 0 0 4)
(vector 2 1 0 3 6 0 8 7 10 0 12 11 14 0 16 0)
(vector 6 5 8 0 2 0 4 3 14 0 16 0 10 9 12 11)
(vector 0 9 12 0 0 13 0 15 2 0 4 3 6 0 8 7)
(vector 0 0 0 15 10 0 0 11 6 5 0 7 0 0 4 3)
(vector 0 4 0 0 0 0 5 0 0 0 0 10 15 0 13 0)
(vector 0 8 0 6 3 4 1 2 0 0 13 14 0 0 9 10)
(vector 0 12 9 0 0 0 13 0 0 0 0 2 0 0 5 0)
(vector 15 0 13 14 11 12 9 0 0 8 0 0 0 4 0 0)
(vector 4 0 0 0 0 7 6 5 0 11 0 0 16 15 14 13)
(vector 8 7 6 0 0 0 2 1 16 0 0 0 0 0 10 0)
(vector 0 0 10 0 16 15 14 0 4 3 0 0 0 0 6 0)
(vector 16 15 0 0 0 0 10 9 8 7 6 5 0 0 0 1)))

(define sudukoSolved #f)


(define empty_3_3 (vector
                     (vector 0 0 0 0 0 0 0 0 0)
                     (vector 0 0 0 0 0 0 0 0 0)
                     (vector 0 0 0 0 0 0 0 0 0)
                     (vector 0 0 0 0 0 0 0 0 0)
                     (vector 0 0 0 0 0 0 0 0 0)
                     (vector 0 0 0 0 0 0 0 0 0)
                     (vector 0 0 0 0 0 0 0 0 0)
                     (vector 0 0 0 0 0 0 0 0 0)
                     (vector 0 0 0 0 0 0 0 0 0)))

;########################################################################################

(define (placeNumber d row col board rows columns boxes)

  (define idx  (+ (* (floor (/ row N)) N) (floor (/ col M))))
  
  (define row-vector (vector-ref rows row))
  (define col-vector (vector-ref columns col))
  (define box-vector (vector-ref boxes idx))

  (vector-set! row-vector d (+ (vector-ref row-vector d) 1))
  (vector-set! col-vector d (+ (vector-ref col-vector d) 1))
  (vector-set! box-vector d (+ (vector-ref box-vector d) 1))

  (define r (vector-ref board row))
  (vector-set! r col d))

(define (placeNextNumber row col board rows columns boxes)
  (if (and (equal? row (- total 1)) (equal? col (- total 1)))
      (set! sudukoSolved #t)
      (if (equal? col (- total 1)) (backtrack (+ row 1) 0 board rows columns boxes)
                                   (backtrack row (+ col 1) board rows columns boxes))))

(define (couldPlace d row col board rows columns boxes)
  (define idx  (+ (* (floor (/ row N)) N) (floor (/ col M)))) 
  (define row-vector (vector-ref rows row))
  (define col-vector (vector-ref columns col))
  (define box-vector (vector-ref boxes idx))

  (if (zero? (+ (vector-ref row-vector d) (+ (vector-ref col-vector d)
                                             (vector-ref box-vector d)))) #t #f))

(define (removeNumber d row col board rows columns boxes)
  (define idx  (+ (* (floor (/ row N)) N) (floor (/ col M))))
  
  (define row-vector (vector-ref rows row))
  (define col-vector (vector-ref columns col))
  (define box-vector (vector-ref boxes idx))

  (vector-set! row-vector d (- (vector-ref row-vector d) 1))
  (vector-set! col-vector d (- (vector-ref col-vector d) 1))
  (vector-set! box-vector d (- (vector-ref box-vector d) 1))

  (define r (vector-ref board row))
  (vector-set! r col 0))

(define (helper d row col board rows columns boxes) 
  (placeNumber d row col board rows columns boxes)
  (placeNextNumber row col board rows columns boxes)
  (if (equal? sudukoSolved #f)
      (removeNumber d row col board rows columns boxes)
      100))

(define (backtrack row col board rows columns boxes)
  (if (zero?  (vector-ref (vector-ref board row) col))
      (for ([i (in-range 1 (+ total 1) 1)])
        (if (equal? (couldPlace i row col board rows columns boxes) #t)
            (helper i row col board rows columns boxes) 
            2))
      (placeNextNumber row col board rows columns boxes)))

(define (sudukoSolver board rows columns boxes)
  (for* ([i (in-range total)] [j (in-range total)])
    (let ([elem (vector-ref (vector-ref board i) j)]) 
      (if (positive? elem) (placeNumber elem i j board rows columns boxes) 2)))
  (backtrack 0 0 board rows columns boxes))

;###############################################################################################
(define (display-board board total)
 (for ([i (in-range total)])
   (display (vector-ref board i))
   (display "\n"))
  
 (display "\n"))

(define (test board m n)
;# Init  
(set! sudukoSolved #f)
(set! M m)
(set! N n)
(set! total (* m n))

(define rows (make-vector (* m n) (make-vector (+ (* m n) 1) 0)))
(define columns (make-vector (* m n) (make-vector (+ (* m n) 1) 0)))
(define boxes (make-vector (* m n) (make-vector (+ (* m n) 1) 0)))
(define empty (make-vector (* m n) (make-vector (* m n) 0)))
  
(for ([i (in-range (* m n))])
    (define row (make-vector (+ (* m n) 1) 0))
    (define column (make-vector (+ (* n m) 1) 0))
    (define box (make-vector (+ (* n m) 1) 0))
    (define empty_row (make-vector (* n m) 0))
           
    (vector-set! rows i row)
    (vector-set! columns i column)
    (vector-set! boxes i box)
    (vector-set! empty i empty_row))
  
(sudukoSolver board rows columns boxes)
(display-board board total))

(define empty-cells 0)

(define (remove-cells board n m)
  (let* ([row (random (* n m))]
         [col (random (* n m))])
    (vector-set! (vector-ref board row) col 0)
    (set! empty-cells (+ empty-cells 1))
    (if (> empty-cells (* 0.95 (* (* n m) (* n m))))
        board
        (remove-cells board n m))))

(define (generate m n)
         (set! sudukoSolved #f)
         (set! M m)
         (set! N n)
         (set! total (* m n))         

         (define rows (make-vector (* m n) (make-vector (+ (* m n) 1) 0)))
         (define columns (make-vector (* m n) (make-vector (+ (* m n) 1) 0)))
         (define boxes (make-vector (* m n) (make-vector (+ (* m n) 1) 0)))
         (define empty (make-vector (* m n) (make-vector (* m n) 0)))
  
         (for ([i (in-range (* m n))])
            (define row (make-vector (+ (* m n) 1) 0))
            (define column (make-vector (+ (* n m) 1) 0))
            (define box (make-vector (+ (* n m) 1) 0))
            (define empty_row (make-vector (* n m) 0))
           
            (vector-set! rows i row)
            (vector-set! columns i column)
            (vector-set! boxes i box)
            (vector-set! empty i empty_row))

         (sudukoSolver empty rows columns boxes)

         (remove-cells empty n m) 
         (for ([i (in-range total)])
            (display (vector-ref empty i))
            (display "\n"))
  
         (display "\n"))

(display "3x3 board\n")
(display-board board_3_3 9)
(display "Solved 3x3 Board\n")
(test board_3_3 3 3)

(display "3x2 board\n")
(display-board board_3_2 6)
(display "Solved 3x2 Board\n")
(test board_3_2 3 2)

(display "4x4 board\n")
(display-board board_4_4 16)
(display "Solved 4x4 Board\n")
(test board_4_4 4 4)

(display "Generated 3x3 Board\n")
(generate 3 3)
(display "Generated 4x4 Board\n")
(generate 4 4)