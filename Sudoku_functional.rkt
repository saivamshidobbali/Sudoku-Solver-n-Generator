#lang racket

(define board_3_3
 (list
 (list 5 3 0 0 7 0 0 0 0)
 (list 6 0 0 1 9 5 0 0 0)
 (list 0 9 8 0 0 0 0 6 0)
 (list 8 0 0 0 6 0 0 0 3)
 (list 4 0 0 8 0 3 0 0 1)
 (list 7 0 0 0 2 0 0 0 6)
 (list 0 6 0 0 0 0 2 8 0)
 (list 0 0 0 4 1 9 0 0 5)
 (list 0 0 0 0 8 0 0 7 9)))

(define empty_board
 (list
 (list 0 0 0 0 0 0 0 0 0)
 (list 0 0 0 0 0 0 0 0 0)
 (list 0 0 0 0 0 0 0 0 0)
 (list 0 0 0 0 0 0 0 0 0)
 (list 0 0 0 0 0 0 0 0 0)
 (list 0 0 0 0 0 0 0 0 0)
 (list 0 0 0 0 0 0 0 0 0)
 (list 0 0 0 0 0 0 0 0 0)
 (list 0 0 0 0 0 0 0 0 0)))

(define board_3_2
 (list
 (list 6 0 3 0 4 1)
 (list 0 0 0 0 0 5)
 (list 0 0 0 0 2 0)
 (list 0 4 0 0 0 0)
 (list 5 0 0 0 0 0)
 (list 3 1 0 5 0 4)))


(define board_4_4
(list
(list 0 2 3 0 0 6 0 0 9 0 11 0 0 0 0 0)
(list 5 6 0 8 1 0 3 0 0 14 0 0 0 10 0 12)
(list 0 10 11 12 13 14 0 16 1 0 0 4 5 6 0 8)
(list 0 14 0 16 0 0 11 0 5 6 7 0 0 0 0 4)
(list 2 1 0 3 6 0 8 7 10 0 12 11 14 0 16 0)
(list 6 5 8 0 2 0 4 3 14 0 16 0 10 9 12 11)
(list 0 9 12 0 0 13 0 15 2 0 4 3 6 0 8 7)
(list 0 0 0 15 10 0 0 11 6 5 0 7 0 0 4 3)
(list 0 4 0 0 0 0 5 0 0 0 0 10 15 0 13 0)
(list 0 8 0 6 3 4 1 2 0 0 13 14 0 0 9 10)
(list 0 12 9 0 0 0 13 0 0 0 0 2 0 0 5 0)
(list 15 0 13 14 11 12 9 0 0 8 0 0 0 4 0 0)
(list 4 0 0 0 0 7 6 5 0 11 0 0 16 15 14 13)
(list 8 7 6 0 0 0 2 1 16 0 0 0 0 0 10 0)
(list 0 0 10 0 16 15 14 0 4 3 0 0 0 0 6 0)
(list 16 15 0 0 0 0 10 9 8 7 6 5 0 0 0 1)))

;##################################################################################################################################################

(define (get-col board row col m n)
(cond
 [(equal? row (* m n)) empty]
 [else (cons (list-ref (list-ref board row) col) (get-col board (+ row 1) col m n))]))

(define (get-row board row col m n)
 (cond
 [(equal? col (* m n)) empty]
 [else (cons (list-ref (list-ref board row) col) (get-row board row (+ col 1) m n))]))

(define (get-box board row col target_row target_col m n)
(cond
 [(equal? row target_row) empty]
 [(equal? col target_col) (get-box board (+ 1 row) (- col  n) target_row target_col m n)] 
 [else (cons (list-ref (list-ref board row) col) (get-box board row (+ col 1) target_row target_col m n))]))

(define (create-list total)
(cond
 [(zero? total) empty]
 [else (cons total (create-list (- total 1)))]))

(define (set-list board row col val)
(let ([l (list-set (list-ref board row) col val)])
   (list-set board row l)))
  
(define (solve board row col m n) 
(cond
[(equal? row (* m n)) board]    
[(equal? col (* m n)) (solve board (+ row 1) 0 m n)]
[(equal? (list-ref (list-ref board row) col) 0) (let ([t_r (- row (remainder row m))]
                                                      [t_c (- col (remainder col n))])
                                                  (ormap (lambda (val) (solve (set-list board row col val) row (+ col 1) m n))
                                                         (remove* (get-box board t_r  t_c (min (+ t_r m) (* m n)) (min (+ t_c n) (* m n)) m n)
                                                         (remove* (get-row board row 0 m n)(remove* (get-col board 0 col m n) (create-list (* m n)))))))]
[else (solve board row (+ col 1) m n)]))

;#########################################################################################################################################################
(define (display-board board total)
 (for ([i (in-range total)])
   (display (list-ref board i))
   (display "\n"))
  
 (display "\n"))

(display "3x3 board\n")
(display-board board_3_3 9)
(display "Solved 3x3 Board\n")
(display-board (solve board_3_3 0 0 3 3) 9)

(display "3x2 board\n")
(display-board board_3_2 6)
(display "Solved 3x2 Board\n")
(display-board (solve board_3_2 0 0 2 3) 6)

(display "3x3 empty board\n")
(display-board empty_board 9)
(display "Solved 3x3 empty board\n")
(display-board (solve empty_board 0 0 3 3) 9)


(display "4x4 board\n")
(display-board board_4_4 16)
(display "Solved 4x4 Board\n")
(display-board (solve board_4_4 0 0 4 4) 16)

(define (remove-cells board m n cells_removed)
(let* ([row (random (* n m))]
         [col (random (* n m))])
    (if (> cells_removed (* 0.95 (* (* n m) (* n m))))
         board
        (remove-cells (set-list board row col 0) n m (+ cells_removed 1)))))

(define (generate m n)
 (display-board (remove-cells (solve (build-list (* m n) (lambda (x) (build-list (* m n) (lambda (x) 0)))) 0 0 m n) m n 0) (* m n)))

(display "generated 3x3 board\n")
(generate 3 3)

(display "generated 4x4 board\n")
(generate 4 4)
