(uiop:define-package :aoc-2021/day-4
  (:use :aoc-2021/prelude))
(cl:in-package :aoc-2021/day-4)

(coalton-toplevel
  (declare parse-int! (String -> Integer))
  (define parse-int! (compose (expect "malformed integer") parse-int))

  (declare read-line! (InputFile -> String))
  (define read-line! (compose (expect "unexpected eof") read-line))

  (declare skip-empties ((Iterator String) -> (Iterator String)))
  (define skip-empties (filter (compose not string-empty?)))
  
  ;; i'll repr a bingo board as a 5x5 matrix of (Optional Integer), with None representing already-marked
  (declare read-calls (InputFile -> (Vector Integer)))
  (define (read-calls f)
    (progn
      (let line = (read-line! f))
      (let calls = (collect-to-vector (map parse-int!
                                           (skip-empties
                                                   (split-string #\, line)))))
      (read-line f) ; discard an empty line
      calls))

  (declare read-board-row (InputFile -> (Matrix (Optional Integer)) -> Integer -> (Matrix (Optional Integer))))
  (define (read-board-row f board rownum)
    (progn
      (let line = (read-line! f))
      (let elts = (map parse-int
                       (skip-empties
                        (split-string #\space line))))
      (for-each (fn (col)
                           (matrix-write!-unsafe board
                                                 (expect "not enough integers for matrix row"
                                                         (next elts))
                                                 rownum col))
                (upto 5))
      board))

  (declare empty-bingo-board (Unit -> (Matrix (Optional Integer))))
  (define (empty-bingo-board _)
    (make-matrix 5 5 None))

  (declare read-bingo-board (InputFile -> (Matrix (Optional Integer))))
  (define (read-bingo-board f)
    (progn
      (let board = (reduce (read-board-row f)
                           (empty-bingo-board)
                           (upto 5)))
      (read-line f) ; discard an empty line
      board))

  (declare parse-file (InputFile -> (Tuple (Vector Integer) (Vector (Matrix (Optional Integer))))))
  (define (parse-file f)
    (progn
      (let calls = (read-calls f))
      (let boards = (make-vector))
      (let read-board-loop =
        (fn (u)
          (if (eof? f)
              (Tuple calls boards)
              (progn (vector-push (read-bingo-board f) boards)
                     (read-board-loop u)))))
      (read-board-loop Unit)))

  (define (mark-cell target row board col)
    (match (matrix-index-unsafe board row col)
      ((Some n) (if (== n target)
                    (matrix-write!-unsafe board None row col)
                    board))
      (_ board)))

  (declare mark-in-row (Integer -> (Matrix (Optional Integer)) -> Integer -> (Matrix (Optional Integer))))
  (define (mark-in-row target board rownum)
    (reduce (mark-cell target rownum)
            board
            (upto 5)))

  (declare mark-in-board (Integer -> (Matrix (Optional Integer)) -> (Matrix (Optional Integer))))
  (define (mark-in-board num board)
    (reduce (mark-in-row num)
            board
            (upto 5)))

  (define (row-bingo? board row)
    (every? isNone (matrix-row-iterator board row)))
  (define (col-bingo? board col)
    (every? isNone (matrix-col-iterator board col)))
  (define (bingo? board)
    (or (any? id (map (row-bingo? board) (upto 5)))
        (any? id (map (col-bingo? board) (upto 5)))))

  (declare mark-board-and-test-bingos ((EscapeContinuation (Tuple (Matrix (Optional Integer)) Integer))
                                       -> Integer
                                       -> (Matrix (Optional Integer))
                                       -> Unit))
  (define (mark-board-and-test-bingos escape call board)
    (if (bingo? (mark-in-board call board))
        (progn 
          (throw escape (Tuple board call)))
        Unit))
  
  (declare mark-boards ((EscapeContinuation (Tuple (Matrix (Optional Integer)) Integer))
                        -> (Vector (Matrix (Optional Integer)))
                        -> Integer
                        -> Unit))
  (define (mark-boards escape boards call)
    (for-each (mark-board-and-test-bingos escape call)
              (vector-iterator boards)))

  (declare find-bingo ((Vector Integer) -> (Vector (Matrix (Optional Integer))) -> (Tuple (Matrix (Optional Integer)) Integer)))
  (define (find-bingo calls boards)
    (let/ec escape
      (progn 
        (for-each (mark-boards escape boards)
                  (vector-iterator calls))
        (error "no bingo after all calls"))))

  (declare bingo-score ((Matrix (Optional Integer)) -> Integer))
  (define (bingo-score winning-board)
    (iterator-sum
     (map (compose iterator-sum unwrapped)
          (map (matrix-row-iterator winning-board)
               (upto 5)))))

  (declare part1 (String -> Integer))
  (define (part1 path)
    (match (expect "failed to open file"
                   (with-input-file (f path) (parse-file f)))
      ((Tuple calls boards)
       (match (find-bingo calls boards)
         ((Tuple winning-board last-call)
          (* (bingo-score winning-board) last-call)))))))

(coalton-toplevel
  (declare mark-board-and-remove-if-bingo (Integer
                                           -> (Matrix (Optional Integer))
                                           -> (Optional (Matrix (Optional Integer)))))
  (define (mark-board-and-remove-if-bingo call board)
    (if (bingo? (mark-in-board call board))
        None
        (Some board)))

  (declare mark-boards-remove-bingos (Integer -> (Vector (Matrix (Optional Integer))) -> (Vector (Matrix (Optional Integer)))))
  (define (mark-boards-remove-bingos call boards)
    (collect-to-vector (unwrapped (map (mark-board-and-remove-if-bingo call)
                                       (vector-iterator boards)))))

  (declare find-last-call-to-win ((Iterator Integer) -> (Matrix (Optional Integer)) -> Integer))
  (define (find-last-call-to-win calls board)
    (let ((call (expect "no more calls" (next calls))))
      (if (bingo? (mark-in-board call board))
          call
          (find-last-call-to-win calls board))))

  (declare find-loser ((Vector Integer)
                       -> (Vector (Matrix (Optional Integer)))
                       -> (Tuple (Matrix (Optional Integer)) Integer)))
  (define (find-loser calls boards)
    (progn
      (let calls-iter = (vector-iterator calls))
      (let get-call = (fn (_) (expect "no more calls"
                                      (next calls-iter))))
      (let loop = (fn (call remaining)
                    (let ((still-remaining (mark-boards-remove-bingos call remaining)))
                      (match (vector-length still-remaining)
                        (0 (error "no loser!"))
                        (1 (let ((losing-board (vector-index-unsafe 0 still-remaining)))
                             (Tuple losing-board (find-last-call-to-win calls-iter losing-board))))
                        (_ (loop (get-call)
                                 still-remaining))))))
      (loop (get-call) boards)))

  (declare part2 (String -> Integer))
  (define (part2 path)
    (match (expect "failed to open file"
                   (with-input-file (f path) (parse-file f)))
      ((Tuple calls boards)
       (match (find-loser calls boards)
         ((Tuple losing-board last-call)
          (* (bingo-score losing-board) last-call)))))))
