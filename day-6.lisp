(uiop:define-package :aoc-2021/day-6
  (:use :aoc-2021/prelude))
(cl:in-package :aoc-2021/day-6)

;;; a bad implementation, for posterity:

(coalton-toplevel
  (define-type Fish
    (Fish (Cell Integer)))
  (define (make-fish days-to-spawn)
    (Fish (make-cell days-to-spawn)))
  (define (spawn-fish _)
    (make-fish 8))

  (define (parse-fishes line)
    (collect-to-vector (map (compose make-fish
                                     (compose unwrap parse-int))
                            (split-string #\, line))))

  (define (file-fishes path)
    (expect "failed to open file"
            (with-input-file (f path)
              (parse-fishes (expect "failed to read line"
                                    (read-line f))))))

  (define (advance-fish fsh)
    (match fsh
      ((Fish days-to-spawn)
       (if (zero? (cell-read days-to-spawn))
           (progn (cell-write 6 days-to-spawn)
                  True)
           (progn (cell-update (+ -1) days-to-spawn)
                  False)))))

  (define (count-new-spawns fishes)
    (count id (map advance-fish (vector-iterator fishes))))

  (define (add-new-fishes fishes new-count)
    (reduce (fn (fishes _)
              (progn 
                (vector-push (spawn-fish)
                             fishes)
                fishes))
            fishes
            (upto new-count)))

  (define (step-simulator fishes)
    (add-new-fishes fishes (count-new-spawns fishes)))

  (define (simulate fishes steps)
    (reduce (fn (fishes _)
              (step-simulator fishes))
            fishes
            (upto steps)))

  (define (puzzle-slow path steps)
    (vector-length (simulate (file-fishes path) steps)))

  (define (part1-slow path)
    (puzzle-slow path 80))
  (define (part2-slow path)
    (puzzle-slow path 256)))

;;; a better implementation:
(coalton-toplevel
  (define (empty-state _)
    (collect-to-vector (map (const 0) (upto 10))))

  (define (add-fish state days-to-spawn)
    (vector-apply-at (+ 1) state days-to-spawn))

  (define (parse-state line)
    (reduce add-fish (empty-state)
            (map (compose unwrap parse-int)
                 (split-string #\, line))))

  (define (next-state old)
    (progn
      (let new = (empty-state))
      (for-each (fn (idx)
                  (vector-set (- idx 1)
                              (vector-index-unsafe idx old)
                              new))
                (range-inclusive 1 8))
      (let spawning = (vector-index-unsafe 0 old))
      (vector-apply-at (+ spawning)
                       new
                       8)
      (vector-apply-at (+ spawning)
                       new
                       6)
      new))

  (define (file-initial-state path)
    (unwrap 
     (with-input-file (f path)
       (parse-state (unwrap (read-line f))))))

  (define (puzzle path steps)
    (iterator-sum (vector-iterator
                   (reduce (fn (state _)
                             (next-state state))
                           (file-initial-state path)
                           (upto steps)))))

  (define (part1 path) (puzzle path 80))
  (define (part2 path) (puzzle path 256)))
