(uiop:define-package :aoc-2021/day-7
  (:use :aoc-2021/prelude))
(cl:in-package :aoc-2021/day-7)

(coalton-toplevel
  (define (parse-input path)
    (expect "failed to open file"
            (with-input-file (f path)
              (collect-to-vector 
               (map (compose (expect "malformed integer")
                             parse-int)
                    (split-string #\, (expect "early eof"
                                              (read-line f))))))))

  (define (position-of-min key iter)
    (map fst
         (find-min (compose key snd)
                   (enumerate iter))))

  (define (part1-cost-to-move from to)
    (abs (- from to)))

  (define (update-target-for-crab cost-to-move crab-loc vec target)
    (vector-apply-at (+ (cost-to-move crab-loc target))
                     vec
                     target))

  (define (update-all-targets-for-crab cost-to-move vec crab-loc)
    (reduce (update-target-for-crab cost-to-move crab-loc)
            vec
            (upto (vector-length vec))))
  
  (define (optimal-loc cost-to-move crabs)
    (progn
      (let len = (+ 1 (unwrap (find-max id (vector-iterator crabs)))))
      (let arr = (collect-to-vector (map (const 0) (upto len))))
      (let populated = (reduce (update-all-targets-for-crab cost-to-move)
                               arr
                               (vector-iterator crabs)))
      (unwrap (find-min id (vector-iterator populated)))))

  (define (part1-slow path)
    (optimal-loc part1-cost-to-move (parse-input path)))

  (declare part2-cost-to-move (Integer -> Integer -> Integer))
  (define (part2-cost-to-move from to)
    (let ((dist (part1-cost-to-move from to)))
      (if (zero? dist) 0
          (floor (* (+ dist 1) dist) 2))))

  (define (part2-slow path)
    (optimal-loc part2-cost-to-move (parse-input path))))

;;; as these puzzles get conceptually harder, i'll write my first attempt, then look at spoilers and discuss
;;; with peers to rewrite a better solution.

(coalton-toplevel
  (define (loc-cost cost-to-move crabs loc)
    (iterator-sum (map (cost-to-move loc) (vector-iterator crabs))))
  
  (define (part1 path)
    (progn
      (let crabs = (parse-input path))
      (vector-sort crabs)
      (loc-cost part1-cost-to-move crabs 
                (expect "not enough crabs"
                        (vector-median crabs)))))

  (define (part2 path)
    (progn
      (let crabs = (parse-input path))
      (let low-avg = (vector-average crabs))
      (let high-avg = (+ 1 low-avg))
      (min (loc-cost part2-cost-to-move crabs low-avg)
           (loc-cost part2-cost-to-move crabs high-avg)))))
