(uiop:define-package :aoc-2021/day-1
  (:use :aoc-2021/prelude))
(cl:in-package :aoc-2021/day-1)

(coalton-toplevel
  (declare file-line-ints 
           (InputFile -> (Iterator Integer)))
  (define (file-line-ints file)
    (map (compose unwrap parse-int)
         (lines file))))

(coalton-toplevel
  (declare count-increases (InputFile -> Integer))
  (define (count-increases input)
    (reduce2 (fn (count prev next)
               (if (> next prev)
                   (+ count 1)
                   count))
             0
             (file-line-ints input))))
