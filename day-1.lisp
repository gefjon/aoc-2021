(uiop:define-package :aoc-2021/day-1
  (:use :aoc-2021/prelude)
  (:export #:part1 #:part2 #:file-line-ints))
(cl:in-package :aoc-2021/day-1)

(coalton-toplevel
  (declare file-line-ints 
           (InputFile -> (Iterator Integer)))
  (define (file-line-ints file)
    (map (compose unwrap parse-int)
         (lines file))))

;; part1
(coalton-toplevel
  (declare %reduce2 ((:state -> :item -> :item -> :state)
                     -> :state
                     -> :item
                     -> (Iterator :item)
                     -> :state))
  (define (%reduce2 func state first-item iter)
    (match (next iter)
      ((None) state)
      ((Some second-item)
       (%reduce2 func
                 (func state first-item second-item)
                 second-item
                 iter))))

  (declare reduce2 ((:state -> :item -> :item -> :state)
                    -> :state
                    -> (Iterator :item)
                    -> :state))
  (define (reduce2 func init iter)
    (match (next iter)
      ((None) init)
      ((Some first)
       (%reduce2 func init first iter))))

  (declare count-increases ((Iterator Integer) -> Integer))
  (define (count-increases ints)
    (reduce2 (fn (count prev next)
               (if (> next prev)
                   (+ count 1)
                   count))
             0
             ints))

  (declare part1 (String -> Integer))
  (define (part1 path)
    (unwrap (with-input-file (f path)
              (count-increases (file-line-ints f))))))

;; part2
(coalton-toplevel
  (declare %triples ((Cell :a) -> (Cell :a) -> (Iterator :a) -> Unit -> (Optional (Tuple3 :a :a :a))))
  (define (%triples first-cell second-cell i)
    (fn (u)
      (match (next i)
        ((None) None)
        ((Some third)
         (let ((second (cell-swap third second-cell))
               (first (cell-swap second first-cell)))
           (Some (Tuple3 first second third)))))))

  (declare triples ((Iterator :a) -> (Iterator (Tuple3 :a :a :a))))
  (define (triples i)
    (match (next i)
      ((None) empty-iterator)
      ((Some first)
       (match (next i)
         ((None) empty-iterator)
         ((Some second)
          (Iterator (%triples (make-cell first) (make-cell second) i)))))))

  (declare sum-triple ((Tuple3 Integer Integer Integer) -> Integer))
  (define (sum-triple trip)
    (match trip
      ((Tuple3 a b c) (+ (+ a b) c))))

  (declare part2 (String -> Integer))
  (define (part2 path)
    (unwrap (with-input-file (f path)
              (count-increases (map sum-triple (triples (file-line-ints f))))))))

