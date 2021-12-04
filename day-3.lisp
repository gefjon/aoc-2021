(uiop:define-package :aoc-2021/day-3
  (:use :aoc-2021/prelude))
(cl:in-package :aoc-2021/day-3)

(coalton-toplevel
  ;; i'll iterate through the list of numbers, keeping a vector of counters as state. for each zero bit, i'll
  ;; decrement the corresponding counter; for each one bit, i'll increment it. then plusp -> one, minusp ->
  ;; zero. not sure how to handle ties.

  (declare bits-per-entry Integer)
  (define bits-per-entry 12)

  (declare make-counter-vec (Unit -> (Vector Integer)))
  (define (make-counter-vec _)
    (let ((vec (make-vector-capacity bits-per-entry)))
      (progn
        (for-each (fn (_) (vector-push 0 vec))
                           (upto bits-per-entry))
        vec)))

  (declare bit-counter-delta (Integer -> Integer -> Integer))
  (define (bit-counter-delta bit int)
    (if (bit-set? bit int)
        1
        -1))

  (declare update-counters-for-entry ((Vector Integer) -> Integer -> (Vector Integer)))
  (define (update-counters-for-entry counters int)
    (reduce (fn (counters bit)
              (vector-apply-at (+ (bit-counter-delta bit int))
                                                  bit
                                                  counters))
            counters
            (upto bits-per-entry)))

  (declare count-all ((Iterator Integer) -> (Vector Integer)))
  (define (count-all iter)
    (reduce update-counters-for-entry
            (make-counter-vec Unit)
            iter))

  (declare counter-to-int ((Integer -> Boolean) -> (Vector Integer) -> Integer))
  (define (counter-to-int bit? counter)
    (reduce (fn (int bit)
              (bitfield-insert 1
                               bit
                               (if (bit? (vector-index-unsafe bit counter))
                                   1
                                   0)
                               int))
            0
            (upto bits-per-entry)))

  (declare counter-gamma ((Vector Integer) -> Integer))
  (define counter-gamma (counter-to-int plus?))

  (declare counter-epsilon ((Vector Integer) -> Integer))
  (define counter-epsilon (counter-to-int minus?))

  (declare file-line-ints (InputFile -> (Iterator Integer)))
  (define (file-line-ints file)
    (map (compose (expect "Ill-formed binary integer") (parse-int-with-base 2))
         (lines file)))

  (declare part1 (String -> Integer))
  (define (part1 path)
    (expect "Failed to open file"
     (with-input-file (f path)
       (progn
         (let counter = (count-all (file-line-ints f)))
         (let gamma = (counter-gamma counter))
         (let epsilon = (counter-epsilon counter))
         (* gamma epsilon))))))

(coalton-toplevel
  (declare read-entries (String -> (Vector Integer)))
  (define (read-entries path)
    (expect "Failed to open file"
            (with-input-file (f path)
              (collect-to-vector (file-line-ints f)))))

  (declare entries-count-bit (Integer -> (Vector Integer) -> Integer))
  (define (entries-count-bit bit entries)
    (iterator-sum (map (compose (fn (b)
                                  (if b 1 -1))
                                (bit-set? bit))
                       (vector-iterator entries))))

  (declare entries-parameter ((Integer -> Boolean)
                              -> (Vector Integer)
                              -> Integer))
  (define (entries-parameter counter-desired-bit entries)
    (let ((inner (fn (remaining-entries bit-to-consider)
                   (match (vector-length remaining-entries)
                     (0 (error "no matching parameter found!"))
                     (1 (vector-index-unsafe 0 remaining-entries))
                     (_ (progn
                          (when (minus? bit-to-consider)
                            (error "too many remaining candidates!"))
                          (let counter = (entries-count-bit bit-to-consider remaining-entries))
                          (let counter-bit = (counter-desired-bit counter))
                          (let keep? = (fn (candidate)
                                         (== counter-bit (bit-set? bit-to-consider candidate))))
                          (let remaining = (collect-to-vector (filter keep?
                                                                      (vector-iterator remaining-entries))))
                          (inner remaining (- bit-to-consider 1))))))))
      (inner entries (- bits-per-entry 1))))

  (declare entries-oxygen ((Vector Integer) -> Integer))
  (define entries-oxygen (entries-parameter (compose not minus?)))

  (declare entries-co2 ((Vector Integer) -> Integer))
  (define entries-co2 (entries-parameter minus?))

  (declare part2 (String -> Integer))
  (define (part2 path)
    (progn
      (let entries = (read-entries path))
      (let oxygen = (entries-oxygen entries))
      (let co2 = (entries-co2 entries))
      (* oxygen co2))))
