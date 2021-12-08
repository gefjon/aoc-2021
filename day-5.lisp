(uiop:define-package :aoc-2021/day-5
  (:use :aoc-2021/prelude))
(cl:in-package :aoc-2021/day-5)

(coalton-toplevel
  (define-type Pt
    (Pt Integer Integer))
  (define-type Line
    (Line Pt Pt))

  (declare parse-pt (String -> Pt))
  (define (parse-pt str)
    (match (collect-to-list (split-string #\, str))
      ((Cons x (Cons y (Nil)))
       (Pt (unwrap (parse-int x))
           (unwrap (parse-int y))))))

  (declare parse-line (String -> Line))
  (define (parse-line str)
    (match (collect-to-list (split-string #\space str))
      ((Cons left (Cons "->" (Cons right (Nil))))
       (Line (parse-pt left) (parse-pt right)))
      (_ (error "unexpected line format"))))

  (declare file-lines ((Line -> Boolean) -> String -> (Vector Line)))
  (define (file-lines keep? path)
    (expect "unable to open file"
            (with-input-file (f path)
              (collect-to-vector (filter keep? (map parse-line (lines f)))))))

  (declare line-max-x (Line -> Integer))
  (define (line-max-x line)
    (match line
      ((Line (Pt x1 _) (Pt x2 _)) (max x1 x2))))

  (declare line-max-y (Line -> Integer))
  (define (line-max-y line)
    (match line
      ((Line (Pt _ y1) (Pt _ y2)) (max y1 y2))))

  (declare alloc-map ((Vector Line) -> (Matrix Integer)))
  (define (alloc-map lines)
    (progn
      (let max-x = (unwrap (find-max id (map line-max-x (vector-iterator lines)))))
      (let max-y = (unwrap (find-max id (map line-max-y (vector-iterator lines)))))
      (make-matrix (+ 1 max-x) (+ 1 max-y) 0)))

  (declare horiz? (Line -> Boolean))
  (define (horiz? line)
    (match line
      ((Line (Pt _ y1) (Pt _ y2)) (== y1 y2))))

  (declare vert? (Line -> Boolean))
  (define (vert? line)
    (match line
      ((Line (Pt x1 _) (Pt x2 _)) (== x1 x2))))

  (define (mark-horizontal mat line)
    (match line
      ((Line (Pt x1 y) (pt x2 _))
       (reduce (fn (mat x)
                 (matrix-update!-unsafe (+ 1) mat x y))
               mat
               (range-inclusive x1 x2)))))

  (define (mark-vertical mat line)
    (match line
      ((Line (Pt x y1) (Pt _ y2))
       (reduce (fn (mat y)
                 (matrix-update!-unsafe (+ 1) mat x y))
               mat
               (range-inclusive y1 y2)))))

  (declare mark-line ((Matrix Integer) -> Line -> (Matrix Integer)))
  (define (mark-line mat line)
    (cond ((horiz? line) (mark-horizontal mat line))
          ((vert? line) (mark-vertical mat line))
          (True (error "don't know how to handle non-horiz non-vert lines"))))

  (define (count-gt2 mat)
    (iterator-sum
     (map (compose (count (<= 2))
                   (matrix-row-iterator mat))
          (upto (matrix-rows mat)))))

  (define (part1 path)
    (progn
      (let lines = (file-lines (fn (line) (or (horiz? line) (vert? line)))
                               path))
      (let mat = (reduce mark-line
                         (alloc-map lines)
                         (vector-iterator lines)))
      (count-gt2 mat))))

(coalton-toplevel
  (define (diag? line)
    (match line
      ((Line (Pt x1 y1) (pt x2 y2))
       (== (abs (- x1 x2)) (abs (- y1 y2))))))

  (define (diag-iter line)
    (match line
      ((Line (Pt x1 y1) (Pt x2 y2))
       (map (uncurry Pt)
            (zip (range-inclusive x1 x2)
                 (range-inclusive y1 y2))))))

  (define (mark-pt mat pt)
    (match pt
      ((Pt x y) (matrix-update!-unsafe (+ 1) mat x y))))
  
  (define (mark-diag mat line)
    (reduce mark-pt mat (diag-iter line)))

  (define (mark-line2 mat line)
    (cond ((horiz? line) (mark-horizontal mat line))
          ((vert? line) (mark-vertical mat line))
          ((diag? line) (mark-diag mat line))
          (True (error "is that even a line?"))))

  (define (part2 path)
    (progn
      (let lines = (file-lines (const True) path))
      (let mat = (reduce mark-line2
                         (alloc-map lines)
                         (vector-iterator lines)))
      (count-gt2 mat))))
