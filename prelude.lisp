(uiop:define-package :aoc-2021/prelude
  (:use-reexport :coalton :coalton-library)
  (:export

   #:if-let #:format

   #:Iterator #:next #:reduce #:unwrapped #:empty-iterator #:for-each #:iterator-sum #:every? #:any?
   #:range #:range-inclusive #:upto
   #:find-min #:find-max #:count #:iterator-length

   #:InputFile #:open-read #:close-read #:read-line #:lines #:eof?
   #:call-with-input-file #:with-input-file

   #:flatten #:unwrap #:expect #:debug

   #:strlen #:string-empty? #:substring #:leading-substring? #:without-leading-substring #:split-string

   #:parse-int-with-base

   #:bitfield-extract #:bitfield-insert #:bit-set? #:zero? #:plus? #:minus? #:sign #:bitwise-invert

   #:vector-apply-at #:vector-iterator #:collect-to-vector #:collect-to-list

   #:Matrix #:make-matrix
   #:matrix-rows #:matrix-columns
   #:matrix-index-unsafe #:matrix-index #:matrix-write!-unsafe #:matrix-update!-unsafe
   #:matrix-row-iterator #:matrix-col-iterator
   
   #:call/ec #:EscapeContinuation #:throw #:let/ec

   #:uncurry))
(cl:in-package :aoc-2021/prelude)

(cl:declaim (cl:optimize (cl:speed 3)
                         (cl:debug 1) ; gimme those tail calls!
                         (cl:safety 2)))

(cl:defun nullable-to-optional (thing)
  (cl:if thing
         (Some thing)
         None))

(cl:defmacro if-let ((pat term) cl:&body (then else))
  `(match ,term
     (,pat ,then)
     (_ ,else)))

(cl:defmacro matches? (pat term)
  `(match ,term
     (,pat True)
     (_ False)))

(cl:defmacro format (template cl:&rest vars)
  (cl:assert (cl:typep template 'cl:string) ()
             "format template must be a string literal")
  (cl:dolist (var vars)
    (cl:assert (cl:typep var 'cl:symbol) ()
               "format arguments must be bare variables, not exprs"))
  `(lisp Unit ,vars
     (cl:progn
       (cl:format cl:t ,template ,@vars)
       Unit)))

;;;; iterator
;;; type definition
(coalton-toplevel
  (define-type (Iterator :item)
    (Iterator (Unit -> (Optional :item)))))

;;; operators
(coalton-toplevel
  (declare next ((Iterator :item) -> (Optional :item)))
  (define (next i)
    (match i
      ((Iterator func) (func Unit))))

  (declare zip ((Iterator :a) -> (Iterator :b) -> (Iterator (Tuple :a :b))))
  (define (zip left right)
    (Iterator (fn (_)
                (if-let ((Tuple (Some l) (Some r)) (Tuple (next left) (next right)))
                  (Some (Tuple l r))
                  None))))

  (declare filter ((:a -> Boolean) -> (Iterator :a) -> (Iterator :a)))
  (define (filter keep? iter)
    (let ((filter-iter (fn  (u)
                         (if-let ((Some item) (next iter))
                           (if (keep? item)
                               (Some item)
                             (filter-iter u))
                         None))))
      (Iterator filter-iter)))

  (declare unwrapped ((Iterator (Optional :a)) -> (Iterator :a)))
  (define (unwrapped iter)
    (let ((unwrapped-iterator
            (fn (u)
              (match (next iter)
                ((None) None)
                ((Some (Some item)) (Some item))
                ((Some (None)) (unwrapped-iterator u))))))
      (Iterator unwrapped-iterator))))

;;; instances
(coalton-toplevel
  (define-instance (Functor Iterator)
    (define (map func iter)
      (match iter
        ((Iterator i)
         (Iterator (fn (u)
                     (map func (i u)))))))))

;;; constructors
(coalton-toplevel
  (declare empty-iterator (Iterator :a))
  (define empty-iterator (Iterator (fn (_) None)))

  (declare range (Integer -> Integer -> (Iterator Integer)))
  (define (range start end)
    (let ((step (if (> start end) -1 1))
          (finished? (if (> start end) <= >=))
          (state (make-cell start)))
      (Iterator
       (fn (_) (let ((old (cell-read state)))
                 (if (finished? old end)
                     None
                     (Some (cell-swap (+ step old) state))))))))

  (declare range-inclusive (Integer -> Integer -> (Iterator Integer)))
  (define (range-inclusive start end)
    (let ((step (if (> start end) -1 1))
          (finished? (if (> start end) < >))
          (state (make-cell start)))
      (iterator
       (fn (_) (let ((old (cell-read state)))
                 (if (finished? old end)
                     None
                     (Some (cell-swap (+ step old) state))))))))

  (declare upto (Integer -> (Iterator Integer)))
  (define (upto end)
    (range 0 end))

  (declare cl-list-iterator-unsafe (Lisp-Object -> (Iterator :a)))
  (define (cl-list-iterator-unsafe lst)
    (let ((cell (make-cell lst)))
      (Iterator (fn (_)
                  (lisp (Optional :a) (cell)
                    (alexandria:if-let (head (cell-read cell))
                      (Some (cl:progn (cell-write (cl:rest head) cell)
                                      (cl:first head)))
                      None)))))))

;;; consumers
(coalton-toplevel
  (declare reduce ((:state -> :item -> :state) -> :state -> (Iterator :item) -> :state))
  (define (reduce func init iter)
    (match (next iter)
      ((Some item) (reduce func
                           (func init item)
                           iter))
      ((None) init)))

  (declare iterator-sum ((Num :a) => ((Iterator :a) -> :a)))
  (define iterator-sum (reduce + (fromInt 0)))

  (declare iterator-length ((Iterator :a) -> Integer))
  (define (iterator-length iter)
    (reduce +
            0
            (map (const 1) iter)))

  (declare count ((:a -> Boolean) -> (Iterator :a) -> Integer))
  (define (count count? iter)
    (reduce +
            0
            (map (fn (item) (if (count? item) 1 0)) iter)))

  (declare for-each ((:item -> :a) -> (Iterator :item) -> Unit))
  (define (for-each func iter)
    (reduce (fn (_) (compose (const Unit) func)) Unit iter))

  (declare every? ((:item -> Boolean) -> (Iterator :item) -> Boolean))
  (define (every? pred iter)
    (match (next iter)
      ((None) True)
      ((Some item) (and (pred item) (every? pred iter)))))

  (declare any? ((:item -> Boolean) -> (Iterator :item) -> Boolean))
  (define (any? pred iter)
    (not (every? (compose not pred) iter)))

  (declare find-min ((Ord :num) => ((Iterator :num) -> (Optional :num))))
  (define (find-min iter)
    (match (next iter)
      ((Some first)
       (Some (reduce min first iter)))
      ((None) None)))

  (declare find-max ((Ord :num) => ((Iterator :num) -> (Optional :num))))
  (define (find-max iter)
    (match (next iter)
      ((Some first)
       (Some (reduce max first iter)))
      ((None) None)))

  (declare collect-to-list ((Iterator :item) -> (List :item)))
  (define (collect-to-list iter)
    (reverse (reduce (flip Cons) Nil iter))))

(coalton-toplevel
  (define-type InputFile
    (InputFile Lisp-Object))

  (declare open-read (String -> (Optional InputFile)))
  (define (open-read path)
    (map InputFile 
         (lisp (Optional Lisp-Object) (path)
           (nullable-to-optional (cl:open path :direction :input
                                               :element-type 'cl:character
                                               :if-does-not-exist cl:nil)))))

  (declare read-line (InputFile -> (Optional String)))
  (define (read-line file)
    (match file
      ((InputFile f)
       (lisp (Optional String) (f)
         (nullable-to-optional (cl:read-line f cl:nil cl:nil))))))

  (declare eof? (InputFile -> Boolean))
  (define (eof? f)
    (match f
      ((InputFile stream)
       (lisp Boolean (stream)
         (alexandria:if-let (ch (cl:read-char stream cl:nil cl:nil))
           (cl:progn (cl:unread-char ch stream)
                     False)
           True)))))

  (declare close-read (InputFile -> Unit))
  (define (close-read file)
    (match file
      ((InputFile f)
       (lisp Unit (f)
         (cl:progn (cl:close f)
                   Unit)))))

  (declare call-with-input-file ((InputFile -> :a) -> String -> (Optional :a)))
  (define (call-with-input-file thunk path)
    (lisp (Optional :a) (thunk path)
      (cl:with-open-file (f path :direction :input
                                 :element-type 'cl:character
                                 :if-does-not-exist cl:nil)
        (cl:if f (Some (cl:funcall thunk (InputFile f)))
               None))))

  (declare lines (InputFile -> (Iterator String)))
  (define (lines f) (Iterator (fn (_) (read-line f))))

  (declare flatten ((Optional (Optional :a)) -> (Optional :a)))
  (define (flatten x)
    (match x
      ((Some inner) inner)
      ((None) None)))

  (declare unwrap ((Optional :a) -> :a))
  (define (unwrap opt)
    (match opt
      ((Some a) a)
      ((None) (error "None in unwrap"))))


  (declare expect (String -> (Optional :a) -> :a))
  (define (expect msg opt)
    (match opt
      ((Some a) a)
      ((None) (error msg)))))

(coalton-toplevel
  (declare debug (String -> :a -> :a))
  (define (debug msg a)
    (progn
      (traceObject msg a)
      a)))

(cl:defmacro with-input-file ((f path) cl:&body body)
  `(call-with-input-file (fn (,f) ,@body)
                         ,path))

(coalton-toplevel
  (declare strlen (String -> Integer))
  (define (strlen str)
    (lisp Integer (str)
      (cl:length str)))

  (declare string-empty? (String -> Boolean))
  (define (string-empty? str) (== (strlen str) 0))

  (declare substring (String -> Integer -> Integer -> String))
  (define (substring str start end)
    (lisp String (str start end)
      (cl:progn
        (cl:assert (cl:>= end start))
        (cl:let ((real-end (cl:min end (strlen str))))
          ;; i have a PR out https://github.com/coalton-lang/coalton/pull/229 to include displaced strings in
          ;; the type `String'. until it gets merged, the following will not build on
          ;; `coalton-lang/coalton/main'. use https://github.com/gefjon/coalton/tree/displaced-string instead.
          (cl:make-array (cl:- real-end start)
                         :displaced-to str
                         :displaced-index-offset start
                         :element-type (cl:array-element-type str))))))

  (declare leading-substring? (String -> String -> Boolean))
  (define (leading-substring? small big)
    (== small (substring big 0 (strlen small))))

  (declare without-leading-substring (String -> String -> (Optional String)))
  (define (without-leading-substring small big)
    (if (leading-substring? small big)
        (Some (substring big (strlen small) (strlen big)))
        None))

  (declare parse-int-with-base (Integer -> String -> (Optional Integer)))
  (define (parse-int-with-base base input)
    (lisp (Optional Integer) (base input)
      (nullable-to-optional (cl:parse-integer input
                                              :junk-allowed cl:t
                                              :radix base))))

  (declare split-string (Char -> String -> (Iterator String)))
  (define (split-string delim str)
    ;; args in this order to support currying
    (lisp (Iterator String) (delim str)
      (cl-list-iterator-unsafe
       (split-sequence:split-sequence delim str)))))

(coalton-toplevel
  (declare bitfield-extract (Integer -> Integer -> Integer -> Integer))
  (define (bitfield-extract size start int)
    (lisp Integer (start size int)
      (cl:ldb (cl:byte size start) int)))

  (declare bitfield-insert (Integer -> Integer -> Integer -> Integer -> Integer))
  (define (bitfield-insert size start new int)
    (lisp Integer (start size new int)
      (cl:dpb new (cl:byte size start) int)))

  (declare bit-set? (Integer -> Integer -> Boolean))
  (define (bit-set? index int)
    (== 1 (bitfield-extract 1 index int)))

  (declare zero? (Integer -> Boolean))
  (define zero?
    (== 0))
  
  (declare plus? (Integer -> Boolean))
  (define (plus? int)
    (> int 0))
  (declare minus? (Integer -> Boolean))
  (define (minus? int)
    (< int 0))

  (declare sign (Integer -> Integer))
  (define (sign int)
    (lisp Integer (int)
      (cl:signum int)))

  (declare bitwise-invert (Integer -> Integer))
  (define (bitwise-invert int)
    (lisp Integer (int)
      (cl:lognot int))))

(coalton-toplevel
  (declare vector-apply-at ((:a -> :a) -> (Vector :a) -> Integer -> (Vector :a)))
  (define (vector-apply-at func vector index)
    "It's a destructive operator, but it still returns the vector for purposes of `reduce'-ing"
    (if-let ((Some old) (vector-index index vector))
      (progn (vector-set index (func old) vector) vector)
      (error "OOB index in vector-apply-at")))

  (declare vector-iterator ((Vector :a) -> (Iterator :a)))
  (define (vector-iterator vec)
    (map (fn (idx) (vector-index-unsafe idx vec))
         (upto (vector-length vec))))

  (declare collect-to-vector ((Iterator :a) -> (Vector :a)))
  (define (collect-to-vector iter)
    (progn
      (let vec = (make-vector Unit))
      (for-each (fn (item) (vector-push item vec))
                         iter)
      vec)))

(coalton-toplevel
  (define-type (Matrix :a)
    (Matrix Lisp-Object))

  (declare matrix-rows ((Matrix :a) -> Integer))
  (define (matrix-rows mat)
    (match mat
      ((Matrix arr) (lisp Integer (arr)
                      (cl:array-dimension arr 0)))))

  (declare matrix-columns ((Matrix :a) -> Integer))
  (define (matrix-columns mat)
    (match mat
      ((Matrix arr) (lisp Integer (arr)
                      (cl:array-dimension arr 1)))))
  
  (declare make-matrix (Integer -> Integer -> :a -> (Matrix :a)))
  (define (make-matrix rows cols initial-element)
    (Matrix (lisp Lisp-Object (rows cols initial-element)
              (cl:make-array (cl:list rows cols)
                             :initial-element initial-element))))
  
  (declare matrix-index-unsafe ((Matrix :a) -> Integer -> Integer -> :a))
  (define (matrix-index-unsafe mat row col)
    (match mat
      ((Matrix arr) (lisp :a (arr row col)
                      (cl:aref arr row col)))))

  (declare matrix-row-inbounds ((Matrix :a) -> Integer -> Boolean))
  (define (matrix-row-inbounds mat row)
    (and (>= row 0) (< row (matrix-rows mat))))

  (declare matrix-col-inbounds ((Matrix :a) -> Integer -> boolean))
  (define (matrix-col-inbounds mat col)
    (and (>= col 0) (< col (matrix-columns mat))))
  
  (declare matrix-index ((Matrix :a) -> Integer -> Integer -> (Optional :a)))
  (define (matrix-index mat row col)
    (if (and (matrix-row-inbounds mat row)
             (matrix-col-inbounds mat col))
        (Some (matrix-index-unsafe mat row col))
        None))

  (declare matrix-write!-unsafe ((Matrix :a) -> :a -> Integer -> Integer -> (Matrix :a)))
  (define (matrix-write!-unsafe mat new-val row col)
    "a destructive operator, but it returns the matrix so you can reduce"
    (progn (match mat
             ((Matrix arr) (lisp :a (arr new-val row col)
                             (cl:setf (cl:aref arr row col) new-val))))
           mat))

  (declare matrix-update!-unsafe ((:a -> :a) -> (Matrix :a) -> Integer -> Integer -> (Matrix :a)))
  (define (matrix-update!-unsafe func mat row col)
    (matrix-write!-unsafe mat
                          (func (matrix-index-unsafe mat row col))
                          row
                          col))

  (declare matrix-row-iterator ((Matrix :a) -> Integer -> (Iterator :a)))
  (define (matrix-row-iterator mat row)
    (progn (unless (matrix-row-inbounds mat row)
             (error "oob row in matrix-row-iterator"))
           (map (matrix-index-unsafe mat row)
                (upto (matrix-columns mat)))))

  (declare matrix-col-iterator ((Matrix :a) -> Integer -> (Iterator :a)))
  (define (matrix-col-iterator mat col)
    (progn (unless (matrix-col-inbounds mat col)
             (error "oob col in matrix-col-iterator"))
           (map ((flip (matrix-index-unsafe mat)) col)
                (upto (matrix-rows mat))))))

(coalton-toplevel
  (define-type (EscapeContinuation :res)
    (EscapeContinuation Lisp-Object))

  (declare throw ((EscapeContinuation :res) -> :res -> :any))
  (define (throw ec res)
    (match ec
      ((EscapeContinuation escape)
       (lisp :any (escape res)
         (cl:funcall escape res)))))
  
  (declare call/ec (((EscapeContinuation :res) -> :res) -> :res))
  (define (call/ec thunk)
    (lisp :res (thunk)
      (cl:block cl:nil
        (cl:flet ((ec (res)
                    (cl:return res)))
          (cl:funcall thunk (EscapeContinuation #'ec)))))))

(cl:defmacro let/ec (ec cl:&body (body))
  `(call/ec (fn (,ec) ,body)))

(coalton-toplevel
  (declare uncurry ((:a -> :b -> :c) -> (Tuple :a :b) -> :c))
  (define (uncurry func tupl)
    (match tupl
      ((Tuple a b) (func a b)))))
