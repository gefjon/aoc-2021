(uiop:define-package :aoc-2021/prelude
  (:use-reexport :coalton :coalton-library)
  (:import-from :alexandria #:if-let)
  (:export

   #:Iterator #:next #:reduce #:reduce2

   #:InputFile #:open-read #:close-read #:read-line #:lines
   #:call-with-input-file #:with-input-file

   #:flatten #:unwrap #:debug))
(cl:in-package :aoc-2021/prelude)

(cl:defun nullable-to-optional (thing)
  (cl:if thing
         (Some thing)
         None))

(coalton-toplevel 
  (define-type (Iterator :item)
    (Iterator (Unit -> (Optional :item))))

  (declare next ((Iterator :item) -> (Optional :item)))
  (define (next i)
    (match i
      ((Iterator func) (func Unit))))

  (define-instance (Functor Iterator)
    (define (map func iter)
      (match iter
        ((Iterator i)
         (Iterator (fn (u)
                     (map func (i u)))))))))

(coalton-toplevel 
  (declare reduce ((:state -> :item -> :state) -> :state -> (Iterator :item) -> :state))
  (define (reduce func init iter)
    (match (next iter)
      ((Some item) (reduce func
                           (func init item)
                           iter))
      ((None) init)))

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
       (%reduce2 func init first iter)))))

(coalton-toplevel 
  (define-type InputFile
    (InputFile Lisp-Object))

  (declare open-read (String -> (Optional InputFile)))
  (define (open-read path)
    (lisp (Optional InputFile) (path)
      (nullable-to-optional (cl:open path :direction :input
                                          :element-type 'cl:character
                                          :if-does-not-exist cl:nil))))

  (declare read-line (InputFile -> (Optional String)))
  (define (read-line file)
    (match file
      ((InputFile f)
       (lisp (Optional String) (f)
         (nullable-to-optional (cl:read-line f cl:nil cl:nil))))))

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
  (define (lines f) (Iterator (fn (unit) (read-line f))))
  
  (declare flatten ((Optional (Optional :a)) -> (Optional :a)))
  (define (flatten x)
    (match x
      ((Some inner) inner)
      ((None) None)))

  (declare unwrap ((Optional :a) -> :a))
  (define (unwrap opt)
    (match opt
      ((Some a) a)
      ((None) (error "None in unwrap")))))

(coalton-toplevel
  (declare debug (String -> :a -> :a))
  (define (debug msg a)
    (progn 
      (traceObject msg a)
      a)))

(cl:defmacro with-input-file ((f path) cl:&body body)
  `(call-with-input-file (fn (,f) ,@body)
                         ,path))
