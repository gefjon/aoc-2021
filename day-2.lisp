(uiop:define-package :aoc-2021/day-2
  (:use :aoc-2021/prelude)
  (:export #:part1 #:part2))
(cl:in-package :aoc-2021/day-2)

(coalton-toplevel
  (define-type Insn
    (Forward Integer)
    (Up Integer)
    (Down Integer))

  (declare parse-insn-ctor ((Integer -> Insn) -> String -> String -> (Optional Insn)))
  (define (parse-insn-ctor ctor name line)
    (map (compose ctor (compose (expect "Malformed integer") parse-int))
         (without-leading-substring name line)))

  (declare parse-fwd (String -> (Optional Insn)))
  (define parse-fwd (parse-insn-ctor Forward "forward "))

  (declare parse-up (String -> (Optional Insn)))
  (define parse-up (parse-insn-ctor Up "up "))
  
  (declare parse-down (String -> (Optional Insn)))
  (define parse-down (parse-insn-ctor Down "down "))
  
  (declare parse-insn (String -> (Optional Insn)))
  (define (parse-insn line)
    (alt (parse-fwd line)
         (alt (parse-up line)
              (parse-down line))))

  (declare file-insns (InputFile -> (Iterator Insn)))
  (define (file-insns f)
    (map (compose (expect "Malformed insn") parse-insn) (lines f))))

(coalton-toplevel 
  (define-type SubLocation
    ;; depth, horiz
    (SubLocation Integer Integer))

  (declare update-sublocation (SubLocation -> Insn -> SubLocation))
  (define (update-sublocation loc ins)
    (match loc
      ((SubLocation depth horiz)
       (match ins
         ((Forward amt) (SubLocation depth (+ horiz amt)))
         ((Up amt) (SubLocation (- depth amt) horiz))
         ((Down amt) (SubLocation (+ depth amt) horiz))))))

  (declare follow ((Iterator Insn) -> SubLocation))
  (define (follow iter)
    (reduce update-sublocation (SubLocation 0 0) iter)))

(coalton-toplevel 
  (declare part1 (String -> Integer))
  (define (part1 path)
    (expect "Failed to open file"
            (with-input-file (f path)
              (match (follow (file-insns f))
                ((SubLocation depth horiz) (* depth horiz)))))))

(coalton-toplevel
  (define-type SubLoc2
    ;; aim, depth, horiz
    (SubLoc2 Integer Integer Integer))
  
  (declare update-subloc-2 (SubLoc2 -> Insn -> SubLoc2))
  (define (update-subloc-2 loc ins)
    (match loc
      ((SubLoc2 aim depth horiz)
       (match ins
         ((Forward amt) (SubLoc2 aim
                                 (+ depth (* aim amt))
                                 (+ horiz amt)))
         ((Up amt) (SubLoc2 (- aim amt) depth horiz))
         ((Down amt) (SubLoc2 (+ aim amt) depth horiz))))))
  
  (declare follow2 ((Iterator Insn) -> SubLoc2))
  (define (follow2 iter)
    (reduce update-subloc-2 (SubLoc2 0 0 0) iter))

  (declare part2 (String -> Integer))
  (define (part2 path)
    (expect "Failed to open file"
            (with-input-file (f path)
              (match (follow2 (file-insns f))
                ((SubLoc2 aim depth horiz) (* depth horiz)))))))
