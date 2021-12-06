(defsystem "aoc-2021"
  :depends-on ("coalton"
               "alexandria"
               "split-sequence")
  :serial t
  :components ((:file "prelude")
               (:file "day-1")
               (:file "day-2")
               (:file "day-3")
               (:file "day-4")
               (:file "day-5")
               (:file "day-6")))
