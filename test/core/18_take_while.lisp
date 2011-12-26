;; (1 2 3)

(take-while
  #'(lambda (x) (< x 4))
  '(1 2 3 4 5 6))