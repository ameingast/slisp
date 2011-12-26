;; (2 3)

(@
  (compose #'++ #'++ 0)
  (compose 
    #'(lambda (x) (+ x 1))
    #'(lambda (x) (+ x 2)) 0))