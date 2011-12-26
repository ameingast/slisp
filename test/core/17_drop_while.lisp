;; (4 5 6)

(drop-while 
  #'(lambda (x) (< x 4))
  '(1 2 3 4 5 6))