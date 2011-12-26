;; (2 4 6)

(filter 
  #'(lambda (x) (eq 0 (mod x 2))) 
  '(1 2 3 4 5 6 7))