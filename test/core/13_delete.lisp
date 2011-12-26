;; (2 4 6)

(delete 
  #'(lambda (x) (not (eq 0 (mod x 2))))
  '(1 2 3 4 5 6 7))