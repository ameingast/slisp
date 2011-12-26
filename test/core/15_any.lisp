;; (1 0)

(@
  (any #'(lambda (x) (> x 1)) '(1 2 3))
  (any #'(lambda (x) (> x 5)) '(1 2 3)))