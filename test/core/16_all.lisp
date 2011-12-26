;; (1 0)

(@
  (all #'(lambda (x) (> x 1)) '(2 3 4))
  (all #'(lambda (x) (< x 1)) '(-1 0 1)))