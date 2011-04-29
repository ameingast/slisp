(setq *TESTS* (list (qsort '(1 2 3)) '(1 2 3)))

(and (map #'eq *TESTS*))