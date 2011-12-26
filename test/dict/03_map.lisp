;; ((a b c) (1 2 3))

(setq *LIST* '(:a 1 :b 2 :c 3))

(@
  (map #'key *LIST*)
  (map #'value *LIST*))