;; ((1 2 3) (1 2 3) (3 2 1) (1 3 5) (5 3 1))

(@
  (.. 1 3)
  (enum-from-to 1 3)
  (enum-to-from 3 1)
  (enum-from-to-step 1 5 2)
  (enum-to-from-step 5 1 2))