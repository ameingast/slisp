(defun id (x) x)

(defun null? (xs)
  (eq '() xs))

(defun compose (f g x)
  (funcall f (funcall g x)))

(defun map (f xs)
  (if (eq '() xs) '()
    (cons (funcall f (car xs))
      (map f (cdr xs)))))

(defun length (xs)
  (if (null? xs) 0
    (+ 1 (length (cdr xs)))))

(defun caar (xs)
  (car (car xs)))

(defun cadr (xs)
  (car (cdr xs)))

(defun cddr (xs)
  (cdr (cdr xs)))

(defun foldl (f z xs)
  (if (null? xs) z
    (foldl f (funcall f z (car xs)) (cdr xs))))

(defun filter (f xs)
  (if (null? xs) '()
    (if (funcall f (car xs)) 
      (cons (car xs) 
        (filter f (cdr xs)))
      (filter f (cdr xs)))))  

(defun drop (n xs)
  (if (eq 0 n) xs
    (drop (- n 1) (cdr xs))))

(defun take (n xs)
  (if (eq 0 n) '()
    (cons (car xs)
      (take (- n 1) (cdr xs)))))

(defun nth (n xs)
  (if (eq 0 n) (car xs)
    (get (- n 1) (cdr xs))))

(defun insert (n x xs)
  (if (eq 0 n) (cons x xs)
    (cons (car xs) 
      (insert (- n 1) x (cdr xs)))))

(defun delete (f xs)
  (if (null? xs) '()
    (if (funcall f (car xs)) (delete f (cdr xs))
      (cons (car xs) (delete f (cdr xs))))))

(defun member (x xs)
  (if (null? xs) 0
    (if (eq x (car xs)) 1
      (member x (cdr xs)))))

(defun any (f xs)
  (let ((xxs (map f xs)))
    (member 1 xxs)))

(defun all (f xs)
  (let ((xxs (map f xs))
        (axs (any #'(lambda (x) (eq x 0)) xxs)))    
    (not axs)))

(defun drop-while (f xs)
  (if (funcall f (car xs)) (drop-while f (cdr xs))
    xs))
 
(defun take-while (f xs)
  (if (not (funcall f (car xs))) '()
    (cons (car xs)
      (take-while f (cdr xs)))))

(defun init (xs)
  (if (null? (cdr xs)) '()
    (cons (car xs)
      (init (cdr xs)))))

(defun last (xs)
  (if (null? (cdr xs)) (car xs)
    (last (cdr xs))))

(defun nub (xs)
  (if (null? xs) '()
    (if (member (car xs) (cdr xs)) (nub (cdr xs))
      (cons (car xs)
        (nub (cdr xs))))))

(defun zip-with (f xs ys)
  (if (or (null? xs) (null? ys)) '()
    (let ((head (funcall f (car xs) (car ys))))
      (cons head
        (zip-with f (cdr xs) (cdr ys))))))

(defun zip (xs ys)
  (zip-with #'list xs ys))

(defun union (xs ys)
  (cond 
    ((and (null? xs) (null? ys)) '())
    ((null? xs) ys)
    (T (if (member (car xs) ys) (union (cdr xs) ys)
          (cons (car xs) 
            (union (cdr xs) ys))))))

(defun iterate (f s c)
  (let ((ss (funcall f s)))
    (if (eq 0 c) '()
      (cons s
        (iterate f ss (-- c))))))

(defun reverse (xs)
  (if (null? xs) '()
    (append (reverse (cdr xs)) 
      (list (car xs)))))

(defun enum-from-to-step (n m s)
  (if (> n m) '()
    (cons n 
      (enum-from-to-step (+ s n) m s))))

(defun enum-from-to-step-slow (n m s)
  (filter #'(lambda (x) (<= x m)) 
      (iterate #'(lambda (x) (+ x s)) n (++ (- m n)))))

(defun enum-to-from-step (n m s)
  (if (<= n m) (list n)
    (cons n
      (enum-to-from-step (- n s) m s))))
      
(defun .. (n m)
  (enum-from-to-step n m 1))

(defun enum-from-to (n m)
  (enum-from-to-step n m 1))

(defun enum-to-from (n m)
  (enum-to-from-step n m 1))
  
(defun max (xs)
  (max-property-with #'< -INF xs))

(defun min (xs) 
  (max-property-with #'> INF xs))

(defun max-property-with (f k xs)
  (if (null? xs) k
    (let ((head (car xs))
          (tail (cdr xs))
          (kk (if (funcall f k head) head k)))
      (max-property-with f kk tail))))

(defun sum (xs) 
  (foldl #'+ 0 xs))
  
(defun prod (xs)
  (foldl #'* 1 xs))
  
(defun qsort (xs)
  (if (null? xs) '()
    (let ((head (car xs))
          (tail (cdr xs))
          (sml (filter #'(lambda (a) (<= a head)) tail))
          (grt (filter #'(lambda (a) (>  a head)) tail))
          (sml-sorted (qsort sml))
          (grt-sorted (qsort grt)))
      (append sml-sorted
        (cons head 
          grt-sorted)))))
