;; C1. Toys
(defun atom? (x)
  (not (listp x)))

(atom? 'Harry)
(atom? '*abc$)

;; nil
(car '())

(defvar l '(((hotdogs)) (and) (pickle) relish))
;; ((hotdogs))
(car l)

(cdr '(a))

(defvar s '((a b c)))
(setq l 'b)
(cons 'a 'b)

(null 'a)

(atom? '(abc efg))
(atom? (cdr '(abc)))

(setq a1 'Harry)
(setq a2 'HarryPotter)
(eq a1 a2)

(setq l1 '(Harry))
(eq l1 l1)

(setq n1 6)
(setq n2 6)
(eq n1 n2)

;; C2. Do It, Do It Again, and Again, and Again

(defun lat? (l)
  (cond
   ((null l) t)
   ((atom? (car l)) (lat? (cdr l)))
   (t nil)))

(defun member? (a lat)
  (cond
   ((null lat) nil)
   (t (or (eq (car lat) a) (member? a (cdr lat))))
   )
  )

(setq l1 '(a1 a2))
(setq l2 '(b1 (b2)))
(lat? l1)
(lat? l2)

(member? 'a2 l1)
(member? 'a3 l1)

;; C3. Cons the Magnificent
(defun rember (a lat)
  (cond
   ((null lat) nil)
   ((eq (car lat) a) (cdr lat))
   (t (cons (car lat) (rember a (cdr lat))))
   )
  )

(defvar c1 '(v1 v2 v3 v4 v2))
(rember 'v2 c1)

(defun firsts (l)
  (cond
   ((null l) nil)
   (t (cons (car (car l)) (firsts (cdr l))))
   )
  )

(defvar c2
  '(
    (a b c d)
    (e f g h)
    (i j k l)
    )
  )

(firsts c2)

(defun insertR (new old lat)
  (cond
   ((null lat) nil)
   ((eq (car lat) old) (cons old (cons new (cdr lat))))
   (t (cons (car lat) (insertR new old (cdr lat))))
   )
  )


(defvar c3 '(a b c d f g h))

(insertR 'e 'd c3)

(defun insertL (new old lat)
  (cond
   ((null lat) nil)
   ((eq (car lat) old) (cons new lat))
   (t (cons (car lat) (insertR new old (cdr lat))))
   )
  )

(defun subst (new old lat)
  (cond
   ((null lat) nil)
   ((eq (car lat) old) (cons new (cdr lat)))
   (t (cons (car lat) (subst new old (cdr lat))))
   )
  )

(defvar c4 '(a b c d e))
(subst 'p 'b c4)

(defun subst2 (new o1 o2 lat)
  (cond
   ((null lat) nil)
   ((or (eq (car lat) o1) (eq (car lat) o2)) (cons new (cdr lat)))
   (t (cons (car lat) (subst2 new o1 o2 (cdr lat))))
   )
  )

(defvar c5 '(a d c b e))
(subst2 'p 'b 'd c5)

(defun multirember (a lat)
  (cond
   ((null lat) nil)
   ((eq (car lat) a) (multirember a (cdr lat)))
   (t (cons (car lat) (multirember a (cdr lat))))
   )
  )

(defvar c6 '(a b c d a e f g a i))
(multirember 'a c6)

(defun multiinsertR (new old lat)
  (cond
   ((null lat) nil)
   ((eq (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
   (t (cons (car lat) (multiinsertR new old (cdr lat))))
   )
  )

(defvar c7 '(a b c b d b e b))
(multiinsertR 'p 'b c7)

(defun multiinsertL (new old lat)
  (cond
   ((null lat) nil)
   ((eq (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
   (t (cons (car lat) (multiinsertL new old (cdr lat))))
   )
  )

(defvar c8 '(a b c b d b e b))
(multiinsertL 'p 'b c8)

(defun multisubst (new old lat)
  (cond
   ((null lat) nil)
   ((eq (car lat) old) (cons new (multisubst new old (cdr lat))))
   (t (cons (car lat) (multisubst new old (cdr lat))))
   )
  )

(defvar c9 '(a b c b d b e b))
(multisubst 'p 'b c9)

;; C4. Numbers Games

(defun add1 (n)
  (+ n 1)
  )

(defun sub1 (n)
  (- n 1)
  )

(atom? -4.3)
(add1 67)
(sub1 0)
(zerop 1)
(zerop 0)
(+ 19 87)

(defun o+ (n m)
  (cond
   ((zerop m) n)
   (t (o+ (add1 n) (sub1 m)))
   )
  )

(o+ 19 87)

(defun o- (n m)
  (cond
   ((zerop m) n)
   (t (o- (sub1 n) (sub1 m)))
   )
  )

(o- 100 10)



;;; tls.el ends here
