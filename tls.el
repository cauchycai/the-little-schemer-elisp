;; C1. Toys
(defun atom? (x)
  (not (listp x)))

(defun zero? (n)
  (zerop n)
  )

(defun null? (l)
  (null l)
  )

(defun eq? (a1 a2)
  (eq a1 a2)
  )

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

(null? 'a)

(atom? '(abc efg))
(atom? (cdr '(abc)))

(setq a1 'Harry)
(setq a2 'HarryPotter)
(eq? a1 a2)

(setq l1 '(Harry))
(eq? l1 l1)

(setq n1 6)
(setq n2 6)
(eq? n1 n2)

;; C2. Do It, Do It Again, and Again, and Again

(defun lat? (l)
  (cond
   ((null? l) t)
   ((atom? (car l)) (lat? (cdr l)))
   (t nil)))

(defun member? (a lat)
  (cond
   ((null? lat) nil)
   (t (or (eq? (car lat) a) (member? a (cdr lat))))
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
   ((null? lat) nil)
   ((eq? (car lat) a) (cdr lat))
   (t (cons (car lat) (rember a (cdr lat))))
   )
  )

(defvar c1 '(v1 v2 v3 v4 v2))
(rember 'v2 c1)

(defun firsts (l)
  (cond
   ((null? l) nil)
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
   ((null? lat) nil)
   ((eq? (car lat) old) (cons old (cons new (cdr lat))))
   (t (cons (car lat) (insertR new old (cdr lat))))
   )
  )


(defvar c3 '(a b c d f g h))

(insertR 'e 'd c3)

(defun insertL (new old lat)
  (cond
   ((null? lat) nil)
   ((eq? (car lat) old) (cons new lat))
   (t (cons (car lat) (insertR new old (cdr lat))))
   )
  )

(defun subst (new old lat)
  (cond
   ((null? lat) nil)
   ((eq? (car lat) old) (cons new (cdr lat)))
   (t (cons (car lat) (subst new old (cdr lat))))
   )
  )

(defvar c4 '(a b c d e))
(subst 'p 'b c4)

(defun subst2 (new o1 o2 lat)
  (cond
   ((null? lat) nil)
   ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
   (t (cons (car lat) (subst2 new o1 o2 (cdr lat))))
   )
  )

(defvar c5 '(a d c b e))
(subst2 'p 'b 'd c5)

(defun multirember (a lat)
  (cond
   ((null? lat) nil)
   ((eq? (car lat) a) (multirember a (cdr lat)))
   (t (cons (car lat) (multirember a (cdr lat))))
   )
  )

(defvar c6 '(a b c d a e f g a i))
(multirember 'a c6)

(defun multiinsertR (new old lat)
  (cond
   ((null? lat) nil)
   ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
   (t (cons (car lat) (multiinsertR new old (cdr lat))))
   )
  )

(defvar c7 '(a b c b d b e b))
(multiinsertR 'p 'b c7)

(defun multiinsertL (new old lat)
  (cond
   ((null? lat) nil)
   ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
   (t (cons (car lat) (multiinsertL new old (cdr lat))))
   )
  )

(defvar c8 '(a b c b d b e b))
(multiinsertL 'p 'b c8)

(defun multisubst (new old lat)
  (cond
   ((null? lat) nil)
   ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
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
(zero? 1)
(zero? 0)
(+ 19 87)

(defun o+ (n m)
  (cond
   ((zero? m) n)
   (t (o+ (add1 n) (sub1 m)))
   )
  )

(o+ 19 87)

(defun o- (n m)
  (cond
   ((zero? m) n)
   (t (o- (sub1 n) (sub1 m)))
   )
  )

(o- 3 10)

(defun addtup (tup)
  (cond
   ((null? tup) 0)
   (t (o+ (car tup) (addtup (cdr tup))))
   )
  )

(addtup '(1 2 3 4 5))

(defun x (n m)
  (cond
   ((zero? m) 0)
   (t (o+ n (x n (sub1 m))))
   )
  )

(x 5 6)
(* 5 6.1)

(defun tup+ (tup1 tup2)
  (cond
   ((null? tup1) tup2)
   ((null? tup2) tup1)
   (t (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))
   )
  )

(tup+ '(1 2) '(4 3 2 1))

(defun o< (n m)
  (cond
   ((zero? m) nil)
   ((zero? n) t)
   (t (o< (sub1 n) (sub1 m)))
   )
  )

(o< 3 3)

(defun o> (n m)
  (cond
   ((zero? n) nil)
   ((zero? m) t)
   (t (o> (sub1 n) (sub1 m)))
   )
  )

(o> 3 3)

(defun o= (n m)
  (cond
   ((o< n m) nil)
   ((o> n m) nil)
   (t t)
   )
  )

(defun o2= (n m)
  (cond
   ((zero? n) (zero? m))
   ((zero? m) nil)
   (t (o2= (sub1 n) (sub1 m)))
   )
  )

(o= 3 3)
(o= 3 5)
(o2= 3 3)
(o2= 5 3)

(defun ↑ (n m)
    (cond
     ((zero? m) 1)
     (t (x n (↑ n (sub1 m))))
     )
  )

(↑ 2 3)
(↑ 5 3)
(expt 5 3)

(defun ÷ (n m)
  (cond
   ((o< n m) 0)
   (t (add1 (÷ (o- n m) m)))
   )
  )

(÷ 6 3)
(÷ 5 3)

(defun tls-length (lat)
  (cond
   ((null? lat) 0)
   (t (add1 (tls-length (cdr lat))))
   )
  )

(tls-length '(a b c d e))

(defun pick (n lat)
  (cond
   ;; ((zero? n) nil)
   ;; ((null? lat) nil)
   ((zero? (sub1 n)) (car lat))
   (t (pick (sub1 n) (cdr lat)))
   )
  )

(pick 3 '(a b c d e))

(defun rempick (n lat)
  (cond
   ((zero? (sub1 n)) (cdr lat))
   (t (cons (car lat) (rempick (sub1 n) (cdr lat))))
   )
  )

(rempick 3 '(a b c d e))

(defun number? (n)
  (numberp n)
  )

(number? 'a)

(defun no-nums (lat)
  (cond
   ((null? lat) nil)
   ((number? (car lat)) (no-nums (cdr lat)))
   (t (cons (car lat) (no-nums (cdr lat))))
   )
  )

(no-nums '(a 2 b c e 9))

(defun all-nums (lat)
  (cond
   ((null? lat) nil)
   ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
   (t (all-nums (cdr lat)))
   )
  )

(all-nums '(a 2 b c e 9))

(defun eqan? (a1 a2)
  (cond
   ((and (number? a1) (number? a2)) (o= a1 a2))
   ((or (number? a1) (number? a2)) nil)
   (t (eq? a1 a2))
   )
  )

(eqan? 1 2)
(eqan? 1 1)
(eqan? 1 'ss)
(eqan? 'ss 'ss)

(defun tls-occur (a lat)
  (cond
   ((null? lat) 0)
   ((eq? a (car lat)) (add1 (tls-occur a (cdr lat))))
   (t (tls-occur a (cdr lat)))
   )
  )

(tls-occur 1 '(1 2 a 4 1 d))
(tls-occur 'a '(1 2 a 4 1 d a b))

(defun one? (n)
  (zero? (sub1 n))
  ;; or
  ;; (o= n 1)
  )

(one? 1)
(one? 2)

(defun rempick2 (n lat)
  (cond
   ((one? n) (cdr lat))
   (t (cons (car lat) (rempick (sub1 n) (cdr lat))))
   )
  )

(rempick2 3 '(a b c d e))

;; C5. *Oh My Gawd*: It's Full of Stars

(defun rember* (a l)
  (cond
   ((null? l) nil)
   ((and (atom? (car l)) (eq? a (car l))) (rember* a (cdr l)))
   ((atom? (car l)) (cons (car l) (rember* a (cdr l))))
   (t (cons (rember* a (car l)) (rember* a (cdr l))))
   )
  )

(defun rember2* (a l)
  (cond
   ((null? l) nil)
   ((atom? (car l))
    (cond
     ((eq? a (car l)) (rember2* a (cdr l)))
     (t (cons a (rember2* a (cdr l))))
     ))
   (t (cons (rember* a (car l)) (rember* a (cdr l))))
   )
  )

(rember* 'cup '((coffie) cup ((tea) cup) (and (hick)) cup))
(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
(rember2* 'cup '((coffie) cup ((tea) cup) (and (hick)) cup))
(rember2* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))

(defun insertR* (new old l)
  (cond
   ((null? l) nil)
   ((atom? (car l))
    (cond
     ((eq? old (car l)) (cons (car l) (cons new (insertR* new old (cdr l)))))
     (t (cons (car l) (insertR* new old (cdr l))))
     ))
   (t (cons (insertR* new old (car l)) (insertR* new old (cdr l))))
   )
  )

(insertR* 'roast 'chuck '((how much (wood)) could
                          ((a (wood) chuck)) (((chuck)))
                          (if (a) ((wood chuck))) could chuck wood))


(defun occur* (a l)
  (cond
   ((null? l) 0)
   ((atom? (car l))
    (cond
     ((eq? a (car l)) (add1 (occur* a (cdr l))))
     (t (occur* a (cdr l)))
     ))
   (t (o+ (occur* a (car l)) (occur* a (cdr l))))
   )
  )

(occur* 'banana '((banana)
                  (split ((((banana ice)))
                          (cream (banana))
                          sherbet)) (banana)
                          (bread)
                          (banana brandy)))

(defun subst* (new old l)
  (cond
   ((null? l) nil)
   ((atom? (car l))
    (cond
     ((eq? old (car l)) (cons new (subst* new old (cdr l))))
     (t (cons (car l) (subst* new old (cdr l))))
     ))
   (t (cons (subst* new old (car l)) (subst* new old (cdr l))))
   )
  )

(subst* 'orange 'banana '((banana)
                          (split ((((banana ice))) (cream (banana))
                                  sherbet)) (banana)
                                  (bread)
                                  (banana brandy)))

(defun insertL* (new old l)
  (cond
   ((null? l) nil)
   ((atom? (car l))
    (cond
     ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
     (t (cons (car l) (insertL* new old (cdr l))))
     ))
   (t (cons (insertL* new old (car l)) (insertL* new old (cdr l))))
   )
  )

(insertL* 'pecker 'chuck '((how much (wood)) could
                           ((a (wood) chuck)) (((chuck)))
                           (if (a) ((wood chuck))) could chuck wood))

(defun member* (a l)
  (cond
   ((null? l) nil)
   ((atom? (car l)) (or (eq? a (car l)) (member* a (cdr l))))
   (t (or (member* a (car l)) (member* a (cdr l))))
   )
  )

(member* 'chips '((potato) (chips ((with) fish) (chips))))
(member* 'chips '((potato) (((with) fish) )))

(defun leftmost (l)
  (cond
   ((atom? (car l)) (car l))
   (t (leftmost (car l)))
   )
  )

(leftmost '((potato) (chips ((with) fish) (chips))))
(leftmost '(((hot) (tuna (and))) cheese))

;; (and ...) and (or ...) expressed by (cond ...)
;; (and a b) = (cond (a b) (t nil))
;; (or a b) = (cond (a t) (t b))

(defun eqlist? (l1 l2)
  (cond
   ((and (null? l1) (null? l2)) t)
   ((or (null? l1) (null? l2)) nil)
   (t (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
   )
  )

(defun eqlist1? (l1 l2)
  (cond
   ((and (null? l1) (null? l2)) t)
   ((or  (null? l1) (null? l2)) nil)
   ((and (atom? (car l1)) (atom? (car l2))) (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
   ((or (atom? (car l1)) (atom? (car l2))) nil)
   (t (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
   )
  )

(defun eqlist2? (l1 l2)
  (cond
   ((and (null? l1) (null? l2)) t)
   ((and (null? l1) (atom? (car l2))) nil)
   ((null? l1) nil)
   ((and (atom? (car l1)) (null? l2)) nil)
   ((and (atom? (car l1)) (atom? (car l2))) (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
   ((atom? (car l1)) nil)
   ((null? l2) nil)
   ((atom? (car l2)) nil)
   (t (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
   )
  )

(defun equal? (s1 s2)
  (cond
   ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
   ((or (atom? s1) (atom? s2)) nil)
   (t (eqlist? s1 s2))
   )
  )

(eqlist? '(a b c d e) '(a b c d e))
(eqlist? '(a b c d e) '(a b c d ))

(eqlist2? '(a b c d e) '(a b c d e))
(eqlist2? '(a b c d e) '(a b c d ))

;; rember for s-expression
(defun rember2 (s l)
  (cond
   ((null? l) nil)
   ((equal? s (car l)) (cdr l))
   (t (cons (car l) (rember2 s (cdr l))))
   )
  )

;; C6. Shadows

(defun numbered? (aexp)
  (cond
   ((atom? aexp) (number? aexp))
   ((or (eq? (car (cdr aexp)) (quote o+)) (eq? (car (cdr aexp)) (quote x)) (eq? (car (cdr aexp)) (quote ↑)))
    (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))
    )
   (t nil)
   )
  )

;; numbered? simplified
;; (defun numbered? (aexp)
;;   (cond
;;    ((atom? aexp) (number? aexp))
;;     (t (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
;;    )
;;   )

(numbered? '(3 o+ (4 x 5)))

(defun value1 (nexp)
  (cond
   ((atom? nexp) nexp)
   ((eq? (car (cdr nexp)) (quote o+)) (o+ (value1 (car nexp)) (value1 (car (cdr (cdr nexp))))))
   ((eq? (car (cdr nexp)) (quote x)) (x (value1 (car nexp)) (value1 (car (cdr (cdr nexp))))))
   (t (↑ (value1 (car nexp)) (value (car (cdr (cdr nexp))))))
   )
  )

(value1 4)
(value1 '(2 x 4))

(defun value2 (nexp)
  (cond
   ((atom? nexp) nexp)
   ((eq? (car nexp) (quote o+)) (o+ (value2 (car (cdr nexp))) (value2 (car (cdr (cdr nexp))))))
   ((eq? (car nexp) (quote x)) (x (value2 (car (cdr nexp))) (value2 (car (cdr (cdr nexp))))))
   (t (↑ (value2 (car (cdr nexp))) (value2 (car (cdr (cdr nexp))))))
   )
  )

(value2 '(x 2 4))

(defun 1st-sub-exp (aexp)
  (car (cdr aexp))
  )

(1st-sub-exp '(o+ 3 (x 4 5)))

(defun 2nd-sub-exp (aexp)
  (car (cdr (cdr aexp)))
  )

(2nd-sub-exp '(o+ 3 (x 4 5)))

(defun operator (aexp)
  (car aexp)
  )

(operator '(o+ 3 (x 4 5)))

(defun value (nexp)
  (cond
   ((atom? nexp) nexp)
   ((eq? (operator nexp) (quote o+)) (o+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
   ((eq? (operator nexp) (quote x)) (x (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
   (t (↑ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
   )
  )

(value '(x 2 4))

(defun sero? (n)
  (null? n)
  )

(defun edd1 (n)
  (cons (quote ()) n)
  )

(defun zub1 (n)
  (cdr n)
  )

(sero? '())
(sero? '(()))

;; this returns (nil) => problem for elisp
(edd1 '())

;; C7. Friends and Relations

(defun set? (lat)
  (cond
   ((null? lat) t)
   ((member? (car lat) (cdr lat)) nil)
   (t (set? (cdr lat)))
   )
  )

(set? '(a b a c d e))
(set? '(a b c d e))
(set? '(apple 3 pear 4 9 apple 3 4))

(defun makeset (lat)
  (cond
   ((null? lat) lat)
   ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
   (t (cons (car lat) (makeset (cdr lat))))
   )
  )

(makeset '(a b a c d e))

;; makeset written with multirember
;; (defun makeset (lat)
;;   (cond
;;    ((null? lat) lat)
;;    (t (cons (car lat) (makeset (multirember (car lat) (cdr lat)))))
;;    )
;;   )

(defun subset? (set1 set2)
  (cond
   ((null? set1) t)
   ;; ((null? set2) nil)
   (t (and (member? (car set1) set2) (subset? (cdr set1) set2)))
   )
  )

(subset? '(a b c) '(a c e b f))
(subset? '(a b c g) '(a c e b f))

(defun eqset? (set1 set2)
   (and (subset? set1 set2) (subset? set2 set1))
  )

(eqset? '(6 large chickens with wings) '(6 chickens with large wings))
(eqset? '(6 large chickens with wings) '(6 chickens with large))

(defun intersect? (set1 set2)
  (cond
   ((or (null? set1) (null? set2)) nil)
   (t (or (member? (car set1) set2) (intersect? (cdr set1) set2)))
   )
  )

(intersect? '(a b c d e) '(u t f g c m l))

(defun intersect (set1 set2)
  (cond
   ((or (null? set1) (null? set2)) nil)
   ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
   (t (intersect (cdr set1) set2))
   )
  )
(intersect '(a b c d e) '(u t f g c m l))

(defun union (set1 set2)
  (cond
   ((null? set1) set2)
   ((member? (car set1) set2) (union (cdr set1) set2))
   (t (cons (car set1) (union (cdr set1) set2)))
   )
  )
(union '(a b c d e) '(u t f g c m l))

;; my version
;; (defun intersectall (l-set)
;;   (cond
;;    ((null? l-set) nil)
;;    ((eq? (tls-length l-set) 1) (car l-set))
;;    (t (intersect (car l-set) (intersectall (cdr l-set))))
;;    )
;;   )

;; The version in the book
(defun intersectall (l-set)
  (cond
   ((null? (cdr l-set)) (car l-set))
   (t (intersect (car l-set) (intersectall (cdr l-set))))
   )
  )

(intersectall '((a b c) (c a d e) (e f g h a b)))

(defun a-pair? (x)
  (cond
   ((atom? x) nil)
   ((null? x) nil)
   ((null? (cdr x)) nil)
   (t (null? (cdr (cdr x))))
   )
  )

(a-pair? '((2) (pair)))
(a-pair? '((2) (pair) a))

(defun first (p)
  (car p)
  )

(defun second (p)
  (car (cdr p))
  )

(defun build (s1 s2)
  (cons s1 (cons s2 (quote ())))
  )

(first (build 'a 'b))
(second (build 'a 'b))

(defun third (l)
  (car (cdr (cdr l)))
  )

(third '(apples peaches pumpkin pie))

(defun fun? (rel)
  (set? (firsts rel))
  )

(fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))
(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))

(defun revrel (rel)
  (cond
   ((null? rel) nil)
   (t (cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel))))
   )
  )

(revrel '((8 a) (pumpkin pie) (got sick)))

(defun revpair (pair)
  (build (second pair) (first pair))
  )

(revpair '(8 a))

(defun revrel2 (rel)
  (cond
   ((null? rel) nil)
   (t (cons (revpair (car rel)) (revrel2 (cdr rel))))
   )
  )

(revrel2 '((8 a) (pumpkin pie) (got sick)))

(defun seconds (l)
  (cond
   ((null? l) nil)
   (t (cons (car (cdr (car l))) (seconds (cdr l))))
   )
  )

(seconds '(
           (a b c d)
           (e f g h)
           (i j k l)
           ))

(defun fullfun? (fun)
  (set? (seconds fun))
  )

(fullfun? '(
            (grape raisin)
            (plum prune)
            (stewed grape)
            ))

(defun one-to-one? (fun)
  (fun? (revrel fun))
  )

(one-to-one? '(
            (grape raisin)
            (plum prune)
            (stewed grape)
            ))


;;; tls.el ends here
