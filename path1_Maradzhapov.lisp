(print "Задание 13.Определите функцию, удаляющую в исходном списке все повторные вхождение элементов.")
(defun F (L P)
 (cond
  ((null L) nil)
  ((null P) L)
  ((equal (car L) (car P))
   (F (cdr L) (cdr P)))
  (T
   (cons (car L) (F (cdr L) P)))))
 

 
(print (f '(1 2 3 4 1 2 3 4) '(1 2)))




(print "Задание 9.Определите функцию, разделяющую исходный список на два подсписка. В первый из них должны попасть элементы с нечетными номерами, во второй - 
элементы с четными номерами.")



(defun len (w n)
(cond
((null w) n)
((atom (car w)) (len (cdr w) (+ n 1)))
)
)

(defun % (b a)
(cond
(t (- b (* a (floor b a))))
)
)

(defun rev (lst lst1)
(cond
((Null lst) lst1)
(t (rev (cdr lst) (cons (car lst) lst)))
)
)

(defun split (w ls1 ls2)
(cond
((null w) (list (rev ls1 ()) (rev ls2 ())))
((eql (% (len w 0) 2) 1) (split (cdr w) (cons (car w) ls1) ls2))
(t (split (cdr w) ls1 (cons (car w) ls2)))
)
)

(print (split '(1 2 3 4 5) () ()))
(print (split () () ()))




(print "Задание 20.Определите функцию ПЕРВЫЙ-АТОМ, результатом которой будет первый атом списка.")

(defun f (x)
(cond ((Atom (car x)) (car x))
(t(f (car x)))))

(print(f '(((5) 6 7)4(1 2) 2 3)))



(print "Задание 30.Запрограммируйте интерпретатор ВЫЧИСЛИ, который преобразует инфиксную запись операций в префиксную и возвращает значение выражение.")
(defun inf-pref (x) (
                 cond
                     ((atom x) x)
                     (T (list (nth 1 x) (inf-pref (nth 0 x)) (inf-pref (nth 2 x))))
                 )
)

 (defun вычисли (x) (eval (inf-pref x)))


 (print (вычисли '((2 * 3) + (3 * 6))))



(print "Задание 14.Определите функцию, осуществляющую перестановку двух элементов списка с заданными номерами.")
(defun _reverse (lst lst1)
    (cond 
        ((Null lst)  lst1)
        (t (_reverse (cdr lst) (cons (car lst) lst1)))
    )
)

(defun my_find (lst n cnt) ; нахождение элемента
    (cond
        ((equal cnt n) (car lst))
        (t (my_find (cdr lst) n (+ cnt 1)))
        )
    )

(defun both (lst i1 i2) 
    (list (my_find lst i1 1) (my_find lst i2 1))
    )

(defun swap (lst output i1 i2 v1 v2 cnt)
    (cond
        ((null lst) (_reverse output ())) ; вывод
        ((equal cnt 0) (swap lst output i1 i2 (cadr (both lst i1 i2)) (car (both lst i1 i2)) (+ cnt 1) ) ) ; инициализация
        ((equal cnt i1) (swap (cdr lst) (cons v1 output) i1 i2 v1 v2 (+ cnt 1))) ; замена
        ((equal cnt i2) (swap (cdr lst) (cons v2 output) i1 i2 v1 v2 (+ cnt 1)))
        (t (swap (cdr lst) (cons (car lst) output) i1 i2 v1 v2 (+ cnt 1))) 
        )
    )

(print (swap '(8 7 5 3 2 1 4 6) () 3 5 0 0 0))



(print "Задание 33.Определите функцию МНОЖЕСТВО, преобразующую список в множество.")

(DEFUN MEMBER (X L)
    (COND
        ((ATOM L) NIL)
        ((EQUAL X (CAR L)) T)
        (T (MEMBER X (CDR L)))))
 
(DEFUN CREATE (L R)

    (COND 
        ((ATOM L) NIL)
         
        ((NOT (MEMBER (CAR L) R)) (CONS (CAR L) (CREATE (CDR L) (CONS (CAR L) R))))
        (T (CREATE (CDR L) R))))
 
 


(PRINT (CREATE '(a b a a c c) ()))


(print "Задание 35.Определите функцию ПОДМНОЖЕСТВО, которая проверяет, является ли одно множество подмножеством другого. Определите такое СОБСТВЕННОЕ-ПОДМНОЖЕСТВО.")

(defun my_member (val lst)
    (cond
        ((null lst) nil)
        ((eq val (car lst)) t)
        (t (my_member val (cdr lst)))
    )
)

(defun ПОДМНОЖЕСТВО (subset set)
    (cond
        ((null subset) t)
        ((my_member (car subset) set) (ПОДМНОЖЕСТВО (cdr subset) set))
        (t NIL)
    )
)

(print (ПОДМНОЖЕСТВО '() '(1 2 3)))
(print (ПОДМНОЖЕСТВО '(1 2) '(1 2 3)))
(print (ПОДМНОЖЕСТВО '(6) '(1 2 3)))