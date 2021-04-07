(defun rev (x &optional (res '()))
    (cond
        ((null x) res)
        (t (rev (cdr x) (cons (car x) res)))
    )
)
(print "Задание 1. Определите FUNCALL через функционал APPLY.")

(defun FUNCALL (f &rest arglist)
   (apply f arglist))
 

 
(print (FUNCALL '+ 1 2 3 4))
(print (FUNCALL 'cadr '(1 2 3 4)))



(print "Задание 3. Определите функционал (APL-APPLY f x), который применяет каждую функцию fi списка (f1 f2 ... fn)
к соответствующему элементу спискаx = (x1 x2 ... xn) и воявращает список, сформированный из результатов.")

(defun APL-APPLY (f args &optional (res '()))
    (cond
        ((NULL args) (rev res))
        ((NULL f) nil)
        (t (APL-APPLY (cdr f) (cdr args) (cons (APPLY (car f) (car args)) res ) ) )
    )
)


(print (APL-APPLY '(+ - * /) '((1 2) (3 4) (5 6) (7 8))))
(print (APL-APPLY '(car cdr ATOM) '( ((1 2)) ((3 4)) ((5 6)))))
(print (APL-APPLY '() '((1 2) (3 4) (5 6) (7 8))))
 
(print "Задание 5. Определите функциональный предикат (НЕКОТОРЫй пред список), который истинен, когда,
       являющийся функциональным аргументом предикат ПРЕД истинен хоть бы для одного элемента списка СПИСОК.")

(defun НЕКОТОРЫЙ (пред список)
    (cond
        ((NULL список) nil)
        ((APPLY пред (car список)) t)
        (t (НЕКОТОРЫЙ пред (cdr список)))
    )
)

(print (НЕКОТОРЫЙ 'ATOM '((a) ()) ) )
(print (НЕКОТОРЫЙ 'ATOM '( ((a b)) ((1 2)) ) ) )
(print (НЕКОТОРЫЙ 'equal '( (1 2) (3 3)) ))



(print "Задание 11. Определите фукнционал МНОГОФУН, который использует функции, являющиеся
аргументами, по следующей схеме: (МНОГОФУН ’(f g ... h) x) ⇔ (LIST (f x) (g x) ... (h x)).")


(defun МНОГОФУН (f x &optional (res '()))
    (cond
        ((NULL f) (rev res))
        (t (МНОГОФУН (cdr f) x (cons (list (APPLY (car f) x)) res)))
        )
)


(print (МНОГОФУН '(+ - *) '(4 3 4)))
(print (МНОГОФУН '(car cadr atom) '((4 3 2 1)) ))
(print (МНОГОФУН '() '(1 2)))
