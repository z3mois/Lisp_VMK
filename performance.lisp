;; основнаяфункция предобработки - возвращает список коэф. с переменными для 1 уравнения
(defun proc_1_uravnenie2 (uravnenie chisla operations bukva nakop flag)
    (cond ;((Null uravnenie) (cons (list (zagonxx (reverse chisla) (reverse operations) (reverse bukva))) nakop))
          ((Null uravnenie) (zagonxx (reverse chisla) (reverse operations) (reverse bukva) nakop) )
          ((eql (car uravnenie) '+) (cond ((Null flag) (proc_1_uravnenie2 (cdr uravnenie) Nil Nil Nil (zagonxx (reverse chisla) (reverse operations) (reverse bukva) nakop) flag))
                                          (T (proc_1_uravnenie2 (cdr uravnenie) '(-1) '(*) Nil (zagonxx (reverse chisla) (reverse operations) (reverse bukva) nakop) flag))))
          ((and (eql (car uravnenie) '-) (Null bukva) (Null chisla)) (proc_1_uravnenie2 (cdr uravnenie) '(-1) '(*) Nil nakop flag))
          ((eql (car uravnenie) '-) (cond ((Null flag) (proc_1_uravnenie2 (cdr uravnenie) '(-1) '(*) Nil  (zagonxx (reverse chisla) (reverse operations) (reverse bukva) nakop) flag))
                                          (T (proc_1_uravnenie2 (cdr uravnenie) Nil Nil Nil (zagonxx (reverse chisla) (reverse operations) (reverse bukva) nakop) flag))))
          ((eql (car uravnenie) '=)  (proc_1_uravnenie2 (cdr uravnenie) '(-1) '(*) Nil  (zagonxx (reverse chisla) (reverse operations) (reverse bukva) nakop) 'T))
          ((or (eql (car uravnenie) '*) (eql (car uravnenie) '/)) (proc_1_uravnenie2 (cdr uravnenie) chisla (cons (car uravnenie) operations) bukva nakop flag))
          ((numberp (car uravnenie)) (proc_1_uravnenie2 (cdr uravnenie) (cons (car uravnenie) chisla) operations bukva nakop flag))
          ((symbolp (car uravnenie)) (proc_1_uravnenie2 (cdr uravnenie) (cons 1 chisla) operations (cons (car uravnenie) bukva) nakop flag))
          (T (proc_1_uravnenie2 (cdr uravnenie) chisla operations bukva nakop flag))
    )
)
    
;; вызывает вычисление коэффициентов перед переменной
(defun zagonxx (chisla operations bukva res)
    (reverse (podstavit_1_skobku2 res (list (car bukva) (zagon2 (cdr chisla) operations bukva (car chisla))) Nil))
)

;; вычисляет коэффициенты перед переменной - например для обработки 2 * X / 3
(defun zagon2 (chisla operations bukva res)
    (cond ((and (Null operations) (Null chisla)) res)
          ((or  (Null operations) (Null chisla)) "Error") ;; add error here
          ((eql (car operations) '/) (zagon2 (cdr chisla) (cdr operations) bukva (/ res (car chisla)) )   ) 
          ((eql (car operations) '*) (zagon2 (cdr chisla) (cdr operations) bukva (* res (car chisla)) )   )
          ;;(T (eval ((quote (car operations)) '(car chisla) '(cadr chisla))))
          (t Nil)
    )
)

(defun predobrabotka_uravneniy2 (uravneniya) ;перевод во внутренний вид
    (cond ((Null uravneniya) Nil)
          (T (cons (proc_1_uravnenie2 (car uravneniya) Nil Nil Nil Nil Nil) (predobrabotka_uravneniy2 (cdr uravneniya))))
    )
)

(defun list_of_all_perem (uravneniya)
    (cond ((Null uravneniya) Nil)
          (T (iskluychit_povtory (append (iskluychit_povtory (poisk_perem_in_1_uravn (car uravneniya))) (list_of_all_perem (cdr uravneniya)))))
))

(defun poisk_perem_in_1_uravn (uravnenie)  ;includes povtory
    (cond ((Null uravnenie) Nil)
          ((or (eql (car uravnenie) '-) (eql (car uravnenie) '+) (eql (car uravnenie) '*)  (eql (car uravnenie) '/) (eql (car uravnenie) '=)) (poisk_perem_in_1_uravn (cdr uravnenie)))
          ((symbolp (car uravnenie)) (cons (car uravnenie) (poisk_perem_in_1_uravn (cdr uravnenie))))
          (T (poisk_perem_in_1_uravn (cdr uravnenie)))
))

(defun main (uravneniya) (main2 (vizov_podstanovki_vo_vse (predobrabotka_uravneniy2 uravneniya) Nil)))
(defun main2 (podstanovka_output) ;исключение уравнений 0 = 0                                                               !!!!!!!!
    (cond ((Null podstanovka_output) Nil)
          ((STRINGP podstanovka_output) "System has no solution!")                                                                                           ; here?
          ((Null (car podstanovka_output)) (main2 (cdr podstanovka_output)))
          (T (cons (car podstanovka_output) (main2 (cdr podstanovka_output))))
))
;----------------------------------------------------------------------------------------------------------------------------------------------------------------
;; спуск
(defun vizov_podstanovki_vo_vse (uravneniya res)
    (cond ;((Null uravneniya) Nil)
          ;((eql (length uravneniya) '1) (cons (check_ostatok (car uravneniya)) res))
          ((Null (car uravneniya)) res)                                                                                       ;!!!!!!!!!!!!!!!!!!!!!!!!
          ((STRINGP (virazhenie_peremennoy (car uravneniya)))  "System has no solution")
          ((eql (length uravneniya) '1) (cons ( virazhenie_peremennoy (car uravneniya)) res))
          (T (vizov_podstanovki_vo_vse (podstanovka_vo_vse (cdr uravneniya) (virazhenie_peremennoy (car uravneniya)))  (cons (virazhenie_peremennoy (car uravneniya)) res)))
    )
)

;; подставить переменную во все уравнения - возвращает уравнения
(defun podstanovka_vo_vse (uravneniya_bez_pervogo znachenie)
    (cond ((Null uravneniya_bez_pervogo) Nil)
          (T (cons (podstanovka (car uravneniya_bez_pervogo) znachenie) (podstanovka_vo_vse (cdr uravneniya_bez_pervogo) znachenie)))
    )
)

;; подставить переменную в 1 уравнение
(defun podstanovka (odno_uravnenie znachenie)
    (cond 
        ((and (Null (car znachenie)) (Null (cdr znachenie))) odno_uravnenie)            ;!!!!!!!!!!!!!!!!!!!!!!!!
        ;((and (Null (car znachenie)) (not (Null (cdr znachenie)))) "Error!")           ;?
        ;; если такая переменная есть, то делаем подстановку
        ((return_koef odno_uravnenie (car znachenie))
                            (summa_uravneniy (uravnenie_bez_podstavlyaemoy_peremennoy odno_uravnenie (car znachenie)) (znachenie_x_koef (cdr znachenie) (return_koef odno_uravnenie (car znachenie)) )))
        ;; иначе возвращаем уравнение неизменённым
        (T odno_uravnenie)
    )
)

(defun summa_uravneniy (uravnenie znachenie) ;сложение уравнений
    (cond ((Null znachenie) uravnenie)
          (T (summa_uravneniy (reverse (podstavit_1_skobku2 uravnenie (car znachenie) Nil)) (cdr znachenie)))
))

(defun podstavit_1_skobku2 (uravnenie skobka res) ;сложение 1 скобки
    (cond ((Null uravnenie) (cons skobka res))
          ((eql (caar uravnenie) (car skobka)) (cond ((will_not_be_zero uravnenie skobka) (append  (append (reverse (cdr uravnenie)) (list (list (caar uravnenie) (+ (cadar uravnenie) (cadr skobka)))) ) res))
                                                     (T (append (reverse (cdr uravnenie)) res))))
          (T (podstavit_1_skobku2 (cdr uravnenie) skobka (cons (car uravnenie) res) ))
))

(defun will_not_be_zero (uravnenie skobka) ;сумма не 0 (иначе надо убрать)
    (cond ((eql (+ (cadar uravnenie) (cadr skobka)) 0) Nil)
          (T T)))

(defun uravnenie_bez_podstavlyaemoy_peremennoy (uravnenie bukva)
    (cond ((Null uravnenie) Nil)
          ((eql (caar uravnenie) bukva) (cdr uravnenie))
          (T (cons (car uravnenie) (uravnenie_bez_podstavlyaemoy_peremennoy (cdr uravnenie) bukva)))
))

(defun znachenie_x_koef (uravnenie_bez_bukvy koef)
    (cond  ((Null uravnenie_bez_bukvy) Nil)
           (T (cons (list (caar uravnenie_bez_bukvy) (* (cadar uravnenie_bez_bukvy) koef))  (znachenie_x_koef (cdr uravnenie_bez_bukvy) koef)))
))

;; возвращает коэфф при перемемнной которую будем подставлять
(defun return_koef (uravnenie bukva)
    (cond ((Null uravnenie) Nil)
          ((eql (caar uravnenie) bukva) (cadar uravnenie))
          (T (return_koef (cdr uravnenie) bukva))
    )
)

(defun virazhenie_peremennoy (uravnenie)
    (cond ((and (Null (caar uravnenie)) (eql (cadar uravnenie) 0)) Nil)  ;ур-я типа X - X = 0 заменяются на Nil                   ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          ((and (eql (length uravnenie) '1) (Null (caar uravnenie)) (not (eql (cadar uravnenie) 0))) "Error!!")  ;ур-я типа 2 = 0   
          ((and (not (eql (length uravnenie) '1)) (Null (caar uravnenie))) (cons (caar (rokirovka uravnenie)) (podelit (cadar (rokirovka uravnenie)) (cdr (rokirovka uravnenie)))))
          (T (cons (caar uravnenie) (podelit (cadar uravnenie) (cdr uravnenie))))
))

(defun podelit (koef uravnenie)
    (cond ((Null uravnenie) Nil)
          (T (cons  (list (caar uravnenie) (/ (cadar uravnenie) (- koef)))  (podelit koef (cdr uravnenie))))
    )
)

(defun rokirovka (uravnenie)
    (cond ((Null uravnenie) Nil)
          ((eql (cdr uravnenie) Nil) uravnenie)
          (T (cons (cadr uravnenie) (cons (car uravnenie) (cddr uravnenie))))))
          
;----------------------------------------------------------------------------------------------------------------------------------------------------------------
(defun reshenie_systemy_uravneniy_s_svob_perem (uravneniya) 
    (cond ((STRINGP (main uravneniya)) "System has no solution!!!")
          ((Null (new_svobodnye_peremennye uravneniya)) (vizov_obratnoy_podstanovki_vo_vse (main uravneniya) Nil))
          (T (cons (cons "svobodnye peremennye" (new_svobodnye_peremennye uravneniya)) 
                                                                 (vizov_obratnoy_podstanovki_vo_vse (main uravneniya) Nil)))
))


;; подъём               
(defun vizov_obratnoy_podstanovki_vo_vse (uravneniya res)
    (cond ((Null uravneniya) res)
          (T (vizov_obratnoy_podstanovki_vo_vse (obratnaya_podstanovka_vo_vse (cdr uravneniya) (car uravneniya))  (cons (car uravneniya) res))) 
    )
)

;; подставить значение переменной во все уравнения - возвращает уравнения
(defun obratnaya_podstanovka_vo_vse (uravneniya_bez_pervogo znachenie)
    (cond ((Null uravneniya_bez_pervogo) Nil)
          (T (cons (obratnaya_podstanovka (car uravneniya_bez_pervogo) znachenie) (obratnaya_podstanovka_vo_vse (cdr uravneniya_bez_pervogo) znachenie)))
    )
)

;; подставить значение переменной в 1 уравнение
(defun obratnaya_podstanovka (odno_uravnenie znachenie)
    (cond 
        ;; если такая переменная есть, то делаем подстановку
        ((return_koef (cdr odno_uravnenie) (car znachenie)) (cons (car odno_uravnenie)
            (summa_uravneniy (uravnenie_bez_podstavlyaemoy_peremennoy (cdr odno_uravnenie) (car znachenie)) (znachenie_x_koef (cdr znachenie) (return_koef (cdr odno_uravnenie) (car znachenie)) )))) 
        ;; иначе возвращаем уравнение неизменённым
        (T odno_uravnenie)
))

(defun poisk_svobodnyx_peremennyx (resheniya) ;для всех скобок
    (cond ((Null resheniya) Nil)
          (T (append (poisk_svobodnyx_peremennyx_in_1_uravn (cdar resheniya)) (poisk_svobodnyx_peremennyx (cdr resheniya))))
))
 
(defun iskluychit_povtory (peremennye) ;if poisk_svobodnyx_peremennyx_in_1_uravn выдаёт (Z Z Z)
    (cond ((Null peremennye) Nil)
          ((member (car peremennye) (cdr peremennye)) (iskluychit_povtory (cdr peremennye)))
          (T (cons (car peremennye) (iskluychit_povtory (cdr peremennye))))
))

(defun poisk_svobodnyx_peremennyx_in_1_uravn (reshenie) ;для одной скобки
    (cond ((Null reshenie) Nil)
          ((eql (caar reshenie) Nil) (poisk_svobodnyx_peremennyx_in_1_uravn (cdr reshenie)))
          (T (cons (caar reshenie) (poisk_svobodnyx_peremennyx_in_1_uravn (cdr reshenie))))
))

(defun poisk_ne_svobodnyx_peremennyx (resheniya) ;наоборот не свободные переменные
    (cond ((Null resheniya) Nil)
          (T (cons (caar resheniya) (poisk_ne_svobodnyx_peremennyx (cdr resheniya))))
))

(defun raznost_mnozhestv (list1 list2 res)
    (cond ((Null list1) res)
          ((member (car list1) list2) (raznost_mnozhestv (cdr list1) list2 res))
          (T (raznost_mnozhestv (cdr list1) list2 (cons (car list1) res)))
))

(defun new_svobodnye_peremennye (uravneniya)
    (raznost_mnozhestv (list_of_all_perem uravneniya) (poisk_ne_svobodnyx_peremennyx (vizov_obratnoy_podstanovki_vo_vse (main uravneniya) Nil)) Nil))

;----------------------------------------------------------------------------------------------------------------------------------------------------------------
(defun pechat_peremennoy (virazhenie)
    (cond ((Null virazhenie) Nil)
          (T (cond ((Null (caar virazhenie)) (cond ((< (cadar virazhenie) 0) (cons (cadar virazhenie) (pechat_peremennoy (cdr virazhenie))))
                                                   (T (cons '+ (cons (cadar virazhenie) (pechat_peremennoy (cdr virazhenie)))))))
                   (T (cond ((< (cadar virazhenie) 0) (cons (cadar virazhenie) (cons (caar virazhenie) (pechat_peremennoy (cdr virazhenie)))))
                            (T (cons '+ (cons (cadar virazhenie) (cons (caar virazhenie) (pechat_peremennoy (cdr virazhenie))))))))))
))

(defun remove_plus (list)
    (cond ((Null list) Nil)
          ((eql (car list) '+) (cdr list))
          (T list)))

(defun pechat_peremennyx (reshenie)
    (cond ((Null reshenie) Nil)
          (T (cons (cons (caar reshenie) (cons '= (remove_plus (pechat_peremennoy (cdar reshenie))))) (pechat_peremennyx (cdr reshenie))))
))

(defun format_output (reshenie)
    (cond   ((Null reshenie) (print "All equations reduced"))
            ((Stringp reshenie) (print reshenie))
            ((Stringp (caar reshenie)) (print (cons (car reshenie) (pechat_peremennyx (cdr reshenie)))))
            (T (print (pechat_peremennyx reshenie)))
    )
)

(defun reshenie_systemy_uravneniy (uravneniya) (format_output (reshenie_systemy_uravneniy_s_svob_perem uravneniya)))

;----------------------------------------------------------------------------------------------------------------------------------------------------------------
;(reshenie_systemy_uravneniy '((X - X + Y - Y = 0) (2 * X + X = 3)))

;(print (predobrabotka_uravneniy2 '((X - 3 * Y + 6 * Z - 18 * K / 5 = -4 / 6)(X + 3 * Y = -5)) ))

;(print (poisk_perem_in_1_uravn '(U - U = 0)))

;(print  (predobrabotka_uravneniy2 '((X - 3 * Y + 6 * Z - 18 * K / 5 = -4 / 6)(X + 3 * Y + 7 * P = -5))) )

;(print (list_of_all_perem '((U - U = 0)(X - 3 * Y + 6 * Z - 18 * K / 5 = -4 / 6)(X + 3 * Y + 7 * P = -5)) ))

;(print (vizov_obratnoy_podstanovki_vo_vse (main '((U - U = 0)(X - 3 * Y + 6 * Z - 18 * K / 5 = -4 / 6)(X + 3 * Y + 7 * P = -5)) ) Nil ))

;(print (poisk_ne_svobodnyx_peremennyx '((X (Z -3) (K 9/5) (NIL -17/6) (P -7/2)) (Y (P -7/6) (NIL -13/18) (Z 1) (K -3/5)))))

;(print (raznost_mnozhestv '(U Z K X Y P) '(X Y) Nil))

;(print (new_svobodnye_peremennye '((U - U = 0)(X - 3 * Y + 6 * Z - 18 * K / 5 = -4 / 6)(X + 3 * Y + 7 * P = -5)) ))

(reshenie_systemy_uravneniy '((2 * X - Y = 4) (X + 3 * Y = -5)))

(reshenie_systemy_uravneniy '((- X + 8 * Y / 2 + Z = 2)(- X - Y - Z = -4)(2 * X + 6 * Y - 3 * Z = 0)))

(reshenie_systemy_uravneniy '((X - X + Y - Y  = 0) (Z - Z = 0) (3 * Z + Z = 20))) 

(reshenie_systemy_uravneniy '((X - X + Y - Y = 1)))

(reshenie_systemy_uravneniy '((Z = 2)(2 * Z - 4 = 0)))

(reshenie_systemy_uravneniy '((6 - 4 * X = 0) (Y + X = 2)))

(reshenie_systemy_uravneniy '((X - X = 0) (Y - Y = 0)))

(reshenie_systemy_uravneniy '((0 = 0)))

(reshenie_systemy_uravneniy '((6 * X + 3 * Y + Y - Y + 2 * Z + 2 * y = 2 * x)(X + 5 * Y + Z + 5 * x = 5 * x)(5 * X = -16 - Z)))

(reshenie_systemy_uravneniy '((0 * X + 7 + 0 = 12 + 3 * X / 2)))

