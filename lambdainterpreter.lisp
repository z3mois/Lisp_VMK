;;;; Функции для добавления объемлющих скобок и вывода ошибок

(defun add_brackets (Str)
    (let ((s (make-array '(0)
                         :element-type 'base-char
                         :fill-pointer 0
                         :adjustable t)))
        (with-output-to-string (sb s)
            (format sb "(")
            (format sb Str)
            (format sb ")")
        ) s)
)

(defun error_print (Exp Fou)
    (let ((s (make-array '(0)
                         :element-type 'base-char
                         :fill-pointer 0
                         :adjustable t)))
        (with-output-to-string (sb s)
            (format sb "Expected ")
            (format sb "~A" Exp)
            (format sb ", but ")
            (format sb "'~A'" Fou)
            (format sb " found.")
        ) s)
)





;;;; Проверка правильности лямбда выражения
;; Все функции в этом блоке возаращаются пару (правильность выражения . хвост, который осталось обработать) 

(defun check_apl (L)
    (cond ((null L) (print (error_print ">" "EOL")) (cons Nil Nil))   ; Не встретили апликацию после абстракции и перменной, всё плохо
          ((eq (car L) #\Space) (check_apl (cdr L)))   ; Пробелы пропускаем
          ((eq (car L) #\>) (cons T (cdr L)))   ; встретили апликацию, всё хорошо
          (T (print (error_print "'>'" (car L))) (cons Nil Nil))   ; Не встретили апликацию после абстракции и перменной, всё плохо
    )
)

(defun check_abstr (L)
    (cond ((null L) (print (error_print "variable" (car L))) (cons Nil Nil))   ; Мы не встретили переменной после абстракции. Всё плохо.
          ((eq (car L) #\Space) (check_abstr (cdr L)))   ; Пробелы пропускаем
          ((not (alpha-char-p (car L))) (print (error_print "variable" (car L))) (cons Nil Nil))   ; Мы не встретили переменной после абстракции. Всё плохо.
          ((check_apl (cdr L)))   ; Проверям, что после переменной есть апликация
    )
)

(defun check_var (L)
    (cond ((null L) (cons T L))   ; Корректные переменные закончились
          ((eq (car L) #\Space) (check_var (cdr L)))   ; Пробелы пропускаем
          ((eq (car L) #\/) (let ((CurVar (check_abstr (cdr L))))   ; Если встретили абстракцию, то проверям одну переменную и апликацию.
                            (let ((Tail (check_var (cdr CurVar))))   ; проверям дальше по рекурсии
                                    (cons (and (car CurVar) (car Tail)) (cdr Tail)))))    ; Проверям, что всё хорошо на этой абстракции и в хвосте
          ((cons T L))   ; Корректные переменные закончились
    )
)

(defun checking (L NumBr)
    (cond ((null L) (cond ((= NumBr 0) (cons T Nil))
                          (T (print (error_print ")" "EOL")) (cons Nil Nil))   ; Если конец строки, то проверям баланс скобок и выходим из рекурсии
                    )
          )
          ((eq (car L) #\Space) (checking (cdr L) NumBr))   ; Пробелы пропускаем
          ((eq (car L) #\() (let ((Vars (check_var (cdr L))))                 ; Если открывающаяся скобка, то сначала проверям абстракции и апликации
                            (let ((Body (checking (cdr Vars) (+ NumBr 1))))   ; Проверям все переменные в это парной скобке
                            (let ((Tail (checking (cdr Body) NumBr)))         ; Проверяем хвост после парной закрытой скобки (дальше идём по рекурсии)
                                    (cons (and (car Vars) (car Body) (car Tail)) (cdr Tail))))))    ; Проверям, что всё везде хорошо
          ((eq (car L) #\)) (cond ((= NumBr 0) (print (error_print "EOL" ")")) (cons Nil Nil))    ; Если закрытая скобка, то проверям баланс скобок и выходим из рекурсии
                                  ((cons T (cdr L)))
                            )
          )
          ((not (alpha-char-p (car L))) (print (error_print "variable" (car L))) (cons Nil Nil))   ; Если встретилось что-то кроме латинской буквы, то это неправильно
          ((checking (cdr L) NumBr))    ; Встретилась латинская буква, всё хорошо (дальше идём по рекурсии)
    )
)

(defun check_lambda (Str)
    (let ((ans (checking (coerce (add_brackets Str) 'list) 0)))  ; Вызов главной проверки
            (and (car ans) (null (cdr ans))))  ; Проверка, что всё хорошо и проверка прошла до конца
)







;;;; Перевод строки лямбда выражения в его внутреннее представление
;; Все функции в этом блоке возаращаются пару (Лямбда-выражение, переведенное во внутреннее представление . хвост, который осталось обработать)
;; Внутреенне представление такое: внутри одной скобки первый элемент в списке записаны все перменные, в хвосте записано тело лямбда-выражения

(defun get_var (L)
    (cond ((null L) L)   ; конец ввода, но она никогда не сработает, при этом елси сработает, то это неправильно
          ((eq (car L) #\Space) (get_var (cdr L)))   ; Пробелы пропускаем
          ((eq (car L) #\>) (cdr L))   ; встретили апликацию, конец перменной
          ((cons (car L) (get_var (cdr L))))   ; записываем перменную во внутреннее представление и добавляем хвост
    )
)

(defun parse_var (L)
    (cond ((null L) (cons Nil Nil))   ; Конец выражения
          ((eq (car L) #\Space) (parse_var (cdr L)))   ; Пробелы пропускаем
          ((eq (car L) #\/) (let ((CurVar (get_var (cdr L))))        ; если встретили абстракцию, то читаем перменную
                            (let ((Tail (parse_var (cdr CurVar))))   ; и запускаем рекурсию
                                    (cons (cons (car CurVar) (car Tail)) (cdr Tail)))))   ; записываем переменную во внутреннее представление и добавляем хвост
          ((cons Nil L))
    )
)

(defun parsing (L) 
    (cond ((null L) (cons Nil Nil))   ; Конец выржания
          ((eq (car L) #\Space) (parsing (cdr L)))   ; Пробелы пропускаем
          ((eq (car L) #\() (let ((Vars (parse_var (cdr L))))    ; Если очередная скобка, то в этой скобке разбираем переменные
                            (let ((Body (parsing (cdr Vars))))   ; и само тело лямбда-выражения (оставшееся в скобке)
                            (let ((Tail (parsing (cdr Body))))   ; запускаем рекурсию
                                    (cons (cons (cons (car Vars) (car Body)) (car Tail)) (cdr Tail))))))   ; записываем внутреннее представление + хвост, который осталось обработать
          ((eq (car L) #\)) (cons Nil (cdr L)))   ; конец парной скобки
          ((let ((Tail (parsing (cdr L))))   ; если просто символ, то запускаем рекурсию дальше
                (cons (cons (car L) (car Tail)) (cdr Tail))))   ; и записываем внетреннее представление
    )
)

(defun read_lambda (Str)
    (cons Nil (car (parsing (coerce (add_brackets Str) 'list))))   ; Отбрасываем хвост и приводим внешнюю скобку ко внутреннему предсталению
)






;;;; Перевод лямбда выражения из внутренного представлния в строку

(defun append_apl (L) (list #\/ L #\>))   ; Добавляем переменную

(defun append_br (L) (append (cons #\( L) '(#\))))   ; добавляем объемлющие скобки

(defun print_vars (L)
    (cond ((null L) Nil)   ; конец выражения
          ((null (cdr L)) (append_apl (car L)))   ; если последняя переменная, то просто доабвляем ее
          ((append (append_apl (car L)) (cons #\Space (print_vars (cdr L)))))   ; а если нет, то ещё добавляем пробел между ним и тем, что вернёт хвост рекурсии
    )
)

(defun print_body (L)
    (cond ((null L) Nil)   ; конец выражения
          ((null (cdr L)) (cond ((atom (car L)) L)   ; если последний символ, то просто доабвляем в список
                                ((append_br (print_br (car L))))   ; если скобка, то выводим то что внутри и добавляем объемлющие скобки.
                          )
          )
          (T (cond ((atom (car L)) (cons (car L) (cons #\Space (print_body (cdr L)))))   ; всё то же самое, только доавляем пробел
                   ((append (print_br (car L)) (cons #\Space (print_body (cdr L)))))
          ))
    )
)

(defun print_br (L)
    (cond ((null L) Nil)   ; конец выражения
          ((null (car L)) (append_br (print_body (cdr L))))   ; если нет переменных, то просто доабавляем тело в скобках
          ((null (cdr L)) (append_br (print_vars (car L))))   ; если нет тела, то просто давляем переменные в скобках
          ((append_br (append (print_vars (car L)) (cons #\Space(print_body (cdr L))))))   ; иначе добавляем и переменные и тело в скобках
    )
)

(defun print_lambda (L) 
    (cond ((atom L) (coerce (list L) 'string))   ; если просто одно выражение, то ничего не делать
          ((coerce (print_br L) 'string))   ; инчае запускаем перевод внутреннего представления в список и переводи этот список в строку
    )
)




;;;; Бета-редукция

(defun reduction (x y L)   ; простая подстановка элемнта вместо атома в S-выражениии
    (cond ((null L) Nil)
          ((listp L) (cons (reduction x y (car L)) (reduction x y (cdr L))))
          ((Eq x L) y)
          (L)
    )
)

(defun norm (L)
    (cond ((atom L) L)   ; конец рекурсии в глубь
          ((null (cdr L)) L)   ; конец рекурсии в одной скобке (конец рекурсии в ширину)
          ((and (null (car L)) (null (cddr L))) (norm (cadr L)))   ; если у нас нетпеременных и только что-то одно (буква или ещё одна скобка) внутри скобок, то убираем скобки
          ((atom (cadr L)) (cons (car L) (norm (cdr L))))   ; если мы рассматриваем букву, то не надо делать редукцию, идём дальше по телу внтутри скобки
          ((null (caadr L)) (norm (cons (car L) (append (cdadr L) (cddr L)))))   ; если мы рассматриваем скобки в котором нет переменных, то мы расскрываем эти скобки
          ((null (cddr L)) (cons (car L) (cons (norm (cadr L)) (cddr L))))   ; если мы рассматриваем скобки за которыми ничего нет, то мы просто идём внутрь скобок
          ((norm  (cons (car L) (cons (cons (cdaadr L) (reduction (caaadr L) (caddr L) (cdadr L))) (cdddr L)))))   ; если внутри скобок есть переменные и за скобками что-то есть, то делаем редукцию
    )
)

(defun applicative_br (L)   ; Запускает аппликативный порядок редукции для всех элементов списка 
    (cond ((null L) Nil)
          ((cons (applicative (car L)) (applicative_br (cdr L))))
    )
)

(defun applicative (L)
    (cond ((atom L) L)   ; конец рекурсии в глубь
          ((null (cdr L)) L)   ; конец рекурсии в одной скобке (конец рекурсии в ширину)
          ((and (null (car L)) (null (cddr L))) (applicative (cadr L)))   ; если у нас нет переменных и это последний элемент в скобке и только что-то одно (буква или ещё одна скобка) внутри скобок, то убираем скобки
          ((atom (cadr L)) (cons (car L) (applicative (cdr L))))   ; если мы рассматриваем букву, то не надо делать редукцию, идём дальше по телу внтутри скобки
          ((null (caadr L)) (applicative (cons (car L) (append (cdadr L) (cddr L)))))   ; если мы рассматриваем скобки в котором нет переменных, то мы расскрываем эти скобки
          ((null (cddr L)) (cons (car L) (cons (applicative (cadr L)) (cddr L))))   ; если мы рассматриваем скобки за которыми ничего нет, то мы просто идём внутрь скобок
          ((applicative  (cons (car L) (cons (cons (cdaadr L) (reduction (caaadr L) (applicative (caddr L)) (applicative (cdadr L)))) (applicative_br (cdddr L))))))
                ; если внутри скобок есть переменные и за скобками что-то есть, то делаем редукцию, но сначала рекрсивно идём в тело функции и все элементы за скобкой
    )
)


;; Нормальный порядок редукции
(defun intepreter (Str)
    (cond ((check_lambda Str) (print_lambda (norm (read_lambda Str)))))
)

;; Аппликативный порядок редукции
(defun intepreter_apl (Str)
    (cond ((check_lambda Str) (print_lambda (applicative (read_lambda Str)))))
)




(print (intepreter_apl "(/ x >)"))
(print (intepreter_apl "(/ x > x y w) (/z> z)"))
(print (intepreter_apl "(/x> x y) z w"))
(print (intepreter "(/x> /y> y) ((/z> z z)(/z> z z))"))
(print (intepreter "(/x> x) (/x> /y> /z> z x y) (/a> a) (/b> (/c>c) b) (/d> /e> e)"))
(print (intepreter_apl "(/x> x) (/x> /y> /z> z x y) (/a> a) (/b> (/c>c) b) (/d> /e> e)"))
(print (intepreter_apl "(/x> /y> x y (/a> /b> b)) (/c> /d> d) (/e> /f> f)"))
(print (intepreter_apl "(/x> /y> x y (/a> /b> b)) (/c> /d> d) (/e> /f> e)"))
(print (intepreter_apl "(/x> /y> x y (/a> /b> b)) (/c> /d> c) (/e> /f> f)"))
(print (intepreter_apl "(/x> /y> x y (/a> /b> b)) (/c> /d> c) (/e> /f> e)"))

