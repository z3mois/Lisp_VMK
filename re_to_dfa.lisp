(setq letters '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))

; возвращает стартовую вершину
(defun head (L)
    (car L)
)

;возвращает терминальную вершину
(defun tail (L)
    (cadr L)
)

; возвращает список ребер графа
(defun graph (L)
    (cddr L)
)


; конструктор графа (вершина_A переход по пустой строке вершина_B)
(defun epsilon (H S)
    (list H S (list H '#\e S)))
    
; функция изменения вершины_A
(defun change_node1 (L m n)
    (cond ((null L) nil)
          ((equal (caar L) m) (cons (list n (cadar L) (caddar L)) (change_node1 (cdr L) m n)))
          ((cons (car L) (change_node1 (cdr L) m n)))
    )
)

; конструктор графа (вершина_A переход по символу edge вершина_B)
(defun simple (H S edge)
    (list H S (list H edge S)))

; операция конкатенации
(defun concat (op1 op2)
    (cond ((null op1) op2)
           ((append (list (head op1) (tail op2)) (append (graph op1) (change_node1 (graph op2) (head op2) (tail op1)))))
    )
)

;операция или (|)
(defun unite (op1 op2 H S)
    (append (list H S) (graph (epsilon H (head op1))) (graph (epsilon H (head op2))) (graph op1) (graph op2) (graph (epsilon (tail op1) S)) (graph (epsilon (tail op2) S)))
)

; звезда Клини (для регулярки вида а+ отдельную функцию не делал, так как а+ -> аа*)
(defun iter (op H S)
    (append (list H S) (graph op) (graph (epsilon H (head op))) (graph (epsilon H S)) (graph (epsilon (tail op) S)) (graph (epsilon (tail op) (head op))))
)

; фунция выполнения операций, обработка регулярного выражение
(defun expr (L R)
    (cond ((null L) R)
          ((equal (cadr L) '*) (expr (cddr L) (concat R (iter (op (car L)) (random 5000) (random 5000)))))
          ((equal (cadr L) '+) (expr (cddr L) (concat R (concat (op (car L)) (iter (op (car L)) (random 5000) (random 5000))))))
          ((not (equal (car L) '\|)) (expr (cdr L) (concat R (op (car L)))))
          ((unite R (expr (cdr L) nil) (random 5000) (random 5000)) )
    )
)

; обработка операндов
(defun op (L)
    (cond ((atom L) (simple (random 5000) (random 5000) L))
          ((expr L nil))
    )
)

; возвращает алфавит - список всех симоволов, по которым возможен преход в графе
(defun get_sigm (L R)
    (cond ((null L) R)
          ((equal (cadar L) '#\e) (get_sigm (cdr L) R))
          ((member (cadar L) R) (get_sigm (cdr L) R))
          ((get_sigm (cdr L) (cons (cadar L) R)))
    )
)

; move (q a) - список всех вершин, в которые идут ребра по a из q
(defun move (L q a)
    (sort (move1 L q a '()) '<)
)

(defun move1 (L q a R)
    (cond ((null L) R)
          ((and (equal (caar L) q) (equal (cadar L) a)) (move1 (cdr L) q a (cons (caddar L) R)))
          ((move1 (cdr L) q a R))
    )
)

; epsclosure (q) - список всех вершин, до которых есть путь из q ТОЛЬКО по eps
(defun epsclosure (L q)
    (sort (epsclosure1 L L q '()) '<)
)

(defun epsclosure1 (L L1 q R)
    (cond ((null L) (cons q R))
          ((equal (caar L) q) (cond ((equal (cadar L) '#\e) (epsclosure1 L1 L1 (caddar L) (epsclosure1 (cdr L) L1 q R))) ((cons q R)) ))
          ((epsclosure1 (cdr L) L1 q R))
    )
)


; member, только лучше
(defun in (a L)
    (cond ((null L) nil)
          ((equal a (car L)) T)
          ((in a (cdr L)))
    )
)

; объединение двух множеств
(defun add (L1 L2)
    (add1 L1 L2 L2)
)

(defun add1 (L1 L2 R)
    (cond ((null L1) R)
           ((not (in (car L1) L2)) (add1 (cdr L1) L2 (cons (car L1) R)))
           ((add1 (cdr L1) L2 R))
    )
)

; здесь Q - это список вершин
(defun epsclosureQ (L q)
    (sort (epsclosureQ1 L q '()) '<)
)

(defun epsclosureQ1 (L q R)
    (cond ((null q) R)
          ((epsclosureQ1 L (cdr q) (add (epsclosure L (car q)) R)))
    )
)

; здесь Q - это список вершин
(defun moveQ (L q a)
    (sort (moveQ1 L q a '()) '<)
)

(defun moveQ1 (L q a R)
    (cond ((null q) R)
          ((moveQ1 L (cdr q) a (add (move L (car q) a) R)))
    )
)

; построение ДКА по НКА
;-----------------------------------------------------------
(defun convert (L)
    (convert1 L (epsclosure (graph L) (head L)))
)

(defun convert1 (L eps)
    (cond ((in (tail L) eps) (foo (graph L) (tail L) (list eps) (list eps) (list eps (list eps) '())))
          ((foo (graph L) (tail L) (list eps) (list eps) (list eps '() '())))
    )
)

(defun foo (L tail checked queue res)
    (cond ((null queue) res)
          ((apply #'foo (get_vertex (car queue) (get_sigm L '()) L tail checked (cdr queue) res)))
    )
)

(defun get_vertex (vertex alph L tail checked queue res)
    (cond ((null alph) (list L tail checked queue res))
          (T (setq v (moveQ L vertex (car alph))) (apply #'get_vertex (append (list vertex (cdr alph) L tail) (update_queue checked queue (epsclosureQ L v)) (list (update_graph L res vertex (car alph) v tail)))))
    )
)


(defun update_queue (checked queue e)
    (cond ((or (in e checked) (null e)) (list checked queue))
          ((list (cons e checked) (append queue (list e))))
    )
)


(defun update_graph (graph L a e b tail)
    (cond ((null b) L)
          ((append (list (car L) (append (cadr L) (check_terminal tail (cadr L) (epsclosureQ graph b))) ) (list (append (caddr L) (graph (simple a (epsclosureQ graph b) e))))))
    )
)

(defun check_terminal (q m L)
    (cond ((and (in q L) (not (in L m))) (list L))
          (nil)
    )
)

(defun vertex (L R)
    (cond ((null L) R)
          ((vertex (cdr L) (add (list (caar L) (caddar L)) R)))
    )
)
;-----------------------------------------------------------

; замена всех элементов e в списке на a
(defun subs (a e L)
    (cond ((null L) nil)
          ( (atom L) (cond ((equal L e) a) (L) ))
          ((equal (car L) e) (cons a (subs a e (cdr L))))
          ((cons (car L) (subs a e (cdr L))))
    )
)

; переименование вершин в графе для читаемости
(defun rename (L vertex alph)
    (cond ((null vertex) L)
          ((rename (append (cons (car (subs (car alph) (car vertex) (list (car L)))) (list (subs (car alph) (car vertex) (cadr L)))) (list (rename_g (car alph) (car vertex) (caddr L)))) (cdr vertex) (cdr alph)))
    )
)

(defun rename_g (alph vertex L)
    (cond ((null L) nil)
          ((cons (subs alph vertex (car L)) (rename_g alph vertex (cdr L))))
    )
)

; функция для запуска обработки регулярного выражения
(defun regex (L)
    (setq x (convert (op L)))
    (rename x (vertex (car (graph x)) '()) letters)
)

; Входные данные: все символы разделены пробелом. Предобработки данных нет: используется упрощенный вариант (см. примеры).
; Выходные данные: граф -> (начальная вершина терминальная вершина <список ребер>). <список ребер> -> (<ребра>). <ребра> -> <ребро> | <ребра>. <ребро> -> (вершина символ перехода вершина)
; Выход граф - где вершины помечены латинскими буквами
; (print (op '((#\a #\b) * #\a #\b *)))
; (print (op '((#\a #\b) * #\a * #\b #\a)))
; (print (op '(#\a +)))
; (setq L (convert (op '(#\a * \| #\b))))
(print (regex '(#\a * \| #\b #\a *)))
; (print (regex '((#\0 \| #\1) * #\0 #\1 #\0 (#\0 \| #\1))))
