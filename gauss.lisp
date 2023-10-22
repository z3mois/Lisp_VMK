(defun gauss (system)
	(analizer (drop (forward system)))
)

(defun forward (system); прыямой проход
	(forward1 NIL system  0 T (list-length (car system)))
)

(defun forward1 (acc system pos flag cols)
    (cond (flag (myprint (append acc system))))
	(cond
		((>= pos (- cols 1)) (append acc system))
		((null system) acc)
        ((null (cdr system)) (append acc system))
        ((null (find_nonzero system pos)) (forward1 acc system (+ pos 1) NIL cols))
        ((not (eql 0 (find_nonzero system pos))) (forward2 acc (swap system 0 (find_nonzero system pos)) pos T cols))
        ((forward2 acc system pos NIL cols))
    )
)

(defun forward2 (acc system pos flag cols)
    (cond (flag (myprint (append acc system))))
    (forward1  (append acc (cons (car system) NIL))
                         (mapcar #'(lambda (x) (submul x (car system) (/ (getel x pos) (getel (car system) pos))))
                                 (cdr system)) (+ pos 1) T cols)
)
(defun backward (system); обратный проход
	(backward2 system (-(list-length system) 1) (car (last system)) NIL)
)

(defun backward1 (system pos acc)
	(backward2 system pos (car (last system)) acc)
)

(defun backward2 (system pos tail acc)
	(myprint (append system acc))
	(cond
		((eql pos 0) (backward3 (append system acc) 0))
		((backward1 
		(mapcar #'(lambda (x) (submul x tail (/ (getel x pos) (getel tail pos))))
                                 (droplast system)) (- pos 1) (cons tail acc)))
	)
)
(defun backward3 (acc pos)
    (cond
        ((null acc) NIL)
        ((cons (mul (car acc) (/ 1 (getel (car acc) pos))) (backward3 (cdr acc) (+ pos 1))))
    )
)

(defun getans (system); получает ответы по единичной правой матрице
    (myprint system)
    (mapcar #'last system)
) 
(defun droplast (system); система без последней строки
	(cond
		((null system) NIL)
		((null (cdr system)) NIL)
		((cons (car system) (droplast (cdr system))))
	)
)

(defun analizer (system); анализирует число решений
    (myprint system)
    (cond
        ((null system) (infsol system))
    	((reduce  #'(lambda (x y)(or x y)) (mapcar #'(lambda (x)(almostzero x)) system)) (zerosol))
    	((and (eql (list-length (car system)) (+(list-length system)1)) (nozerocols system) ) (singsol system))
    	((infsol system))
    )
)

(defun drop (system); удаляет нулевые строки
	(cond
		((null system) NIL)
		((fullzero (car system)) (drop (cdr system)))
		((cons (car system) (drop (cdr system))))
	)
)

(defun nozerocols (system); проверка на наличие нулевых столбцов
	(nozerocols1 system 0 (list-length (car system)))
)
(defun nozerocols1 (system pos len)
	(cond
		((>= pos (- len 1)) T)
		((not (find_nonzero system pos)) NIL)
		((nozerocols1 system (+ pos 1) len))

	)
)

(defun zerosol (); нет решений
	(print "No solutons!")
	NIL
)

(defun singsol(system); 1 решение
	(print "There is one solution!")
    (getans (backward system))
)

(defun infsol(system); бесконечно много решений
	(print "There is infinite number of solutions!")
    (print (sepvar system (list-length (car system)) 0 0))
    (cond
        ((null system) '("X_i = C_i")) 
	    ((infsol1 system (sepvar system (list-length (car system)) 0 0)))
))

(defun infsol1(system order)
	(infans (backward (sortbyorder system order)) order)
)
(defun infans(solution order); вывод общего решения
    (myprint solution)
    (mapcar #'(lambda (x)(  cons (caaar x) (cons (cadaar x) (cdr(reduce 'append x)))     ))
        (zip (zip (mapcar #'(lambda (y)(cons (cons 'X (+ 1 y)) (cons '= ()) ))(depend order (list-length solution)))
    (mapcar #'(lambda (y) (cons (bias y (list-length solution))NIL)) solution))
    (mapcar #'(lambda (z) (zip z (mapcar #'(lambda (q)(cons (cons 'X (+ 1 q)) ()))(free order (list-length solution))))) (mapcar #'(lambda (y) (mapcar '-(free y (list-length solution)))) solution))
)))

(defun free(order n) ;своюбодные переменные
    (cond
        ((eql n -1) order)
        ((free (cdr order) (- n 1)))
    )
)

(defun depend (order n) ;зависимые переменные
    (cond
        ((eql n 0) NIL)
        ((cons (car order) (depend (cdr order) (- n 1))))
    )
)
(defun bias (order n); свободный член 
    (cond
        ((eql n 0) (car order))
        ((bias (cdr order) (- n 1)))
    )
)

(defun sepvar (system rows cur pos); определяем перестановку
    (cond
        ((>= pos rows) NIL)
        ((>= cur (list-length system)) (append (sepvar system rows cur (+ pos 1)) (cons pos NIL)))
        ((null (eql (getel (getel system cur) pos) 0)) (cons pos (sepvar system rows (+ cur 1) (+ pos 1))))
        ((append (sepvar system rows cur (+ pos 1)) (cons pos NIL)))
    )
)

(defun arange (a b); список с числами от a до b
	(cond
		((eql a b) NIL)
		((cons a (arange (+ a 1) b)))
	)
)

(defun sortbyorder (system order); сортировка по индексам отсортированного массива
    (mapcar #'(lambda (x) (mapcar #'car(sort (zip x (mapcar #'cdr (sort (zip order (arange 0 (list-length order))) #'< :key #'car))) #'< :key #'cdr) )) system)
)

(defun count_nonzeros(system i); колво ненулевых эементов в i столбце
    (reduce #'+ (mapcar #'(lambda (x)(cond 
                                          ((eql(getel x i) 0) 0)
                                          (1)
                                      )) system))
)
(defun zip(row order); список точечных пар
    (cond
        ((null row) NIL)
        ((cons (cons (car row) (car order)) (zip (cdr row) (cdr order))))
    )
)

(defun fullzero (row); все нули
    (reduce #'(lambda (x y)( and x y)) (mapcar #'(lambda (x)(eql x 0)) row))
)

(defun almostzero (row); все нули, правая часть нет
    (and (not (eql (car (reverse row)) 0)) (fullzero (cdr (reverse row))))
)
(defun myprint (system) ;вывод для матрицы 
    (print "--------------------------------------")
    (mapcar 'print system)
    
)

(defun mul (row a) ;умножеие строки на число
	(cond
		((null row) NIL)
		((cons (* a (car row)) (mul (cdr row) a)))
	)
)

(defun submul (row1 row2 a) ;вычитание строки умноженной на число 
	(cond
		((null row1) NIL)
		((cons (- (car row1) (* a (car row2))) (submul (cdr row1) (cdr row2) a)))
	)
)

(defun swap (system i j) ;перестановка строк
	(cond
		((eql i j) system)
	    ((swap1 system i j 0 (getel system i) (getel system j)))
	)
)

(defun swap1 (system i j pos x y)
	(cond
		((null system) NIL)
		((eql pos i) (cons y (swap1 (cdr system) i j (+ pos 1) x y)))
		((eql pos j) (cons x (swap1 (cdr system) i j (+ pos 1) x y)))
		((cons (car system) (swap1 (cdr system) i j (+ pos 1) x y)))
	)
)

(defun getel (system i) ;взятие элемента по индексу
	(getel1 system i 0)
)

(defun getel1 (system i pos)
	(cond
		((eql i pos) (car system))
		((getel1 (cdr system) i (+ pos 1)))
	) 
)

(defun find_nonzero(system i) ;поиск ненулевого элемента в i столбце
    (find_nonzero1 system 0 i)
)

(defun find_nonzero1(system pos i)
	(cond
		((null system) NIL)
		((eql (getel (car system) i) 0) (find_nonzero1 (cdr system) (+ pos 1) i))
		(pos)
	)
)
   
(myprint(gauss '(
                 (0 0 0 0 1 1 2)
                 (0 0 1 1 0 0 4)
                 (1 1 0 0 0 0 6)
                 )))
(print "////////////////////////////////////")
