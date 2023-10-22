;gnu clisp  2.49.60

(defun change_all2 (words)
    (mapcar (lambda (x) (coerce x 'list)) words)
)

(defun insert_word (word tree)
    (cond 
        ((null word) (cons (list '#\0) tree))
        ((null tree) (list(cons (car word) (list(insert_word (cdr word) tree)))))
        ((find_pls word tree) tree) ;можно убрать, если хочется оставить подсчет повторения слов в дереве (количество 0 на уровне)
        ((string> (car word) (caar tree)) (cons (car tree) (insert_word word (cdr tree))))
        ((eq (car word) (caar tree)) (cons (cons (caar tree) (list(insert_word (cdr word) (cadar tree)))) (cdr tree)))
        (T (cons (car(insert_word word ())) tree))
    )
)

(defun make_tree (words)
    (cond
        ((null (cdr words)) (insert_word (car words) '()))
        (T (insert_word (car words) (make_tree (cdr words))))
    )
)
;(print(make_tree(change_all2 (list "lol" "kek" "memes" "lel" "kel" "mem"))))

;((#\k ((#\e ((#\k ((#\0))))))) ((#\e ((#\l ((#\0))))) (#\l ((#\o ((#\l ((#\0))))))))(#\m ((#\o ((#\l ((#\0)))))))) 
;((#\k ((#\e ((#\k ((#\0))))))) ((#\l ((#\e ((#\l ((#\0))))))) (#\o ((#\l ((#\0))))))) 
;((#\k ((#\e ((#\k ((#\0))) (#\l ((#\0))))))) (#\l ((#\e ((#\l ((#\0))))) (#\o ((#\l ((#\0))))))) (#\m ((#\e ((#\m ((#\0) (#\e ((#\s ((#\0)))))))))))) 

(defun find_pls (word tree)
    (cond
        ((and (eq (car word) (caar tree)) (eq #\0 (car word))) T)
        ((null tree) NIL)
        ((eq (car word) (caar tree)) (find_key (cdr word) ( cadar tree)))
        (T (find_key word (cdr tree)))
    )
)
(defun find_key (word tree)
 (find_pls (append(coerce word 'list) '(#\0)) tree)
)

(defun preorder (node f)
  (when node
    (funcall f (first node))
    (preorder (second node) f)
    (preorder (third node)  f)))

(defun print_tree_N (n tree acc)
    (cond
        ((eq acc '0) (mapcar  (lambda (x) (print_tree_N (- n '1) x (+ acc 1))) tree))
        ((null tree) NIL)
        ((eq n '-1) NIL)
        (T 
       (print (car tree));probably add one more cond for #\0 at the end of the word + troubles with NIL
         (PRIN1 acc)
        (mapcar  (lambda (x) (print_tree_N (- n '1) x (+ acc 1))) (cadr tree))
       )
    )
)

(print(make_tree(change_all2 (list "lol" "kek" "memes" "lel" "kel" "mem"))))
(print(make_tree(change_all2 (list "mem" "memes" "mem" "kek" "mem" "memes"))))


(print(find_key "say" (make_tree(change_all2 (list "lol" "kek" "memes" "lel" "kel" "mem")))))
(print(find_key "lel" (make_tree(change_all2 (list "lol" "kek" "memes" "lel" "kel" "mem")))))
(print(find_key "meme" (make_tree(change_all2 (list "lol" "kek" "memes" "lel" "kel" "mem")))))

(print(print_tree_N 3 (make_tree(change_all2 (list "mem" "lol" "lel"))) 0))
(print '())
(print(print_tree_N 5 (make_tree(change_all2 (list "memes" "lol" "lel"))) 0))
