(defun bubble-sort-functional (list &key (key #'identity) (test #'<))
  "Функція сортування методом бульбашки, оптимізована для мінімізації викликів KEY."
  ;; Попередньо обчислюємо ключі для всіх елементів списку
  (let ((keyed-list (mapcar (lambda (x) (cons x (funcall key x))) list)))
    ;; Внутрішня функція для одного проходу сортування
    (labels ((single-pass (lst)
               (if (null (cdr lst))
                   lst
                   (let* ((first (car lst))          ;; Пара (елемент . ключ)
                          (second (cadr lst))       ;; Наступна пара
                          (key1 (cdr first))        ;; Ключ першого елемента
                          (key2 (cdr second)))      ;; Ключ другого елемента
                     (if (funcall test key2 key1)
                         ;; Міняємо місцями, якщо порядок неправильний
                         (cons second (single-pass (cons first (cddr lst))))
                         ;; Інакше залишаємо як є
                         (cons first (single-pass (cdr lst)))))))
             (recursive-sort (lst changes)
               (if (not changes)
                   lst
                   (let ((new-list (single-pass lst)))
                     (recursive-sort new-list (not (equal lst new-list)))))))
      ;; Виконуємо сортування й повертаємо список без ключів
      (mapcar #'car (recursive-sort keyed-list t)))))

(defun run-bubble-sort-test (input expected-result test-description &key (key #'identity) (test #'<))
  "Test function specifically for bubble-sort-functional with key and test parameters."
  (let ((result (bubble-sort-functional input :key key :test test)))
    (if (equalp result expected-result)
        (format t "~A: successfully.~%" test-description)
        (format t "~A: failed! ~%Expected: ~A~%Got: ~A~%" test-description expected-result result))))

(defun run-tests ()
  "Testing bubble-sort-functional with various cases."
  
  ;; Standard cases
  (run-bubble-sort-test '(7 1 9 3 2 4 6 5) '(1 2 3 4 5 6 7 9) "Standard case with random numbers")
  (run-bubble-sort-test '(10 20 30 40 50) '(10 20 30 40 50) "Already sorted in ascending order")
  (run-bubble-sort-test '(50 40 30 20 10) '(10 20 30 40 50) "Sorted in descending order")
  (run-bubble-sort-test '() '() "Empty input list")
  (run-bubble-sort-test '(7) '(7) "Single element list")
  (run-bubble-sort-test '(1 2 3 2 1 2 3) '(1 1 2 2 2 3 3) "List with repeated elements")
  
  ;; Key function tests
  (run-bubble-sort-test '(7 -3 -9 2 8 -4 5 -6 1) '(1 2 -3 -4 5 -6 7 8 -9)
                         "Sorting using absolute values"
                         :key #'abs)
  (run-bubble-sort-test '(7 -3 -9 2 8 -4 5 -6 1) '(-9 8 7 -6 5 -4 -3 2 1)
                         "Sorting with absolute values in descending order"
                         :key #'abs :test (function >))

  ;; Custom test functions
  (run-bubble-sort-test '(7 2 9 4 6) '(2 4 6 7 9) "Sorting in ascending order using <" :test (function <))
  (run-bubble-sort-test '(7 2 9 4 6) '(9 7 6 4 2) "Sorting in descending order using >" :test (function >))
  (run-bubble-sort-test '((5 . 3) (1 . 2) (8 . 5) (7 . 1))
                       '((1 . 2) (5 . 3) (7 . 1) (8 . 5))
                       "Sorting pairs by the first element"
                       :key #'car)
  (run-bubble-sort-test '((5 . 3) (1 . 2) (8 . 5) (7 . 1))
                       '((7 . 1) (1 . 2) (5 . 3) (8 . 5))
                       "Sorting pairs by the second element"
                       :key #'cdr)

  ;; String tests
  (run-bubble-sort-test (coerce "abcdef" 'list) (coerce "abcdef" 'list)
                      "Sorting string in ascending order"
                      :test (function char<))

  (run-bubble-sort-test (coerce "abcdef" 'list) (coerce "fedcba" 'list)
                      "Sorting string in descending order"
                      :test (function char>)))

(run-tests)


(defun duplicate-elements-fn (n &key (duplicate-p #'(lambda (x) t)))
#'(lambda (x)
    (if (funcall duplicate-p x)
        (mapcar #'(lambda (y) x) (make-list n))
        (list x))))

(defun test-duplicate-elements-fn ()
  "Tests for the duplicate-elements-fn function."
  
  (let ((test1 (mapcan (duplicate-elements-fn 2) '(1 2 3)))
        (test2 (mapcan (duplicate-elements-fn 2 :duplicate-p #'evenp) '(1 2 3 4 5)))
        (test3 (mapcan (duplicate-elements-fn 3 :duplicate-p #'oddp) '(1 2 3 4 5))))
    
    (if (equal test1 '(1 1 2 2 3 3))
        (format t "Test 1 passed: expected (1 1 2 2 3 3), got ~A~%" test1)
        (format t "Test 1 failed: expected (1 1 2 2 3 3), got ~A~%" test1))
    
    (if (equal test2 '(1 2 2 3 4 4 5))
        (format t "Test 2 passed: expected (1 2 2 3 4 4 5), got ~A~%" test2)
        (format t "Test 2 failed: expected (1 2 2 3 4 4 5), got ~A~%" test2))
    
    (if (equal test3 '(1 1 1 2 3 3 3 4 5 5 5))
        (format t "Test 3 passed: expected (1 1 1 2 3 3 3 4 5 5 5), got ~A~%" test3)
        (format t "Test 3 failed: expected (1 1 1 2 3 3 3 4 5 5 5), got ~A~%" test3))
    
    (format t "All tests completed.~%")))

(test-duplicate-elements-fn)