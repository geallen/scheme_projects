;with using binary search algorithm, finds how many times algorithm used to find the value in given list
#lang racket
(define binary_search
  (lambda (searched_value liste)
    (let loop ((lst liste)
               (a 1))
      (if (equal? #f (is_element searched_value lst)) #f
          (cond [(< (car searched_value) (car (mid_element lst))) (loop (first_half lst) (+ a 1))]
                [(> (car searched_value) (car (mid_element lst))) (loop (last_half lst) (+ a 1))]
                [else a ]))
      )))
    (define first_half 
    (lambda (lst)
            (take lst (quotient (length lst) 2))))
    
    (define last_half 
      (lambda (lst)
            (drop lst (quotient (length lst) 2))))
    
    (define mid_element 
    (lambda (lst)
            (let loop ((alist lst)
                       (blist (cdr lst)))
             (cond [(null? blist) (list (car alist))]
                   [(null? (cdr blist)) (list (car alist))]
                   [else (loop (cdr alist) (cddr blist))]))))
(define is_element
      (lambda (value lst)
        (cond [(null? lst) #f]
          [(equal? (car lst) (car value)) #t]
              
              [else (is_element value (cdr lst))])))