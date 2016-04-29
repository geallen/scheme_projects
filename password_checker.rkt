#lang racket

;run command (strongPassword 'name '(1 2 3 + * A g 8 4 Y e f 9 3 /))

(define strongPassword
  (lambda (username psw)
    (define s1 "Not suitable password!")
    (cond ((null? psw) s1)
          ((>= (numberOfList psw) 15)
           (if (and (equal? #t (upperCaseControl psw))
                (equal? #t (lowerCaseControl psw))
                (equal? #t (numberControl psw))
                (equal? #t (symbolControl psw)))
                      
               (add psw) s1))
          (else s1))))

 (define add
  (let ((the-list '()))
    (lambda (newPsw)
      (cond ((< (length the-list) 3)
             (set! the-list
               (append the-list (list newPsw)))
             the-list)
            (else
              (set! the-list (append (cdr the-list) (list newPsw)))
              the-list)))))

(define numberOfList
  (lambda (lst)
    (num  lst 0)))
(define num
  (lambda (lst count)
    (cond ((null? (cdr lst)) (+ 1 count))
          (else (num (cdr lst) (+ count 1))))))

(define upperCaseControl
  (lambda (lst)
    (define upperCaseLetters '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
    (cond ((null? (cdr lst)) #f)
          ((equal? #t (control lst upperCaseLetters)) #t)
          (else (upperCaseControl (cdr lst))))))

(define lowerCaseControl
  (lambda (lst)
    (define lowerCaseLetters '(a b c d e f g h i j  k l m n o p q r s t u v w x y z))
    (cond ((null? (cdr lst)) #f)
          ((equal? #t (control lst lowerCaseLetters)) #t)
          (else (lowerCaseControl (cdr lst))))))

(define numberControl
  (lambda (lst)
    (define numbers '(0 1 2 3 4 5 6 7 8 9))
    (cond ((null? (cdr lst)) #f)
          ((equal? #t (control lst numbers)) #t)
          (else (numberControl (cdr lst))))))

(define symbolControl
  (lambda (lst)
    (define symbols '(' ! ? $ % ^  & * _ - + = : @ ~ < > , \ / ))
    (cond ((null? (cdr lst)) #f)
          ((equal? #t (control lst symbols)) #t)
          (else (symbolControl (cdr lst))))))

(define control
  (lambda (lst1 lst2)
    (cond ((null? lst2) #f)
          ((equal? (car lst1) (car lst2)) #t)
          (else (control lst1 (cdr lst2))))))
