(define (list-ref l n)
  (if (= n 0)
      (car l)
      (list-ref ((cdr l)) (- n 1))))

(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l))
            (lambda ()
              (map f ((cdr l)))))))

(define (zip f l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        (else (cons (f (car l1) (car l2))
                    (lambda ()
                      (zip f ((cdr l1)) ((cdr l2))))))))

(define ones
  (cons 1
        (lambda ()
          ones)))

(define integers
  (cons 1
        (lambda ()
          (zip + ones integers))))

(define (double x) (* 2 x))

(list-ref (map double integers) 0)
(list-ref (map double integers) 1)
(list-ref (map double integers) 2)
(list-ref (map double integers) 3)
(list-ref (map double integers) 4)
(list-ref (map double integers) 5)
(list-ref (map double integers) 6)
(list-ref (map double integers) 8)
