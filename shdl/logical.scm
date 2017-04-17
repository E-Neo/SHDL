(define-module (shdl logical)
  #:export (logical-not
            logical-and
            logical-or
            logical-xor))


(define (logical-not input)
  (cond ((= input 0) 1)
        ((= input 1) 0)
        (else (error "Invalid signal" input))))

(define (logical-and input0 input1)
  (cond ((and (= input1 0) (= input0 0)) 0)
        ((and (= input1 0) (= input0 1)) 0)
        ((and (= input1 1) (= input0 0)) 0)
        ((and (= input1 1) (= input0 1)) 1)
        (else (error "Invalid signal" input0 input1))))

(define (logical-or input0 input1)
  (cond ((and (= input1 0) (= input0 0)) 0)
        ((and (= input1 0) (= input0 1)) 1)
        ((and (= input1 1) (= input0 0)) 1)
        ((and (= input1 1) (= input0 1)) 1)
        (else (error "Invalid signal" input0 input1))))

(define (logical-xor input0 input1)
  (cond ((and (= input1 0) (= input0 0)) 0)
        ((and (= input1 0) (= input0 1)) 1)
        ((and (= input1 1) (= input0 0)) 1)
        ((and (= input1 1) (= input0 1)) 0)
        (else (error "Invalid signal" input0 input1))))
