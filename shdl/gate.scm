(define-module (shdl gate)
  #:use-module (shdl wire)
  #:use-module (shdl agenda)
  #:use-module (shdl logical)
  #:export (make-not-gate
            make-and-gate
            make-or-gate
            make-xor-gate))


(define (make-not-gate agenda not-gate-delay)
  (lambda (input output)
    (define (not-action)
      (let ((new-value (logical-not (get-signal input))))
        (after-delay agenda not-gate-delay
                     (lambda ()
                       (set-signal! output new-value)))))
    (add-action! input not-action)
    *unspecified*))

(define (make-and-gate agenda and-gate-delay)
  (lambda (input0 input1 output)
    (define (and-action)
      (let ((new-value (logical-and (get-signal input0)
                                    (get-signal input1))))
        (after-delay agenda and-gate-delay
                     (lambda ()
                       (set-signal! output new-value)))))
    (add-action! input0 and-action)
    (add-action! input1 and-action)
    *unspecified*))

(define (make-or-gate agenda or-gate-delay)
  (lambda (input0 input1 output)
    (define (or-action)
      (let ((new-value (logical-or (get-signal input0)
                                   (get-signal input1))))
        (after-delay agenda or-gate-delay
                     (lambda ()
                       (set-signal! output new-value)))))
    (add-action! input0 or-action)
    (add-action! input1 or-action)
    *unspecified*))

(define (make-xor-gate agenda xor-gate-delay)
  (lambda (input0 input1 output)
    (define (xor-action)
      (let ((new-value (logical-xor (get-signal input0)
                                    (get-signal input1))))
        (after-delay agenda xor-gate-delay
                     (lambda ()
                       (set-signal! output new-value)))))
    (add-action! input0 xor-action)
    (add-action! input1 xor-action)
    *unspecified*))
