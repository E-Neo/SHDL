(use-modules (shdl agenda)
             (shdl wire)
             (shdl gate))


(define agenda (make-agenda))
(define not-gate (make-not-gate agenda 1))
(define and-gate (make-and-gate agenda 1))
(define or-gate (make-or-gate agenda 1))
(define xor-gate (make-xor-gate agenda 1))

(define (half-adder a b s c)
  (xor-gate a b s)
  (and-gate a b c))

(define (full-adder a b c-in s c-out)
  (let ((t1 (make-wire))
        (t2 (make-wire))
        (t3 (make-wire)))
    (xor-gate a b t1)
    (xor-gate t1 c-in s)
    (and-gate t1 c-in t2)
    (and-gate a b t3)
    (or-gate t2 t3 c-out)))

(define (4-bits-adder vec-a vec-b c0 vec-s c4)
  (let ((c1 (make-wire))
        (c2 (make-wire))
        (c3 (make-wire)))
    (full-adder (vector-ref vec-a 0) (vector-ref vec-b 0) c0
                (vector-ref vec-s 0) c1)
    (full-adder (vector-ref vec-a 1) (vector-ref vec-b 1) c1
                (vector-ref vec-s 1) c2)
    (full-adder (vector-ref vec-a 2) (vector-ref vec-b 2) c2
                (vector-ref vec-s 2) c3)
    (full-adder (vector-ref vec-a 3) (vector-ref vec-b 3) c3
                (vector-ref vec-s 3) c4)))

(define (2-to-1-multiplexer d0 d1 s y)
  (let ((t0 (make-wire))
        (t1 (make-wire))
        (t2 (make-wire)))
    (not-gate s t0)
    (and-gate d0 t0 t1)
    (and-gate s d1 t2)
    (or-gate t1 t2 y)))

(define (4-bits-not vec-x vec-y)
  (not-gate (vector-ref vec-x 0) (vector-ref vec-y 0))
  (not-gate (vector-ref vec-x 1) (vector-ref vec-y 1))
  (not-gate (vector-ref vec-x 2) (vector-ref vec-y 2))
  (not-gate (vector-ref vec-x 3) (vector-ref vec-y 3)))

(define (4-bits-additive-inverse vec-x vec-y)
  (let ((vec-t0 (make-wires 4))
        (vec-t1 (make-wires 4))
        (t0 (make-wire))
        (t1 (make-wire)))
    (4-bits-not vec-x vec-t0)
    (set-signals! vec-t1 #(1 0 0 0))
    (4-bits-adder vec-t0 vec-t1 t0 vec-y t1)))
