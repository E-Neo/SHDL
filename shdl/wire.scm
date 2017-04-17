(define-module (shdl wire)
  #:use-module (srfi srfi-43)
  #:export (make-wire
            get-signal
            set-signal!
            add-action!
            make-wires
            get-signals
            set-signals!
            add-actions!))


(define (call-each procedures)
  (if (not (null? procedures))
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (make-wires num)
  (let ((wires (make-vector num)))
    (vector-map (lambda (i v) (make-wire))
                wires)))

(define (get-signals wires)
  (vector-map (lambda (i v) (get-signal v))
              wires))

(define (set-signals! wires new-values)
  (let ((wires-length (vector-length wires))
        (new-values-length (vector-length new-values)))
    (if (= wires-length new-values-length)
        (vector-map! (lambda (i v1 v2)
                       (set-signal! v1 v2)
                       v1)
                     wires new-values)
        (error "Mismatch length" wires-length new-values-length))))

(define (add-actions! wires action-procedures)
  (let ((wires-length (vector-length wires))
        (action-procedures-length (vector-length action-procedures)))
    (if (= wires-length action-procedures-length)
        (vector-map! (lambda (i v1 v2)
                       (add-action! v1 v2)
                       v1)
                     wires action-procedures)
        (error "Mismatch length" wires-length action-procedures-length))))
