
; ##############
; # SUPERNUTS2 #
; ##############
; A simple PRNG
; author: dusthillresident@gmail.com https://github.com/dusthillresident/
;  Tested with guile, elk, scm, and 'Ikarus Scheme' interpreters.
;  Currently undergoing statistical testing with 'dieharder' and 'practrand'
;
;                                Functions defined:
;  supernuts2 --------------------------------------------------------------------------
;  ---- The supernuts2 PRNG. -----------------------------------------------------------
;  * Takes no arguments
;  * Returns a pseudorandom value between 0.0 and 1.0 (not inclusive)
;  supernuts2-seed ---------------------------------------------------------------------
;  ---- Set the state of the supernuts2 PRNG. ------------------------------------------
;  * Takes an argument 'seed' which can be either
;    . A list with two numbers in the range -2147483647.0 to 2147483647.0
;    . A number in the range in the range -2147483647.0 to 2147483647.0
;  * Returns nothing
;  supernuts2-get-seed -----------------------------------------------------------------
;  ---- Obtain the current state of the supernuts2 PRNG. -------------------------------
;  * Takes no arguments
;  * Returns a list containing two numbers in the range -2147483647.0 to 2147483647.0

(define supernuts2
 (let* ((nlim -2147483647.0) (lim 2147483647.0) (n1 nlim) (n2 nlim)) ; n1 and n2 together constitute the current state
  ; this is the main core of supernuts2
  (define (fnr s)
   (let* ((a (* s 0.7685679659876367))
          (b (* s 0.4115346876567834))
          (v (* s 0.1178977654437879
                  (- a (truncate a))
                  (- b (truncate b)))))
    (if (< v 0.0)
     (- (- v (truncate v)))
     (- v (truncate v)))))
  ; this is the core of supernuts2, stage 2
  (define (fnr2)
   (set! n1 (+ n1 1.0))
   (if (> n1 lim)
    (set! n1 nlim))
   (set! n2 (+ n2 0.73262452))
   (if (> n2 lim)
    (set! n2 nlim))
   (let ((v (+ (fnr n2) (fnr n1))))
    (- v (truncate v))))
  ; check if seed values are valid
  (define (validn? n) (and (>= n nlim) (<= n lim)))
  ; the externally defined procedures of supernuts2 are created here
  (list 
; ---------- supernuts2 --------------------------
   (lambda ()
    (let ((v (+ (truncate (* (fnr2) 65536.0)) (* (truncate (* (fnr2) 65536.0)) 65536.0))))
     (/ v 4294967296.0)))
; ---------- supernuts2-seed ---------------------
   (lambda (seed)
    (if (list? seed)
     ; scenario A: seed is a list representing the values of n1 and n2
     (begin
      (let ((errmsg (string-append
                     "Seed must be a list of two numbers between "
                     (number->string nlim)
                     " and "
                     (number->string lim))))
       (if (not (equal? (length seed) 2))
        (error errmsg "length"))
       (for-each
        (lambda (n) (if (not (validn? n)) (error errmsg (number->string n))))
        seed)
       (set! n1 (exact->inexact (list-ref seed 0)))
       (set! n2 (exact->inexact (list-ref seed 1)))))
     ; scenario B: seed is a number representing n1. n2 shall be reset to nlim
     (begin
      (if (not (validn? seed))
       (error (string-append
               "Seed value must be within the range "
               (number->string nlim)
               " to "
               (number->string lim))))
      (set! n1 (exact->inexact seed))
      (set! n2 nlim))))
; ---------- supernuts2-get-seed ---------------------
   (lambda ()
    (list n1 n2)))))

(define supernuts2-get-seed (list-ref supernuts2 2))
(define supernuts2-seed (list-ref supernuts2 1))
(set! supernuts2 (list-ref supernuts2 0))
