
; ##############
; # SUPERNUTS2 #
; ##############
; A simple PRNG
; author: dusthillresident@gmail.com https://github.com/dusthillresident/
;  Tested with guile, elk, scm, and 'Ikarus Scheme' interpreters.
;  Seems to pass 'dieharder' test suite
;  Has been tested up to 2TB in 'practrand' with no anomalies yet but further testing is needed.
;
;                                Functions defined:
;  supernuts2 --------------------------------------------------------------------------
;  ---- The supernuts2 PRNG. -----------------------------------------------------------
;  * Takes no arguments
;  * Returns a pseudorandom value between 0.0 and 1.0 (not inclusive)
;  supernuts2-seed ---------------------------------------------------------------------
;  ---- Set the state of the supernuts2 PRNG. ------------------------------------------
;  * Takes an argument 'seed' which can be either
;    . A list with two numbers in the range 0 to 7772345.67
;    . A number in the range in the range 0 to 7772345.67
;  * Returns nothing
;  supernuts2-get-seed -----------------------------------------------------------------
;  ---- Obtain the current state of the supernuts2 PRNG. -------------------------------
;  * Takes no arguments
;  * Returns a list containing two numbers in the range 0 to 7772345.67

(define supernuts2
 (let ((n1 0.0) (n2 0.0) (nlim 0.0) (lim 7772345.67)) ; n1 and n2 together constitute the current state
  (define (fnr s) ; this is the core of supernuts2
   (let* ((a (* s 0.768567965987636759608412853505842308898385650814))
          (b (* s 0.411534687656783463715361117436596435037355834417))
          (v (* s 0.117897765443787942569497802054514193224765904051
                  (- a (truncate a))
                  (- b (truncate b)))))
    (- v (truncate v))))
  (define (validn? n) (and (>= n nlim) (<= n lim))) ; check if seed values are valid
  (list 
; ---------- supernuts2 --------------------------
   (lambda ()
    (set! n1 (+ n1 0.991743537859395982857))
    (if (> n1 lim)
     (set! n1 (* lim (- n2 (truncate n2)))))
    (set! n2 (+ n2 0.73262452))
    (if (> n2 lim)
     (set! n2 (- n1 (truncate n1))))
    (let ((v (+ (fnr n2) (fnr n1))))
     (- v (truncate v))))
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
