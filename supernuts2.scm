
;.................
;                .
;  SuperNuts II  .
;                .
;.................

; A simple random number generator written in Scheme.
; It can generate a number between 0.0 and 1.0,
; or a number between 0.0 and a specified inexact number 'n' (not inclusive),
; or an integer number between 0 and a specified exact number 'n' (not inclusive)

; WARNING: DO NOT USE SUPERNUTS II FOR SCIENTIFIC OR MATHEMATICAL APPLICATIONS
; NOT SUITABLE FOR MONTE-CARLO SIMULATIONS
; It may be suitable for non-critical applications, such as pseudo-randomness for computer games,
; or for graphical applications (eg. noise for dithering)

; Usage:

; Create a supernuts2 generator using the create-supernuts2-generator procedure, like so:
;  (define r (create-supernuts2-generator))
; Then you can use it like this for example:
;  (display r) (newline)
; You can save the current state of the generator like this:
;  (define saved-state (r 'get-seed))
; And you can restore the state like this:
;  (r 'set-seed saved-state)
; You can set the state of the generator yourself by supplying a number between 0.0 and 4294967295.0 like this:
;  (r 'set-seed 12345.6789)
; Or by supplying a list of two numbers between 0.0 and 4294967295.0 like this:
;  (r 'set-seed (list 123.4 567.8))

; Information:

;  create-supernuts2-generator:
;   Return value:
;    . A procedure, representing the new supernuts2 generator instance which has just been created.
;   Arguments supported:
;    . No argument given:
;     The state of the new generator instance is randomised based on the system time in seconds.
;    . A list containing two numbers (0.0 and 4294967295.0):
;     The state of the new generator instance is set to the values of the list provided.
;    . A number 'n' between 0.0 and 4294967295.0:	
;     Equivalent to passing '(list n 0.0)' as argument

;  supernuts2 generator instance procedure:
;   Return value can be any of the following, depending on argument given:
;    No argument given:
;     . A number between 0.0 and 1.0 (not inclusive)
;    An exact number 'n'
;     . An integer between 0 and n-1 inclusive
;    An inexact number 'n' 
;     . A number between 0.0 and n (not inclusive)
;    A string "get-seed" or a symbol 'get-seed:
;     . A list of two numbers between 0.0 and 4294967295.0 representing the current state
;    A string "get-proc" or a symbol 'get-proc:
;     . The supernuts2 procedure itself for this generator instance,
;       might be useful if execution speed is a concern
;       and you want to remove the extra overhead of
;       checking for arguments for every call
;    A string "set-seed" or a symbol 'set-seed:
;     Undefined, no return value
;
;  valid arguments for supernuts2 generator instance procedure:
;   An exact number
;   An inexact number 
;   String "get-seed" or symbol 'get-seed
;   String "set-seed" or symbol 'get-seed which can be followed by either
;    . a list of two numbers between 0.0 and 4294967295.0
;    . a single number between 0.0 and 4294967295.0
;   String "get-proc" or symbol 'get-proc

(define (create-supernuts2-generator . argument)
 (let ((n1 4117483646.0) (n2 4117484646.0) (lim 4294967295.0))
  ; this is used to obtain the current seed of this supernuts2 generator instance.
  ; it's called when a generator instance is called with symbol 'get-seed as its argument
  (define (sn2-get-seed) 
   (list n1 n2))
  ; this is used internally to validate seed values
  (define (sn2-validn? n)
   (and (>= n 0.0) (<= n lim)))
  ; this is used to set the state of this supernuts generator instance.
  ; it's called when a generator instance is called with symbol 'seed as its argument
  (define (sn2-seed argument)
   (cond
    ((number? argument)
     (if (sn2-validn? argument)
      (begin (set! n1 (exact->inexact argument)) (set! n2 0.0))
      (error (string-append "Seed number must be in range 0.0 to " (number->string lim)) "seed")))
    ((list? argument)
     (if (and (= 2 (length argument)) (sn2-validn? (car argument)) (sn2-validn? (cadr argument)))
      (begin (set! n1 (exact->inexact (car argument))) (set! n2 (exact->inexact (cadr argument))))
      (error "Invalid seed list" "seed")))
    (else (error (string-append "Seed value must be either a number between 0.0 and " (number->string lim)) "argument"))))
  ; this is the main core of supernuts2
  (define (fnr s)
   (set! s (* s 0.334676634894728348364971474961322998613979219379))
   (set! s (* (- s (truncate s)) 17761.666323247834363789693166846731410213886264229347))
   (let* ((a (* s 0.768567965987636759608412853505842308898385650814))
          (b (* s 0.411534687656783463715361117436596435037355834417))
          (v (* s 0.117897765443787942569497802054514193224765904051
                  (- a (truncate a)) (- b (truncate b)))))
    (- v (truncate v))))
  ; this is the supernuts2 random number generator
  (define (supernuts2)
   (set! n1 (+ n1 0.991743437879393982817103117921463949398269173764))
   (if (> n1 lim)
    (set! n1 (* 17616.76161761671671671 (- n2 (truncate n2)))))
   (set! n2 (+ n2 0.742624623477394783349389101027376677423781494347))
   (if (> n2 lim)
    (set! n2 (* 11617.17767167766167117 (- n1 (truncate n1)))))
   (let ((v  (+ (* (fnr n2) 1671.1761761) (* (fnr n1) 1161.717176))))
    (- v (truncate v))))
  ; if arguments were passed to 'create-supernuts2-generator', we expect it to be a seed value representing the initial state.
  ; otherwise, if no initial state is given, we will generate a state based on the current system time.
  (if (not (null? argument))
   (sn2-seed (car argument))
   (let ((t (current-time)))
    (set! n1 (* lim (fnr (* t 0.17173482593758925))))
    (set! n2 (* lim (fnr (* t 0.44167675765934236))))))
  ; the main body of a supernuts2 generator instance.
  (lambda (. argument)
   (if (null? argument)
    (supernuts2)
    (let ((cargument (car argument)))
     (if (string? cargument)
      (set! cargument (string->symbol cargument)))
     (cond
      ((symbol? cargument)
       (case cargument
        ('get-seed (sn2-get-seed))
        ('set-seed (sn2-seed (cadr argument)))
        ('get-proc supernuts2)))
      ((number? cargument)
       (if (exact? cargument)
        (inexact->exact (truncate (* (supernuts2) cargument)))
        (* (supernuts2) cargument)))
      (else (error "Invalid argument" "argument"))))))))
