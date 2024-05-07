#lang racket

(provide with-cwd
         with-env
         with-temporary-dir
         chomp
         run
         run/strings)

(define-syntax with-cwd
  (syntax-rules ()
    ((_ cwd body ...)
     (parameterize ((current-directory cwd))
       body ...))))

(define-syntax with-env
  (syntax-rules ()
    ((_ env body ...)
     (parameterize ((current-environment-variables (environment-variables-copy (current-environment-variables))))
       (for-each (lambda (v) (putenv (car v) (cdr v))) env)
       body ...))))

(define-syntax with-temporary-dir
  (syntax-rules ()
    ((_ body ...)     
     (let ((tmp (chomp (run "mktemp" "-d"))))
       (with-handlers (((lambda (v) #t)
                        (lambda (v)
                          (system* "/bin/rm" "-rf" tmp)
                          (raise v))))
         (let ((res (with-cwd tmp body ...)))
           (system* "/bin/rm" "-rf" tmp)
           res))))))

(define (chomp s)
  (string-trim s "\n" #:right? #t))

(define (read-lines port)
  (let read-lines ((next-line (read-line port)) (accum '()))
    (if (eof-object? next-line)
        (reverse accum)
        (read-lines (read-line port) (cons next-line accum)))))
    

(define (split-lines s)
  (string-split s #px"(\r\n|\n|\r)"))

(define (run cmd . args)
  (define-values (sp out in err) (apply subprocess #f #f #f (find-executable-path cmd) args))
  (define stdout (port->string out))
  (define stderr (port->string err))
  (close-output-port in)
  (close-input-port out)
  (close-input-port err)
  (subprocess-wait sp)
  (if (not (zero? (subprocess-status sp)))
      (raise-user-error 'run (format "error running ~a: ~a" cmd stderr))
      stdout))

(define run/strings (compose split-lines run))
