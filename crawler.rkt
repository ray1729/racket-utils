#lang racket

(require (prefix-in http: net/http-easy)
         net/url
         html-parsing
         xml/path)

(provide crawl host=? delay-upto)

(define (url-without-fragment u)
  (url->string (struct-copy url u (fragment #f))))

(define (extract-links url x)
  (list->set (map (lambda (u) (url-without-fragment (combine-url/relative url u)))
                  (se-path*/list '(a @ href) x))))

(define (process url handler)
  (match (http:get url)
    ((http:response #:status-code 200 #:headers ((content-type (regexp #"text/html"))) #:body body)
     (let ((x (html->xexp (bytes->string/utf-8 body))))
       (handler url x)
       (extract-links (string->url url) x)))
    (_ (set))))

(define (host=? host)
  (lambda (u) (string=? host (url-host (string->url u)))))

(define (delay-upto n)
  (lambda () (sleep (random n))))

(define (crawl url handler #:limit (limit #f) #:delay (delay (lambda () #f)) #:wanted? (wanted? (lambda (url) #t)))
  (let crawl ((todo (set url)) (seen (set)))
    (unless (or (set-empty? todo) (and limit (>= (set-count seen) limit)))
      (if (not (wanted? (set-first todo)))
          (crawl (set-rest todo) seen)
          (let ((links (process (set-first todo) handler))
                (seen (set-add seen (set-first todo))))
            (delay)
            (crawl (set-union (set-rest todo) (set-subtract links seen)) seen))))))
