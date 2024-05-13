#lang racket

(require (prefix-in http: net/http-easy)
         net/url
         html-parsing
         xml/path)

(provide crawl host=? delay-upto)

(define (url-without-fragment u)
  (struct-copy url u (fragment #f)))

(define (extract-links base-url xpr)
  (list->set (map url->string
                  (filter (lambda (u) (or (string=? (url-scheme u) "http") (string=? (url-scheme u) "https")))
                          (map (lambda (u) (url-without-fragment (combine-url/relative base-url u)))
                               (se-path*/list '(a @ href) xpr))))))

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
  (let crawl ((frontier (set url)) (visited (set)))
    (unless (or (set-empty? frontier) (and limit (>= (set-count visited) limit)))
      (if (not (wanted? (set-first frontier)))
          (crawl (set-rest frontier) visited)
          (let ((links (process (set-first frontier) handler))
                (visited (set-add visited (set-first frontier))))
            (delay)
            (crawl (set-union (set-rest frontier) (set-subtract links visited)) visited))))))
