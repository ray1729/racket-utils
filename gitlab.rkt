#lang racket

(require net/url)
(require (prefix-in http: net/http-easy))
(require (only-in srfi/1 alist-cons))

(provide gitlab-client%)

(define (gitlab-token-auth token)
  (lambda (uri headers params)
    (values (hash-set headers 'private-token token) params)))

(define gitlab-client%
  (class object%
    
    (super-new)
    
    (init-field token
                (api-url (string->url "https://gitlab.com")))
    
    (define/public (url-for path-parts #:query [query-params '()])
      (struct-copy url api-url
               [query query-params]
               [path (map (lambda (p) (path/param p '())) (append (list "api" "v4") path-parts))]))

    (define/public (get path-parts #:query [query-params '()])
      (let* ([u (url-for path-parts #:query query-params)]
             [res (http:get u #:auth (gitlab-token-auth token))])
        (if (> (http:response-status-code res) 299)
            (raise-user-error 'gitlab-client/get (format "GET ~a: ~a" (url->string u) (http:response-status-message res)))
        res)))

    (define/public (get-paged path-parts #:query [query-params '((per_page . "100"))])
      (define (get-pages response accum)
        (let ([accum (append accum (http:response-json response))]
          [next-page (bytes->string/utf-8 (http:response-headers-ref response 'x-next-page))])
          (if (non-empty-string? next-page)
              (get-pages (get path-parts #:query (alist-cons 'page next-page query-params)) accum)
              accum)))
      (get-pages (get path-parts #:query query-params) '()))

    (define/public (list-descendant-groups group-id)
      (get-paged (list "groups" group-id "descendant_groups")))

    (define/public (list-projects group-id)
      (get-paged (list "groups" group-id "projects")))))