#lang racket

(require net/url
         (prefix-in http: net/http-easy)
         (only-in srfi/1 alist-cons))

(provide make-client
         api-url
         get
         get-paged
         list-descendant-groups
         list-group-projects
         list-groups)

(struct client (url token))

(define (gitlab-token-auth token)
  (lambda (uri headers params)
    (values (hash-set headers 'private-token token) params)))

(define (make-client #:url (url (string->url "https://gitlab.com")) #:token token)
  (client url token))

(define (api-url c path-parts (query-params '()))
  (struct-copy url
               (client-url c)
               (query query-params)
               (path (map (lambda (p) (path/param p '())) (append (list "api" "v4") path-parts)))))

(define (get c path-parts (query-params '()))
  (let* ((u (api-url c path-parts query-params))
         (res (http:get u #:auth (gitlab-token-auth (client-token c)))))
    (if (>= (http:response-status-code res) 300)
        (raise-user-error 'gitlab/get (format "GET ~a: ~a" (url->string u) (http:response-status-message res)))
        res)))

(define (ensure-per-page params page-size)
  (if (assoc 'per_page params)
      params
      (alist-cons 'per_page page-size params)))

(define (get-paged c path-parts (query-params '()))
  (let ((query-parms (ensure-per-page query-params "100")))
    (define (get-pages response accum)
      (let ((accum (append accum (http:response-json response)))
            (next-page (bytes->string/utf-8 (http:response-headers-ref response 'x-next-page))))
        (if (non-empty-string? next-page)
            (get-pages (get c path-parts (alist-cons 'page next-page query-params)) accum)
            accum)))
    (get-pages (get c path-parts query-params) '())))

(define (list-descendant-groups c group-id)
  (get-paged c (list "groups" group-id "descendant_groups")))

(define (list-group-projects c group-id)
  (get-paged c (list "groups" group-id "projects")))

(define (list-groups c (params '()))
  (get-paged c (list "groups") params))

