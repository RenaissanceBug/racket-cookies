#lang racket/base

(require racket/contract/base
         racket/match
         racket/string)

(provide
 (contract-out
  [cookie-name? (-> any/c boolean?)]
  [cookie-value? (-> any/c boolean?)]
  [path/extension-value? (-> any/c boolean?)]
  [domain-value? (-> any/c boolean?)]))


;;;;;;;;; Cookie names ;;;;;;;;;

;; cookie-name? : Any -> Bool
;; true iff s is a token, per RFC6265; see below
(define cookie-name-re
  (pregexp (format "^[^~a]+$" (regexp-quote "()<>@,;:\\\"/[]?={} \t"))))

(define (cookie-name? s)
  (regexp-match? cookie-name-re s))

;; token          = 1*<any CHAR except CTLs or separators>
;; separator      = "(" | ")" | "<" | ">" | "@"
;;                | "," | ";" | ":" | "\" | <">
;;                | "/" | "[" | "]" | "?" | "="
;;                | "{" | "}" | SP | HT
;; see also RFC2616 Sec 2.2


;;;;;;;;; Cookie values ;;;;;;;;;

;; cookie-value? : Any -> Boolean
;; true iff x is a valid cookie value, per RFC6265. From the RFC:
;;   cookie-value    = *cookie-octet
;;                   / ( DQUOTE *cookie-octet DQUOTE )
;; where cookie-octet is defined below
(define (cookie-value? value)
  (match value
    [(regexp #rx"^\"(.*)\"" (list _ quoted-value))
     (cookie-value?* quoted-value)]
    [_
     (cookie-value?* value)]))

(define cookie-value-re
  (pregexp (format "^[~a0-9A-Za-z\\-]*$" (regexp-quote "()!#$%&'*+./:<=>?@[]^_{|}~"))))
(define (cookie-value?* value)
  (regexp-match? cookie-value-re value))

;; From the RFC:
;;      path-value = *av-octet
;;    extension-av = *av-octet
;; where av-octet is defined below.
(define (path/extension-value? x) ; : Any -> Boolean
  (and (string? x)
       (regexp-match? av-octets-re x)))

(define av-octets-re
  (pregexp (format "^[^~a]*$" (regexp-quote
                               (string-append
                                (apply
                                 string
                                 (cons
                                  (integer->char 127)
                                  (for/list ([code (in-range 0 32)])
                                    (integer->char code))))
                                "#\\;")))))


;; Per RFC1034.3.5 (with the RFC1123 revision to allow domain name
;;   components to start with a digit):
;; subdomain = label *("." label)
;; label     = ( ALPHA / DIGIT ) [ *ldh (ALPHA / DIGIT) ]
;; ldh       = ALPHA / DIGIT / "-"

(define domain-label-rx
  ;; Regexp matching one component of a domain name:
  #px"^[[:alnum:]][[:alnum:]-]*[[:alnum:]]$")

;; Test if dom is a valid domain name. From the RFC:
;;      domain-value = <subdomain>
;;                     ; as def'd in RFC1034 Sec 3.5
;;                     ; and enhanced by RFC1123 Sec 2.1
(define (domain-value? dom) ; Any -> Boolean
  (and (string? dom)
       (let ([parts (string-split dom "." #:trim? #f)])
         (and (not (null? parts))
              (for/and ([part parts])
                (regexp-match domain-label-rx part))))
       #t))

;;;; Underlying charsets

;; From the RFC:
;;      cookie-octet = <US-ASCII chars excluding CTLs, whitespace, DQUOTE,
;;                      comma, semicolon, and backslash>
;;          av-octet = <any CHAR except CTLs or #\;>
;;               CTL = ASCII octets 0-31 and 127

;; Charset used in cookie values includes the following chars:
;; ( ) ! # $ % & '  * + - . / 0 1 2 3 4 5 6 7 8 9 : < = > ? @ [ ] ^ _ `
;; { | } ~ A-Z a-z
