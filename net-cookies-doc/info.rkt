#lang info

;; for net-cookies-doc

(define collection 'multi)

(define pkg-desc
  "documentation part of \"net-cookies\"")

(define deps '("base"))

(define update-implies '("net-cookies-lib"))

(define build-deps
  '("net-cookies-lib"
    "racket-doc"
    "web-server-lib"
    "web-server-doc"
    "net-doc"
    "scribble-lib"
    ))

(define version "1.1.3")

(define license
  '(Apache-2.0 OR MIT))
