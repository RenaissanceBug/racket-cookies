#lang info

;; for net-cookies-test

(define collection 'multi)

(define pkg-desc "tests for \"net-cookies\"")

(define deps '("base"))

(define build-deps
  '("net-cookies-lib"
    "rackunit-lib"))

(define update-implies '("net-cookies-lib"))

(define version "1.1.3")
