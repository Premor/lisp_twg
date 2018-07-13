;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:twg-asd
  (:use :cl :asdf))

(in-package :twg-asd)

(asdf:defsystem :twg
  :name "twg"
  :version "0.0.0"
  :maintainer "T. God"
  :author "Desmon Table"
  :licence "BSD sans advertising clause (see file COPYING for details)"
  :description "twg"
  :long-description "Lisp implementation of our favorite ruminant"
  :depends-on (:sdl2)
  :components ((:file "t-sdl2.lisp"))
  )
