#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  (filter-map
   (lambda (filename)
     (and (equal? (path-extension filename) ".ss")
	  (not (equal? "build.ss" (path-strip-directory filename)))
	  (path-strip-extension filename)))
   (read-all (open-directory))))
     