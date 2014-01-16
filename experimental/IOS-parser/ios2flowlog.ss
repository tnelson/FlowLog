#lang racket/base

; Simple wrapper for ios-compile.ss
;
; build me with:
; $ raco exe ios2flowlog.ss
;
; example for how to run on file IOS/talk-natfw.txt:
; $ ios2flowlog --path IOS talk-natfw.txt

(require racket/cmdline)
(require "ios-compile.ss")

(define path ".")
(define default-permit 'false)

(command-line
 #:program "ios2flowlog"
 #:once-each
 ("--default-permit" "Use a default permit ACL"
                     (set! default-permit 'true))
 ("--default-deny" "Use a default deny ACL [default]"
                   (set! default-permit 'false))
 ("--path" input "Directory path to IOS files [default: .]"
           (set! path input))
 #:args files
 (when (> (length files) 0)
     (compile-configurations path files default-permit)))