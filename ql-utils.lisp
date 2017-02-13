;;;; ql-utils.lisp
;;
;; Quicklisp addon utils
;;

(defpackage #:ql-utils
  (:use #:cl)
  (:nicknames qlu)
  (:export bundle-dependencies list-all-dependencies))

(in-package #:ql-utils)

;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------


;;----------------------------------------------------------------------------
;; Implementation
;;----------------------------------------------------------------------------

;; The ql:bundle-systems is not aware of local systems
;; therefore in order to export all systems our local (newly implemented) system
;; depends on, need to implement the wrapper

(defun bundle-dependencies (system-name to)
  "Export all systems the SYSTEM-NAME depends on to the directory TO using
ql:bundle-systems. System should be quickloaded before."
  (let ((system (asdf/system:find-system system-name)))
    (unless system
      (error (format nil "System ~a is not found by ASDF" system-name)))
    (ql:bundle-systems (asdf/system:system-depends-on system) :to to)))


(defun list-all-dependencies (system-name)
  "Constructs a list of all dependencies (direct or indirect) of the SYSTEM-NAME.
System should be quickloaded before."
  (let (depends
        (stack (list system-name))
        (system (asdf/system:find-system system-name)))
    (unless system
      (error (format nil "System ~a is not found by ASDF" system-name)))
  (loop do
        (loop for dep in (asdf/system:system-depends-on (asdf/system:find-system (pop stack)))
              do (pushnew dep depends :test #'equal)
              (pushnew dep stack :test #'equal))
        while stack)
  depends))
