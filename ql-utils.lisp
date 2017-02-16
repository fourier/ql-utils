;;;; ql-utils.lisp
;;
;; Quicklisp addon utils
;;

(defpackage #:ql-utils
  (:use #:cl)
  (:nicknames qlu)
  (:export
   bundle-dependencies
   list-all-dependencies
   search-for
   info))

(in-package #:ql-utils)

;;----------------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------------


;;----------------------------------------------------------------------------
;; Implementation
;;----------------------------------------------------------------------------

(defun bundle-dependencies (system-name to)
  "Export all systems the SYSTEM-NAME depends on to the directory TO using
ql:bundle-systems. System should be quickloaded before."
;; The ql:bundle-systems is not aware of local systems
;; therefore in order to export all systems our local (newly implemented) system
;; depends on it was needed to implement the wrapper."
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


(defun search-for (some-name)
  "Wrapper around ql:system-apropos searching for SOME-NAME"
  (let ((systems (ql-dist:system-apropos-list some-name)))
    (mapcar
     (lambda (system)
       (let* ((name (ql-dist:name system))
              (url (get-system-page name)))
         (format nil "~a~@[ ~a~]" name url)))
     systems)))


(defun info (system-name)
  (let ((qdocs (concatenate 'string "http://quickdocs.org/" system-name "/")))
    (multiple-value-bind (response code)
        (drakma:http-request qdocs)
      ;; if the system found 
      (when (= 200 code)
        (let ((parsed (html-parse:parse-html response)))
          ;; html->body->div id "container"->div id "content"
          ;; :header
          (assoc :header (third (second (third (second parsed))))))))))


(defun get-system-page (system-name)
  (flet ((link-name (alist)
           (second alist))
         (link-url (alist)
           (third (car alist))))
  (let ((qdocs (concatenate 'string "http://quickdocs.org/" system-name "/")))
    (multiple-value-bind (response code)
        (drakma:http-request qdocs)
      ;; if the system found 
      (when (= 200 code)
        (let* ((parsed (html-parse:parse-html response))
               ;; html->body->div id "container"->div id "content"
               ;; :header -> header-links
               (links (cdr (fourth (assoc :header (third (second (third (second parsed))))))))
               (website-alist (find-if (lambda (x) (string= (link-name x) "Website")) links))
               (sourcecode-alist (find-if (lambda (x) (string= (link-name x) "Source Code")) links)))
          ;; the following heuristics applies:
          ;; if website button is available, use it
          ;; if source code button is available (probably leading to github page), use it
          ;; otherwise use quick docs url
          (cond (website-alist (link-url website-alist))
                (sourcecode-alist (link-url sourcecode-alist))
                (t qdocs))))))))
