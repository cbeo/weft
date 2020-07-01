(defpackage #:weft.supported
  (:use #:cl)
  (:import-from #:weft
                #:unify
                #:implies-p))

(in-package #:weft.supported)

(defstruct supported value support)
;; and define UNIFY for supported types.

;; (defmethod unify ((supported1 supported) (supported2 supported))
;;   (let ((val1 (supported-value supported1))
;;         (val2 (supported-value supported2)))
;;     (multiple-value-bind (unified consistent-p) (unify val1 val2)
;;       (if (not consistent-p) (values nil nil)
;;           (cond ((equalp val1 unified)
;;                  (if (implies-p val2 unified)
;;                      (if (more-informative-support-p supported2 supported1)
;;                          supported2
;;                          supported1)
;;                      supported1))

;;                 ((equalp unified val2)
;;                  supported2)

;;                 (t
;;                  (make-supported :value unified
;;                                  :supports (unify-supports supported1 supported2))))))))

;; (defun unify-supports (sup1 sup2)
;;   (union (supported-support sup1) (supported-support sup2) :test #'equalp))

;; (defun more-informative-support-p (s1 s2) (error "not yet implemented"))

