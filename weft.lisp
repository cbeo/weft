;;;; weft.lisp
(defpackage #:weft
  (:use #:cl)
  (:export #:init
           #:+nothing+
           #:content
           #:inform
           #:new-neighbor
           #:unify
           #:implies-p
           #:cell))

(in-package #:weft)

(defun init ()
  (setf lparallel:*kernel* (lparallel:make-kernel 8 :name "weft-kernel")))

(defvar +nothing+ #(:the-nothing)
  "Distinquished object representing the lack of content in a cell.")

(defun nothing-p (thing) (eq thing +nothing+))

;;; Cell Protocol

(defgeneric content (cell)
  (:documentation "Extracts a cells' current content, possibly returning +nothing+"))

(defgeneric inform (cell content)
  (:documentation "Adds some content to a cell"))

(defgeneric new-neighbor (cell propagator)
  (:documentation "Asserts that a propagator should be queued when the cell's content changes"))


;;; Information Protocol

(defgeneric unify (old new)
  (:documentation "Unifies OLD information with NEW information. 

Returns two values UNIFIED and CONSISTENT-P.

UNIFIED is the result of a unify and CONSISTENT-P is T if OLD is
consistent with NEW, otherwise CONSISTENT-P is NIL.

UNIFY should be a pure function, and should under no circumstances
modify its arguments."))

(defun implies-p (a b)
  (multiple-value-bind (unified consistent-p)  (unify a b)
    (and consistent-p (equalp a unified))))

;;; Cell Class
(defvar *cell-id* 0)

(defclass cell ()
  ((name
    :reader cell-name
    :initarg :name
    :initform (format nil "cell-~a" (incf *cell-id*)))
   (content
    :initform +nothing+)
   (neighbors
    :accessor neighbors
    :initform (list))))

(defmethod content ((cell cell))
  (slot-value cell 'content))

(defun alert-propagator (p)
  (lparallel:future (funcall p)))

(defun alert-propagators (ps)
  (dolist (p ps) (alert-propagator p)))

;; so i think that a UNIFY generic function can be written for each
;; pair of types that one might want to store in a cell. ..

;; what is the best way to handle contradictions? rn i'm signallying
;; them to be hanelded - when signalled they aren't altering the
;; actual cell contents, and aren't alerting propagators.

;; how are conditions to be handled when they're signaled in lparallel
;; futures threads?

(defmethod inform ((cell cell) increment)
  (let (status)
    (flet ((updater (increment content)
             (cond
               ((nothing-p increment) content)
               ((nothing-p content)
                (setf status 'alert-propagators)
                increment)
               (t
                (multiple-value-bind (unified consistent-p) (unify content increment)
                  (if consistent-p unified
                      (progn
                        (setf status 'inconsistency)
                        content)))))))
      (sb-ext:atomic-update (slot-value cell 'content)
                            #'updater
                            increment))
    (case status
      (alert-propagators (alert-propagators (neighbors cell)))
      (inconsistency (error "Inconsistency! (~a ~a)" (cell-name cell) increment)))))

(defmethod new-neighbor ((cell cell) nbr)
  (unless (member nbr (neighbors cell) :test #'eql)
    (sb-ext:atomic-push nbr (slot-value cell 'neighbors))
    (alert-propagator nbr)))

(defun propagator (neighbors to-do)
  (dolist (cell neighbors)
    (new-neighbor cell to-do))
  (alert-propagator to-do))

(defun function->propagator-constructor (fn)
  (lambda (&rest cells)
    (let ((output (car (last cells)))
          (inputs (butlast cells)))
      (propagator inputs
                  (lambda ()
                    (inform output
                            (apply fn (mapcar #'content inputs))))))))

(defun handle-nothings (fn)
  (lambda (&rest args)
    (if (some #'nothing-p args)
        +nothing+
        (apply fn args))))

(defmethod unify ((old (eql t)) (new (eql nil)))
  (values nil nil))

(defmethod unify ((old (eql nil)) (new (eql t)))
  (values nil nil))

(defmethod unify ((old (eql t)) (new (eql t)))
  (values old t))

(defmethod unify ((old (eql nil)) (new (eql nil)))
  (values old t))

(defmethod unify ((old number) (new number))
  (if (= old new)
      (values old t)
      (values nil nil)))

(defun constant (val)
  (function->propagator-constructor (lambda () val)))

(defmacro propagate (fn (&rest inputs) output)
  `(funcall (function->propagator-constructor (handle-nothings ,fn))
            ,@inputs ,output))

(defun cell (&optional (name (format nil "cell-~a" (incf *cell-id*))))
  (make-instance 'cell :name name))

(defmacro with-cells ((&rest cell-vars) &body body)
  (let ((cell-bindings (mapcar (lambda (var) (list var (list 'weft::cell)))
                               cell-vars)))
    `(let ,cell-bindings
       ,@body)))

(defun list-rotate (ls)
  (append (rest ls) (list (first ls))))

(defun list-rotations (ls)
  (let ((rotations (list ls)))
    (dotimes (_ (1-  (length ls)) rotations)
      (push (list-rotate (first rotations))
            rotations))))

(defmacro commutative-network (operation inverse (&rest inputs) output)
  (let ((input-rotations (list-rotations inputs)))
    `(progn
       (propagate ,operation ,inputs ,output)
       ,@(loop :for alt-inputs :in input-rotations
              :collect `(propagate ,inverse ,(cons output (cdr alt-inputs)) ,(car alt-inputs))))))


(defun implication (a b)
  (propagate (lambda (x) (if x x +nothing+)) (a) b)
  (propagate (lambda (x) (if (not x) x +nothing+)) (b) a))

(defun it-is (a)
  (inform a t))

(defun it-is-not (a)
  (inform a nil))

;; what about and, and or, ??? should it-is and it-is-not be macros ?
;; should there be an overarchign 'logic' macro that uses propnets
;; under the hood, and interpretes keywords to create the right
;; structures?
;; is there a way to wait ona cell to "finish"?
