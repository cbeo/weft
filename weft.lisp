;;;; weft.lisp
(defpackage #:weft
  (:use #:cl))

(in-package #:weft)

(defun init ()
  (setf lparallel:*kernel* (lparallel:make-kernel 8 :name "weft-kernel")))

(defvar *nothing* #(:the-nothing)
  "Distinquished object representing the lack of content in a cell.")

(defun nothing-p (thing) (eq thing *nothing*))

;;; Cell Protocol

(defgeneric content (cell)
  (:documentation "Extracts a cells' current content, possibly returning *nothing*"))

(defgeneric add-content (cell content)
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


;; (defgeneric implies-p (a b)
;;   (:documentation "Returns T if A is at least as informative as
;;   B. That is, whenver you know B, you also know A. 

;;   For example if a number is between 3 and 6 (A) then it is also
;;   between 0 and 10 (B), then (implies-p A B) is true."))

(defun implies-p (a b)
  (multiple-value-bind (unified consistent-p)  (unify a b)
    (and consistent-p (equalp a unified))))

(defun iff-p (a b)
  (or (equalp a b)
      (and (implies-p a b) (implies-p b a))))

;;; Cell Class

(defclass simple-cell ()
  ((name :reader cell-name
         :initform (gensym "simple-cell")
         :initarg :name)
   (content
    :initform *nothing*)
   (neighbors
    :accessor neighbors
    :initform (list))))

(defmethod content ((cell simple-cell))
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

;; TODO. change name of ADD-CONTENT to INFORM

(defmethod add-content ((cell simple-cell) increment)
  (let (status)
    (flet ((updater (increment content)
             (cond
               ((nothing-p increment) content)
               ((nothing-p content)
                (setf status 'alert-propagators)
                increment)
               (t
                (multiple-value-bind (unified consistent-p) (unify content increment)
                  (if conistent-p unified
                      (progn
                        (setf status 'inconsistency)
                        content)))))))

      (sb-ext:atomic-update (slot-value cell 'content)
                            #'updater
                            increment))
    (case status
      (alert-propagators (alert-propagators (neighbors cell)))
      (inconsistency (error "Inconsistency! (~a ~a)" (cell-name cell) increment)))))

;; (defmethod add-content ((cell simple-cell) increment)
;;   (with-slots (content neighbors) cell
;;     (cond
;;       ((nothing-p increment) :ok)

;;       ((nothing-p content)
;;        (setf content increment)
;;        (alert-propagators neighbors))

;;       (t
;;        (unless (equal content increment)
;;          (error "Inconsistency"))))))

(defmethod new-neighbor ((cell simple-cell) nbr)
  (unless (member nbr (neighbors cell) :test #'eql)
    (sb-ext:atomic-push nbr (slot-value cell 'neighbors))        ;(push nbr (neighbors cell))
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
                    (add-content output
                                 (apply fn (mapcar #'content inputs))))))))

(defun handle-nothings (fn)
  (lambda (&rest args)
    (if (some #'nothing-p args)
        *nothing*
        (apply fn args))))

(defmethod unify ((old number) (new number))
  (if (= old new)
      (values old t)
      (values nil nil)))

(defvar adder (function->propagator-constructor (handle-nothings #'+)))
(defvar subtractor (function->propagator-constructor (handle-nothings #'-)))
(defvar multiplier (function->propagator-constructor (handle-nothings #'*)))
(defvar divider (function->propagator-constructor (handle-nothings #'/)))

(defun constant (val)
  (function->propagator-constructor (lambda () val)))

(defmacro propagate (fn (&rest inputs) output)
  `(funcall (function->propagator-constructor (handle-nothings ,fn))
            ,@inputs ,output))

(defun sum (x y total)
  (funcall adder x y total)
  (funcall subtractor total x y)
  (funcall subtractor total y x))

(defun product (x y product)
  (funcall multiplier x y product)
  (funcall divider product x y)
  (funcall divider product y x))


(defun fahrenheit-celsius (f c)
  (let ((thirty-two (simple-cell "thirty two"))
        (f-32 (simple-cell "f-32"))
        (five (simple-cell "five"))
        (c*9 (simple-cell "c*9"))
        (nine (simple-cell "nine")))
    (funcall (constant 32) thirty-two)
    (funcall (constant 5) five)
    (funcall (constant 9) nine)
    (sum thirty-two f-32 f)
    (product f-32 five c*9)
    (product c nine c*9)))

(defun celsius-kelvin (c k)
  (let ((many (simple-cell "many")))
    (funcall (constant 273.15) many)
    (sum c many k)))

(defun simple-cell (name) (make-instance 'simple-cell :name name))


;; TODO add "supported" data, i.e. dependency dtracking
;; something like

(defstruct supported value support)
;; and define UNIFY for supported types.

(defmethod unify ((sup1 supported) (sup2 supported))
  (let ((val1 (supported-value sup1))
        (val2 (supported-value sup2)))
    (multiple-value-bind (unified consistent-p) (unify val1 val2)
      (if (not consistent-p) (values nil nil)
          (cond ((equalp val1 unified)
                 (if (implies-p val2 unified)
                     (if (more-informative-support-p sup2 sup1)
                         sup2
                         sup1)
                     sup1))

                ((equalp unified val2)
                 sup2)

                (t
                 (make-supported :value unified
                                 :supports (unify-supports sup1 sup2))))))))

(defun unify-supports (sup1 sup2)
  (union (supported-support sup1) (supported-support sup2) :test #'equalp))

;; TODO add a TMS cell structure and define UNIFY for it.  It should
;; implicitly use "supported" types, and for each list of supports,
;; maintain different vlaues.  It should recursivelyc all UNIFY on the
;; values.  Radul just has each cell remember just the computations
;; actually made -i.e. it doesn't do all permutations of all possible
;; worldviews, just those that have actually been asserted.
