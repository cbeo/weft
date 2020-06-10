;;;; weft.lisp
(defpackage #:weft
  (:use #:cl))

(in-package #:weft)

(defun init ()
  (setf lparallel:*kernel* (lparallel:make-kernel 8 :name "lparallel")))

(defvar *nothing* #(:the-nothing)
  "Distinquished object representing the lack of content in a cell.")

(defun nothing-p (thing) (eq thing *nothing*))

(defgeneric content (cell)
  (:documentation "Extracts a cells' current content, possibly returning *nothing*"))

(defgeneric add-content (cell content)
  (:documentation "Adds some content to a cell"))

(defgeneric new-neighbor (cell propagator)
  (:documentation "Asserts that a propagator should be queued when the cell's content changes"))

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

(defmethod add-content ((cell simple-cell) increment)
  (format t "adding content for ~a~%" (cell-name cell))
  (let (status)
    (sb-ext:atomic-update (slot-value cell 'content)
                   (lambda (increment content)
                     (cond
                       ((nothing-p increment) content)
                       ((nothing-p content)
                        (setf status 'alert-propagators)
                        increment)
                       (t (unless (equal content increment)
                            (setf status 'inconsistency))
                          content)))
                   increment)
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

(defvar adder (function->propagator-constructor (handle-nothings #'+)))
(defvar subtractor (function->propagator-constructor (handle-nothings #'-)))
(defvar multiplier (function->propagator-constructor (handle-nothings #'*)))
(defvar divider (function->propagator-constructor (handle-nothings #'/)))

(defun constant (val)
  (function->propagator-constructor (lambda () val)))

(defmacro propagate (fn (&rest inputs) output)
  `(funcall (function->propagator-constructor (handle-nothings ,fn))
            ,@inputs ,output))

(defun f->c (f c)
        (let ((thirty-two (make-instance 'simple-cell))
              (f-32 (make-instance 'simple-cell))
              (five (make-instance 'simple-cell))
              (c*9 (make-instance 'simple-cell))
              (nine (make-instance 'simple-cell)))
          (funcall (constant 32) thirty-two)
          (funcall (constant 5) five)
          (funcall (constant 9) nine)
          (funcall subtractor f thirty-two f-32)
          (funcall multiplier f-32 five c*9)
          (funcall divider c*9 nine c)))

(defun sum (x y total)
  (funcall adder x y total)
  (funcall subtractor total x y)
  (funcall subtractor total y x))

(defun simple-cell (name) (make-instance 'simple-cell :name name))
