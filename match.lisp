;;;; match.lisp

(in-package #:sade)

(defgeneric %match (spec data bindings)
  (:method (spec data bindings)
    nil))

(defun match-symbol (spec data bindings)
  (cond
    ((eq spec '*)
     t)
    ((primitive-p spec)
     (eql data spec))
    ((gethash spec bindings)
     (%match (gethash spec bindings) data bindings))
    ((symbolp spec)
     (setf (gethash spec bindings) data))))

(defmethod %match ((spec symbol) data bindings)
  (match-symbol spec data bindings))

(defmethod %match ((spec integer) (data integer) bindings)
  (= spec data))

(defmethod %match ((spec list) (data list) bindings)
  (when (or (and (eq (car (last spec)) '**)
                 (>= (length data) (1- (length spec))))
            (= (length data) (length spec)))
    (loop for spec-elem in (if (eq (car (last spec)) '**)
                               (butlast spec)
                               spec)
          for data-elem in data
          for i from 0
          unless (%match spec-elem data-elem bindings)
            do (return nil)
          finally (return i))))

(defun match (spec data)
  (let ((bindings (make-hash-table :test 'equalp)))
    (values (%match spec data bindings)
            bindings)))
