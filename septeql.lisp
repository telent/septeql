(cl:in-package :septeql)

;;;; literals

(defun lisp-to-sql-string (string)
  "Does necessary quoting and/or escaping of STRING such that it can be 
interpolated into a SQL statement as a string literal"
  (with-output-to-string (o)
    (princ "\'" o)
    (loop for c across string do
	  (case c
	    ((#\' #\\) (princ c o) (princ c o))
	    (t (princ c o))))
    (princ "\'" o)))

(defun lisp-to-sql-name (symbol)
  "Transform the lisp SYMBOL into an identifier that the database will like."
  (substitute #\_ #\- (symbol-name symbol)))

(defun sql-name-to-lisp (string)
  "Tranform the database name STRING into a lisp-friendly keyword."
  (intern (substitute #\- #\_ (string-upcase string)) :keyword))


;;;; parsing expressions that return scalar values.  

(defparameter *infix-syntax* '(+ - / * = > < >= <= <> like ilike is and
				 or |::| ~ ~* ! !*)
  "Functions to be transformed to infix syntax.  Must be column-expr * column-expr => column-expr")

(defun infixize (op args)
  (labels ((i (args)
	      (format nil "(~A ~A ~A)"
		      (if (> (length args) 2)
			  (i (butlast args))
			(first args))
		      op (car (last args)))))
    (i args)))

(defvar *scalar-translations* (make-hash-table :test 'equal))

(defun translate-scalar (expr)
  (typecase expr
    (string (lisp-to-sql-string expr))
    (complex (error "Don't know how to translate complex numbers to SQL"))
    (integer (format nil "~S" expr))
    (real (format nil "~S" (coerce expr 'single-float)))
    (null "NULL")
    (boolean "TRUE")
;    (keyword (lisp-to-sql-string (string expr)))
    (symbol (lisp-to-sql-name expr))
    (cons 
     (destructuring-bind (op &rest args) expr
       (cond
	 ((member op *infix-syntax* :test 'string-equal)
	  (infixize op (mapcar #'translate-scalar args)))
	 (t
	  (let ((func (gethash (symbol-name op) *scalar-translations*)))
	    (if func 
		(apply func args)
		(format nil "~A(~{~A~^,~})" 
			(lisp-to-sql-name op )
			(mapcar #'translate-scalar args))))))))))

(defun make-scalar-translator (name lambda)
  (setf (gethash (symbol-name name) *scalar-translations*) lambda))

(defmacro define-scalar-translator (name args &body forms)
  `(make-scalar-translator (quote ,name) (lambda ,args ,@forms)))

(define-scalar-translator position (needle haystack)
  (format nil "position(~A in ~A)" 
	  (translate-scalar needle)
	  (translate-scalar haystack)))
	  

(define-scalar-translator as (column-expr alias)
  (format nil "~A AS ~A" (translate-scalar column-expr) (lisp-to-sql-name alias)))

(define-scalar-translator cast (column-expr type)
  (format nil "cast(~A as ~A)" (translate-scalar column-expr) 
	  (lisp-to-sql-name type)))


(define-scalar-translator case (&rest clauses)
  (with-output-to-string (o)
    (princ "(CASE " o)
    (loop for (expr result) in clauses
	  if (and (symbolp expr) (member expr '(t else) :test #'string-equal))
	  do (format o "ELSE ~A " (translate-scalar result)) 
	  else do (format o "WHEN ~A THEN ~A "
			  (translate-scalar expr)
			  (translate-scalar result)))
    (princ "END) " o)))

;; SQL's CASE is more like CL's COND, so define the latter as an alias
(define-scalar-translator cond (&rest clauses)
  (translate-scalar `(case ,@clauses)))

;;; we *could* use the zero-length symbol for string concatenation,
;;; relying on the printer to render it as ``||'', but that's arguably
;;; kinda tacky.  So, ++ for sticking strings together

(define-scalar-translator ++ (&rest columns)
  (infixize "||" (mapcar #'translate-scalar columns)))

(define-scalar-translator in (column-expr choices)
  (format nil "~A IN (~{~A~^,~})" (translate-scalar column-expr) 
	  (mapcar #'translate-scalar choices)))


;;;; and relations


;;; this models a sql SELECT statement
;;; XXX we don't support HAVING, mostly becaue I don't understand it
(defclass sql ()
  ((distinct :initarg :distinct)
   (attributes :initarg :attributes)
   (from  :initarg :from )
   (where :initarg :where)
   (group-by :initarg :group-by)
   (order-by :initarg :order-by)
   (start :initarg :start) ; LIMIT and OFFSET are
   (end :initarg :end)     ; computed with these
   ))

(defmacro sv (ob slot &optional (default nil default-p))
  (if default-p 
      `(if (slot-boundp ,ob ,slot) (slot-value ,ob ,slot) ,default)
      `(slot-value ,ob ,slot)))

(defun any-slot-bound-p (ob slots)
  (some (lambda (s)  (slot-boundp ob s)) slots))

(defvar *rel-translations* (make-hash-table :test 'equal))

(defun parse-relation (s)
  (etypecase s
    (cons (destructuring-bind (op &rest args) s
	    (apply (gethash (symbol-name op) *rel-translations*) args)))
    (symbol (make-instance 'sql :from s))))

(defmacro define-translator (op args &body forms)
  `(setf (gethash (symbol-name ',op) *rel-translations*)
    (lambda ,args ,@forms)))

(define-translator select (relation predicate)
  (let ((s (parse-relation relation)))
    (when (any-slot-bound-p s '(start end group-by))
      (setf s (make-instance 'sql :from s)))
    (if (slot-boundp s 'where)
	(setf (sv s 'where) `(and ,(sv s 'where) ,predicate))
	(setf (sv s 'where) predicate))
    s))

(define-translator project (attributes relation)
  (let ((s (parse-relation relation)))
    (setf (sv s 'attributes) attributes)
    s))

(define-translator join (rel1 rel2 expr)
  (let ((l (parse-relation rel1))
	(r (parse-relation rel2)))
    (when (any-slot-bound-p l '(start end group-by attributes order-by where))
      ;; _any_ slot other than FROM
      (setf l (make-instance 'sql :from l)))
    (when (any-slot-bound-p r '(start end group-by attributes order-by where))
      (setf r (make-instance 'sql :from r)))
    (make-instance 'sql :from `(join ,(sv l 'from) ,(sv r 'from) ,expr))))

(define-translator left-join (rel1 rel2 expr)
  (let* ((j (parse-relation `(join ,rel1 ,rel2 ,expr)))
	 (from (sv j 'from)))
    (setf (sv j 'from) `(left-join ,@(cdr from)))
    j))

(define-translator distinct (rel)
  (let ((r (parse-relation rel)))
    (setf (sv r 'distinct) t)
    r))

(define-translator group (aggregates dividers relation)
  (let ((l (parse-relation relation)))
    (when (any-slot-bound-p l '(start end group-by attributes order-by))
      (setf l (make-instance 'sql :from l)))
    (setf (sv l 'attributes)
	  (append aggregates dividers)
	  (sv l 'group-by) dividers)
    l))
	  
    
(define-translator order (relation attribute &optional direction)
  (let ((sql (parse-relation relation)))
    (when (any-slot-bound-p sql '(order-by start end))
      (setf sql (make-instance 'sql :from sql)))
    (setf (slot-value sql 'order-by)
	  (cons attribute (or direction :asc)))
    sql))

(define-translator rename (relation name)
  (let ((sql (parse-relation relation)))
    (cond ((any-slot-bound-p sql
			     '(start end group-by attributes order-by where))
	   ;; _any_ slot other than FROM
	   (make-instance 'sql :from `(rename ,sql ,name)))
	  (t 
	   (setf (sv sql 'from)
		 `(rename ,(sv sql 'from) ,name))
	   sql))))

(defun min~ (&rest args)
  (let ((real-args (remove-if #'not args)))
    (and (car real-args) (apply #'min real-args))))

(define-translator limit (relation start end)
  (let ((sql (parse-relation relation)))
    (let ((start (max start (sv sql 'start 0)))
	  (end (min~ end  (sv sql 'end nil))))
      ;; the effect of nested LIMIT clauses is to use the narrowest ones 
      (setf (sv sql 'start) start
	    (sv sql 'end) end)
      sql)))

;;; This is a less useful function than its name would suggest.
;;; It's really only used to give a somewhat sensible name to a 
;;; subselect where the user hasn't provided any
(defun sql-name (sql)
  (cond ((typep sql 'sql) (sql-name (sv sql 'from)))
	((symbolp sql) (lisp-to-sql-name sql))
	((eql (car sql) 'join) (gensym))
	((eql (car sql) 'rename) (third sql))
	(t (error "no name"))))

(defun from-clause-as-string (from)
  (cond ((typep from 'sql)
	 (format nil "(~A) ~A " (as-string from) (sql-name from)))
	((symbolp from) (lisp-to-sql-name from))
	((eql (car from) 'rename)
	 (let ((rel (second from))
	       (name (lisp-to-sql-name (third from))))
	   (if (typep rel 'sql)
	       (format nil "(~A) ~A " (as-string rel) name)
	       (format nil "~A ~A " (from-clause-as-string rel) name))))
	((eql (car from) 'join)	 
	 (format nil "(~A JOIN ~A ON ~A)"
		 (from-clause-as-string  (second from))
		 (from-clause-as-string  (third from))
		 (translate-scalar (fourth from))))
	((eql (car from) 'left-join)	 
	 (format nil "(~A LEFT JOIN ~A ON ~A)"
		 (from-clause-as-string  (second from))
		 (from-clause-as-string  (third from))
		 (translate-scalar (fourth from))))
	(t (error "buggered if I know"))))

(defun as-string (sql)
  (with-output-to-string (s)
    (format s "SELECT ~A ~{~A~^,~} "
	    (if (sv sql 'distinct nil) "DISTINCT" "")
	    (or (mapcar #'translate-scalar (sv sql 'attributes nil)) '(*)))
    (format s "FROM ~A " (from-clause-as-string (sv sql 'from)))
    (let* ((nilg (gensym))
	   (w (sv sql 'where nilg)))
      (unless (eql w nilg) (format s "WHERE ~A " (translate-scalar w))))
    (let ((group (sv sql 'group-by nil)))
      (when group (format s "GROUP BY ~{~A~^,~} "
			  (mapcar #'translate-scalar group))))
    (let ((order (sv sql 'order-by nil)))
      (when order (format s "ORDER BY ~A ~A "
			  (translate-scalar (car order))
			  (symbol-name (cdr order)))))
    (let* ((offset (sv sql 'start 0))
	   (end (sv sql 'end nil)))
      (unless (zerop offset) (format s "OFFSET ~A " offset))
      (if end (format s "LIMIT ~A " (- end offset))))))

(defun to-sql (relation)
  (as-string (parse-relation relation)))
