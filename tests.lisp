;;; This file created by kludgy bits of elisp, have a care when
;;; editing by hand.  See http://www.coruskate.net/Testing%20times

;; sent
(+ 1 2) ; this is a comment
;; received
3
;; sent
(to-sql '(project ((as (from-universal-time date) ident) name) venue))
;; received
"SELECT FROM_UNIVERSAL_TIME(DATE) AS IDENT,NAME FROM VENUE "

;; sent
(+ 1 
	2
	3)
	
;; received
6
;; sent
(to-sql 'relation)
;; received
"SELECT * FROM RELATION "
;; sent
(to-sql '(select relation (= value 1)))
;; received
"SELECT * FROM RELATION WHERE (VALUE = 1) "
;; sent
(to-sql '(order relation attribute))
;; received
"SELECT * FROM RELATION ORDER BY ATTRIBUTE ASC "
;; sent
(to-sql '(order relation attribute :desc))
;; received
"SELECT * FROM RELATION ORDER BY ATTRIBUTE DESC "
;; sent
(to-sql '(limit (order relation attribute :desc) 5 15))
;; received
"SELECT * FROM RELATION ORDER BY ATTRIBUTE DESC OFFSET 5 LIMIT 10 "
;; sent
(to-sql '(order (limit relation 5 15) attribute :desc) )
;; received
"SELECT * FROM (SELECT * FROM RELATION OFFSET 5 LIMIT 10 ) RELATION  ORDER BY ATTRIBUTE DESC "
;; sent
(to-sql '(project (att1 att2 att3) relation))
;; received
"SELECT ATT1,ATT2,ATT3 FROM RELATION "
;; sent
(to-sql '(project (att1 (as att2 name) att3) relation))
;; received
"SELECT ATT1,ATT2 AS NAME,ATT3 FROM RELATION "
;; sent
(to-sql '(project (att1 (as (function att2 att10) name) att3) relation))
;; received
"SELECT ATT1,FUNCTION(ATT2,ATT10) AS NAME,ATT3 FROM RELATION "
;; sent
(to-sql '(select r (or (= a b) (> a b) (< a b) (<= a b) (>= a b) (like a b) (ilike a b) (~ a b) (~* a b) (! a b) (!* a b)))) ; all the infix operations
;; received
"SELECT * FROM R WHERE (((((((((((A = B) OR (A > B)) OR (A < B)) OR (A <= B)) OR (A >= B)) OR (A LIKE B)) OR (A ILIKE B)) OR (A ~ B)) OR (A ~* B)) OR (A ! B)) OR (A !* B)) "
;; sent
(to-sql '(project ((as (++ a b) cat)) r))
;; received
"SELECT (A || B) AS CAT FROM R "
;; sent
(to-sql '(rename relation new-name))
;; received
"SELECT * FROM RELATION NEW_NAME  "
;; sent
(to-sql '(rename (order relation id) new-name))
;; received
"SELECT * FROM (SELECT * FROM RELATION ORDER BY ID ASC ) NEW_NAME  "
;; sent
(to-sql '(join (join rel1 rel2 predicate) rel3 pred2))
;; received
"SELECT * FROM ((REL1 JOIN REL2 ON PREDICATE) JOIN REL3 ON PRED2) "
;; sent
(to-sql '(join rel1 rel2 predicate))
;; received
"SELECT * FROM (REL1 JOIN REL2 ON PREDICATE) "
;; sent
(to-sql '(rename (join rel1 rel2 predicate) newname))
;; received
"SELECT * FROM (REL1 JOIN REL2 ON PREDICATE) NEWNAME  "
;; sent
(to-sql '(rename (order relation attribute) newname))
;; received
"SELECT * FROM (SELECT * FROM RELATION ORDER BY ATTRIBUTE ASC ) NEWNAME  "
;; sent
(to-sql '(select (limit (order relation attribute) 0 3) pred))
;; received
"SELECT * FROM (SELECT * FROM RELATION ORDER BY ATTRIBUTE ASC LIMIT 3 ) RELATION  WHERE PRED "

;; sent
(7ql:to-sql '(project ("string") rel))
;; received
"SELECT 'string' FROM REL "
;; sent
(7ql:to-sql '(project ("str'ing") rel))
;; received
"SELECT 'str''ing' FROM REL "
;; sent
(7ql:to-sql '(group ((as (sum quantity) quantity) (as (sum (* quantity cost)) value))
		   (event-id event-description)
		   order-line))
;; received
"SELECT SUM(QUANTITY) AS QUANTITY,SUM((QUANTITY * COST)) AS VALUE,EVENT_ID,EVENT_DESCRIPTION FROM ORDER_LINE GROUP BY EVENT_ID,EVENT_DESCRIPTION "
;; sent
(7ql:to-sql '(project ((case ((= 1 1) true) ((= 1 5) hello) (t null))) foo))
;; received
"SELECT (CASE WHEN (1 = 1) THEN TRUE WHEN (1 = 5) THEN HELLO ELSE NULL END)  FROM FOO "
;; sent
(7ql:to-sql '(project ((as (case ((= 1 1) true) ((= 1 5) hello) (t null)) title)) foo))
;; received
"SELECT (CASE WHEN (1 = 1) THEN TRUE WHEN (1 = 5) THEN HELLO ELSE NULL END)  AS TITLE FROM FOO "
;; sent
(7ql:to-sql `(project (3d1) dual))
;; received
"SELECT 30.0 FROM DUAL "
;; sent
(7ql:to-sql `(project (3/4) dual))
;; received
"SELECT 0.75 FROM DUAL "
;; sent
(7ql:to-sql `(project (3) dual))
;; received
"SELECT 3 FROM DUAL "
;; sent
(type-of (second (multiple-value-list (ignore-errors (to-sql `(project (#c(1 2)) dual))))))
;; received
SIMPLE-ERROR
;; sent
(to-sql '(project ((cond ((= 1 1) true) ((= 1 5) hello) (t null))) foo))
;; received
"SELECT (CASE WHEN (1 = 1) THEN TRUE WHEN (1 = 5) THEN HELLO ELSE NULL END)  FROM FOO "
;; sent
(to-sql '(select relation (and (= value 1) (< another 10) (or true false))))
;; received
"SELECT * FROM RELATION WHERE (((VALUE = 1) AND (ANOTHER < 10)) AND (TRUE OR FALSE)) "