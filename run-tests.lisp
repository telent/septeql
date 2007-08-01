(in-package :septeql)

(defun do-tests (&optional verbose)
  (with-open-file (stream *tests-file*)
    (let ((failed nil))
      (loop
       (let* ((eof (gensym))
	      (send (read stream nil eof))
	      (expect (read stream nil eof)))
	 (when (eql send eof) (return failed))
	 (let* ((actual (eval send))
		(summary (list :sent send :expected expect :got actual)))
	   (cond ((equal actual expect)
		  (when verbose
		    (format *trace-output* "OK    ~A~%   => ~A~%" 
			    send expect)))
		 (t
		  (when verbose
		    (format *trace-output* "NOT   ~A~%   => ~A~%  got ~A~%"
			    send expect actual))
		  (push summary failed)))))))))
