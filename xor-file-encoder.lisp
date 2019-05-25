(ql:quickload :babel)

(defparameter *key* (babel:string-to-octets "SuperDuperSecretIHopeIXORNoNullBytes"))

(defun strxor (a b &optional (acc 'nil))
	(cond ((> (length a) (length b))
				 (strxor (adjust-array a (length b)) b 'nil))
				((< (length a) (length b))
				 (strxor a (adjust-array b (length a)) 'nil))
				((< (length acc) (length a))
				 (strxor a b (cons (logxor (aref a (length acc)) (aref b (length acc))) acc)))
				(t (reverse acc))))

(defun xor-recode (from-file to-file)
  (with-open-file (input-stream from-file
																:direction :input
																:element-type '(unsigned-byte 8))
    (with-open-file (output-stream to-file
																	 :direction :output
																	 :if-exists :supersede
																	 :if-does-not-exist :create
																	 :element-type '(unsigned-byte 8))
      (let ((buf (make-array (length *key*) :element-type (stream-element-type input-stream))))
				(loop for pos = (read-sequence buf input-stream)
					 while (plusp pos)
					 do (progn
								(write-sequence (strxor buf *key*) output-stream :end pos)))))))
