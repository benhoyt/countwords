;; modified from http://rosettacode.org/wiki/Tokenize_a_string#Common_Lisp
(defun split-string (string)
  (loop for start = 0 then (1+ finish)
        for finish = (position #\Space string :start start)
        collecting (subseq string start finish)
        until (null finish)))

(defparameter *counter* (make-hash-table :test #'equal))
(defconstant +spaces+ '(#\Space #\Tab #\Newline))

(defun trim-spaces (string)
  (string-trim +spaces+ string))

(defun update-word (word) 
  (incf (gethash word *counter* 0)))

(defun main ()
  (loop for line = (read-line nil nil) while line
        for words = (split-string (string-downcase (trim-spaces line)))
        do (loop for word in words unless (zerop (length word)) do (update-word word)))

  (let ((ordered (loop for key being the hash-keys of *counter*
                       using (hash-value value)
                       collect (cons key value))))
    (sort ordered #'> :key #'cdr)
    (dolist (pair ordered)
      (format t "~a ~a~%" (car pair) (cdr pair)))))
