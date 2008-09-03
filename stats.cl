;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                         ;;;;
;;;;                                                         ;;;;
;;;;                                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                         
;;;; UPDATE-POS-ARGS
;;;;                                                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-pos-args (pred-arg pos-arg-list)
  (let ((arg-count (cdr (assoc pred-arg pos-arg-list))))
    (setf pos-arg-list
      (acons (intern pred-arg)
	     (if arg-count
		 (+ arg-count 1)
	       1)
	     pos-arg-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                         
;;;; ANALYZE-POS
;;;;                                                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; POS's that I don't deal with: q, x, p, c 

(defun analyze-pos (pos-name pos-count pred-arg)
  (setf *pos-names*
    (acons (intern pos-name)
	   (if pos-count
	       (+ pos-count 1)
	     1)
	   *pos-names*))
  
  (let ((arg-count (cond
		    ((string= pos-name "n")
		     (cdr (assoc (intern pred-arg) *noun-args*)))
		    ((string= pos-name "v")
		     (cdr (assoc (intern pred-arg) *verb-args*)))
		    ((string= pos-name "a")
		     (cdr (assoc (intern pred-arg) *adj-args*)))
		    (t
		     (cdr (assoc (intern pred-arg) *other-args*))))))
    (cond
     ((string= pos-name "n")
      (setf *noun-args*
	(acons (intern pred-arg)
	       (if arg-count
		   (+ arg-count 1)
		 1)
	       *noun-args*)))
     ((string= pos-name "v")
      (setf *verb-args*
	(acons (intern pred-arg)
	       (if arg-count
		   (+ arg-count 1)
		 1)
	       *verb-args*)))
     ((string= pos-name "a")
      (setf *adj-args*
	(acons (intern pred-arg)
	       (if arg-count
		   (+ arg-count 1)
		 1)
	       *adj-args*)))
     (t
      (setf *other-args*
	(acons (intern pred-arg)
	       (if arg-count
		   (+ arg-count 1)
		 1)
	       *other-args*)))
     ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                         ;;;;
;;;; Main functions                                          ;;;;
;;;;                                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                         
;;;; CORRAL-LEXICON                                          
;;;;                                                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun corral-lexicon ()
  (with-open-file
      (input-stream (car *lexicon-files*) :direction :input)
    (loop for line = (read-line input-stream nil 'eof)
	do
	  (when (eq 'eof line)
	    (return))
	  (let* ((splat
		  (split-subseq-seq ":=" line))
		 (pos-type
;;		  (car (split-subseq-seq " "
		  (string-trim " "
			       (string-right-trim "&"
						  (car (cdr splat)))))
		 )

	    (if (> (length splat) 1)
		(let ((bob 'asdf))
		  (format nil "~%~A" (car (split-subseq-seq
					 " "
					 (car (cdr splat)))))
		  (loop for body-line = (read-line
				       input-stream nil 'eof)
		    do
		      (when (and (> (length body-line) 0)
				 (equal
				  (char body-line
					(1- (length body-line)))
				  #\.))
			(return))
		      (let* ((pred-name-temp
			     (car (cdr (split-subseq-seq
					"KEYREL.PRED "
					body-line))))
			     (pred-name
			      (if (not pred-name-temp)
				  (car (cdr (split-subseq-seq
					"KEYREL.CARG "
					body-line)))
				pred-name-temp)))

			(if pred-name
			    (let* ((trimmed-pred-name
				    (trim-pred-name pred-name))
				   (pos-count
				    (cdr (assoc
					  (intern pos-type)
					  *pos-types*)))
				   (pred-count
				    (cdr (assoc
					  (intern trimmed-pred-name)
					  *lex-pred-names*)))
				   (pos-pred-pair
				    (cdr (assoc
					  (intern pos-type)
					  *pos-pred-map*)))
				   )

			      (setf *pos-types*
				(acons (intern pos-type)
				       (if pos-count
					   (+ pos-count 1)
					 1)
				       *pos-types*))
			      
			      (setf *pos-pred-map*
				(acons (intern pos-type)
				       (cons
					(intern trimmed-pred-name)
					(if pos-pred-pair
					   pos-pred-pair
					  '()))
				       *pos-pred-map*))
			      			      
			      (let ((a-v-n (car
					    (split-subseq-seq
					     "_" pos-type)))
				    (n-pos-count
				     (cdr (assoc
					   (intern pos-type)
					   *n-pos-types*)))
				    (v-pos-count
				     (cdr (assoc
					   (intern pos-type)
					   *v-pos-types*)))
				    (a-pos-count
				     (cdr (assoc
					   (intern pos-type)
					   *a-pos-types*))))

				(cond
				 ((string= a-v-n "n")
				  (setf *n-pos-types*
				    (acons (intern pos-type)
					   (if n-pos-count
					       (+ n-pos-count 1)
					     1)
					   *n-pos-types*)))
				 ((string= a-v-n "v")
				  (setf *v-pos-types*
				    (acons (intern pos-type)
					   (if v-pos-count
					       (+ v-pos-count 1)
					     1)
					   *v-pos-types*)))
				 ((or (string= a-v-n "aj")
				      (string= a-v-n "av"))
				  (setf *a-pos-types*
				    (acons (intern pos-type)
					   (if a-pos-count
					       (+ a-pos-count 1)
					     1)
					   *a-pos-types*)))))
				 ;;(t (format t "~%~A" a-v-n))))
				
			      (setf *lex-pred-names*
				(acons (intern trimmed-pred-name)
				       (if pred-count
					   (+ pred-count 1)
					 1)
				       *lex-pred-names*))
			     			      
			      (return)
			      ))
			))
		  ))
	    )))
  ;;(format t "~%---Predicates Used in the Lexicon---")
  ;;(list-assoc-vals-gt-x *lex-pred-names* 5)
  
  ;;(format t "~%---N POS Types---")
  ;;(list-assoc-vals-gt-x *n-pos-types* 0)
  
  ;;(format t "~%---V POS Types---")
  ;;(list-assoc-vals-gt-x *v-pos-types* 0)
  
  (format t "~%---A POS Types---")
  (list-assoc-vals-gt-x *a-pos-types* 0)
 
  ;;(format t "~%---POSxPred Map---")
  ;;(list-assoc-vals-gt-x *pos-pred-map* (1- 0))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                         
;;;; CORRAL-PREDS-AND-ARGS                                   
;;;;                                                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun corral-preds-and-args ()
    (loop for filename in *predicate-dictionary-files*
	do
	  (with-open-file
	      (input-stream filename :direction :input)
	   ;(let ((line (read-line input-stream nil 'eof)))
	    (loop for line = (read-line input-stream nil 'eof)
		do
		  (when (eq 'eof line)
		    (return))
		  (let* ((splat
			  (split-subseq-seq " " line))
			 (pred-name (car splat))
			 (pred-arg
			  (car (split-subseq-seq
				(format nil "~A " (car splat)) line)))
			 (pred-split
			  (split-subseq-seq "_" pred-name))
			 (pos-name (car (cdr pred-split)))
			 (pred-name-count
			  (assoc pred-name *pred-names*))
			 (pred-arg-count
			  (cdr (assoc (intern pred-arg) *pred-args*)))
			 (pos-count
			  (cdr (assoc (intern pos-name) *pos-names*)))
			 )
		    
		    (setf *pred-names*
		      (acons pred-name
			     (if pred-name-count
				 (+ pred-name-count 1)
			       1)
			     *pred-names*))
		    
		    (setf *pred-args*
		      (acons (intern pred-arg)
			     (if pred-arg-count
				 (+ pred-arg-count 1)
			       1)
			     *pred-args*))
		    
		    (if (or (eq (length pred-split) 3)
			    (eq (length pred-split) 4))
			(analyze-pos pos-name pos-count pred-arg))
		    ;; These other length PREDS should be dealt with
		      ;;(format t "~%~A" line))
		    ))))
    ;;(format t "~%---Predicate Names---")
    ;;(list-assoc-vals *pred-names*)
    
    (format t "~%---Argument Structures---")
    (list-assoc-vals *pred-args*)
        
    (format t "~%---POS Types---")
    (list-assoc-vals *pos-names*)
        
    (format t "~%---Noun Arguments---")
    (list-assoc-vals *noun-args*)
        
    (format t "~%---Verb Arguments---")
    (list-assoc-vals *verb-args*)
        
    (format t "~%---Adjective Arguments---")
    (list-assoc-vals *adj-args*)
    
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                         
;;;; CHECK-POS-PRED-MAP                                      
;;;;                                                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-pos-pred-map ()
  (loop for filename in *predicate-dictionary-files*
      do
	(with-open-file
	    (input-stream filename :direction :input)
	  (loop for line = (read-line input-stream nil 'eof)
	      do
		(when (eq 'eof line)
		  (return))
		(let* ((splat
			(split-subseq-seq " " line))
		       (pred-name (car splat))
		       (pred-arg
			(car (split-subseq-seq
			      (format nil "~A " (car splat)) line)))
		       )
		    
		    (setf *pred-arg-map*
		      (acons (intern pred-name)
			     (intern pred-arg)
			     *pred-arg-map*))))))
  
  (with-open-file
      (input-stream (car *lexicon-files*) :direction :input)
    (loop for line = (read-line input-stream nil 'eof)
	do
	  (when (eq 'eof line)
	    (return))
	  (let* ((splat
		  (split-subseq-seq ":=" line))
		 (pos-type
		  (if (> (length splat) 1)
		      (car (split-subseq-seq " "
					     (car (cdr splat))))))
		 )
	    (if pos-type
		(loop for body-line = (read-line
				       input-stream nil 'eof)
		    do
		      (when (and (> (length body-line) 0)
				 (equal
				  (char body-line
					(1- (length body-line)))
				  #\.))
			(return))
		      (let ((pred-name
			     (car (cdr (split-subseq-seq
					"KEYREL.PRED "
					body-line)))))
			
			(if pred-name
			    (let* ((trimmed-pred-name
				    (trim-pred-name pred-name))
				   (pos-arg-pair
				    (cdr (assoc
					  (intern pos-type)
					  *pos-arg-map*)))
				   (pred-arg-pair
				    (cdr (assoc
					  (intern trimmed-pred-name)
					  *pred-arg-map*)))
				   )
			      
			      (if (and pos-arg-pair
				       (not (eq pos-arg-pair
						pred-arg-pair)))
				  (format t "~%Mismatch: ~A || ~A"
					  pos-arg-pair
					  pred-arg-pair)
				(setf *pos-arg-map*
				  (acons (intern pos-type)
					 (intern pred-arg-pair)
					 *pos-arg-map*)))

			      (return)
			      ))
			))
		  )
	    )))
  )
