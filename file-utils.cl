;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                         ;;;;
;;;;                                                         ;;;;
;;;;                                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sfy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;                     
;;;;  FILTER-FULL-ARGS
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun filter-full-args (predicate-list)
  (setf new-predicate-list '())
  
  (loop
      for predicate in predicate-list
      do
	
	(setf new-predicate-list
	  (append
	   (let ((pred-split (split-subseq-seq " : " predicate)))
	     (let ((left-hand  (string-upcase  
				(string-left-trim 
				 "_" 
				 (string-trim 
				  "\"" 
				  (string-trim " " (car pred-split))))))
		   (right-hand (string-right-trim "." (car (cdr pred-split)))))

	       (verbose-mode-only (format nil " Argument list to be filterd:~%    ~A : ~A~%" left-hand right-hand))
	       
	       (filter-ambiguous-arguments left-hand
					   (string-delete right-hand ","))
	       ))
	   new-predicate-list)
	  )
	)
  new-predicate-list)

(defun filter-out-bracket-by-type (left-hand right-hand left-bracket right-bracket)
  (let ((new-left-hand (subseq right-hand 0 left-bracket))
	(new-right-hand (subseq right-hand (1+ right-bracket) (length right-hand)))
	(new-center-hand (subseq right-hand (1+ left-bracket) right-bracket)))
    
    (verbose-mode-only (format nil "    ~A||~A||~A~%"
			       new-left-hand
			       new-center-hand
			       new-right-hand))
    
    (append (filter-ambiguous-arguments
	     left-hand 
	     (concatenate 'string new-left-hand new-right-hand))
	    (filter-ambiguous-arguments 
	     left-hand  
	     (concatenate 'string 
	       (concatenate  'string 
		 new-left-hand new-center-hand)
	       new-right-hand)))
    )
  )

(defun filter-ambiguous-arguments (left-hand right-hand) 
  (let ((curly-bracket (list (search "{" right-hand) (search "}" right-hand))))
    (if (car curly-bracket)
	(filter-out-bracket-by-type
	 left-hand right-hand (car curly-bracket) (car (cdr curly-bracket)))
      (let ((square-bracket (list (search "[" right-hand) (search "]" right-hand))))
	(if (car square-bracket)
	    (filter-out-bracket-by-type
	     left-hand right-hand (car square-bracket) (car (cdr square-bracket)))
	  (let ((filtered-right-hand (string-trim " " (string-replace right-hand "  " " "))))

	    (list (cons left-hand
			(return-odd-list-entries
			 (split-subseq-seq " " filtered-right-hand))))
	    )
	  )
	) 
      )
    )
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;                     
;;;;  GET-ARGS-FOR-PRED
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-full-args-for-pred (predicate)
  (collapse-all-like-ary-predicates
   (filter-full-args
    (get-args-for-pred-in-listing *predicate-dictionary-files* predicate))))

(defun get-args-for-pred-in-listing (filelisting predicate)
  "Given the name of a file FILENAME, opens the file and reads its contents until it finds a line starting with PREDICATE.  Return that line."
  (setf predicate-spec '()) 
  
  (loop for filename in filelisting
    do 
      (if (= (length predicate-spec) 0)
	  (with-open-file
	      (input-stream filename :direction :input) 

	    (loop for line = (read-line input-stream nil 'eof)
		do 
		  (when (or (eq 'eof line) 
			    (> (length predicate-spec) 0))
		    (return))
		  (if (string=
		       (string-upcase 
			(string-left-trim
			 "_"
			 (string-trim
			  "\""
			  (string-trim
			   " "
			   (car 
			    (split-subseq-seq " : " line))))))
		       predicate)
		      (setf predicate-spec (cons line '()))))
	    (if (> (length predicate-spec) 0)
		(loop for line = (read-line input-stream nil 'eof)
		    do
		      (if (string=
			   (string-upcase 
			    (string-left-trim
			     "_"
			     (string-trim
			      "\""
			      (string-trim
			       " "
			       (car 
				(split-subseq-seq " : " line))))))
			   predicate)
			  (setf predicate-spec (cons line predicate-spec))
			(return))))
	    ))) 
  
  ;;(verbose-mode-only (format nil "get-args-for-pred-in-listing:~%  ~A~%" predicate-spec))
  
  predicate-spec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  WRITE-NEW-LEXICON-TO-FILE
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-new-lexicon-to-file (lexical-entry)
  (with-open-file (output-stream *new-lexicon-file* 
		   :direction :output 
		   :if-exists :append) 
    (write-string (format nil "~%~A~%" lexical-entry) 
		  output-stream) 
    (format t "--~%~A~%--" lexical-entry) 
    ) 
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  WRITE-NEW-PREDICATE-TO-FILE
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-new-predicate-to-file (pred-entry)
  (with-open-file (output-stream *new-predicate-dictionary-file* 
		   :direction :output 
		   :if-exists :append) 
    (write-string (format nil "  ~A~%" pred-entry)
		  output-stream) 
    (format t "  ~A~%" pred-entry)
    ) 
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  GET-POS-AND-PRED
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-pos-and-pred (stem)
  (setf pos-and-pred-pairs '())
  
  (loop
      for lexicon-file in *lexicon-files*
      do
	(with-open-file
	    (input-stream lexicon-file :direction :input)
	  (loop for line = (read-line input-stream nil 'eof)
	      do
		(when (eq 'eof line)
		  (return))
		(let* ((splat
			(split-subseq-seq ":=" line))
		       (pos-type
			(string-trim " "
				     (string-right-trim "&"
							(car (cdr splat)))))
		       )
		  
		  (if (> (length splat) 1)
		      (let* ((stem-line (read-line input-stream nil 'eof))
			     (test-stem
			      (car (cdr (split-subseq-seq "\"" stem-line)))))
			(if (string= stem test-stem)
			    (loop
				for body-line = (read-line input-stream nil 'eof)
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
					(let ((trimmed-pred-name
					       (trim-pred-name pred-name)))
					  
					  (format nil "~%~A:~%-~A~%-~A"
						  test-stem
						  pos-type
						  trimmed-pred-name)
					  
					  (setf pos-and-pred-pairs
					    (cons
					     (list pos-type trimmed-pred-name)
					     pos-and-pred-pairs))
					  
					  (return)))
				    )))))))))
  pos-and-pred-pairs)
