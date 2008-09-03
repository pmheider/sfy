;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                        ;;;;
;;;; UTILITIES                                              ;;;;
;;;;                                                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sfy)

(defun sntell (x)
  (snepslog::tell x))
(defun snask (x)
  (snepslog::ask x))
(defun list-wffs ()
  (snepslog::tell "list-wffs"))
(defun snepslog ()
  (cl-user::snepslog))


;; Eventually, this function will look in the network for
;; new words and(?)/or strings to make any already-been-seen
;; strings integrate properly with the network
(defun verify-string (fpv)
  fpv
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;; 
;;;;  VERBOSE-MODE-ONLY
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun verbose-mode-only (str1)
  (if *sfy-verbose-mode*
      (format t "~A" str1)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;; 
;;;;  DEMO-MODE-ONLY
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun demo-mode-only (str1)
  (if *sfy-demo-mode*
      (format t "~A" str1)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;; 
;;;;  COLLAPSE-TWO-N-ARY-PREDICATES
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun collapse-all-like-ary-predicates (predicate-list)
  (setf n-ary-predicates '())
  
  (if (= (length predicate-list) 1)
      predicate-list
    (let ()
      (loop
	  for predicate in predicate-list
	  do
	    (let* ((pred-length (length predicate))
		   (location-in-assoc (assoc pred-length n-ary-predicates)))
	      
	      (setf n-ary-predicates
		(acons
		 pred-length
		 (if (not location-in-assoc)
		     predicate
		   (collapse-two-n-ary-predicates 0
						  (cdr location-in-assoc)
						  predicate
						  )
		   )
		 n-ary-predicates)
		)
	      )
	    )
      (return-assoc-data n-ary-predicates)
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;; 
;;;;  COLLAPSE-TWO-N-ARY-PREDICATES
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun collapse-two-n-ary-predicates (last-argument-added old-predicate new-predicate)
  (if (= last-argument-added (length old-predicate))
      '()
    (cons
     (let ((old-argument (nth last-argument-added old-predicate))
	   (new-argument (nth last-argument-added new-predicate)))
       
       (if (search new-argument old-argument)
	   old-argument
	 (concatenate 'string old-argument "_" new-argument)
	 )
       )
     (collapse-two-n-ary-predicates (1+ last-argument-added) old-predicate new-predicate))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;; 
;;;;  RETURN-ASSOC-DATA
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun return-assoc-data (assoc-list)
  (return-assoc-data-kernel assoc-list '())
  )

(defun return-assoc-data-kernel (assoc-list known-keys)
  (if (= (length assoc-list) 0)
      '()
    (let* ((key-datum (car assoc-list))
	   (key (car key-datum)))
      (if (not (assoc key known-keys))
	  (cons (cdr key-datum)
		(return-assoc-data-kernel
		 (cdr assoc-list)
		 (acons key "found" known-keys)
		 )
		)
	)
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;; 
;;;;  RETURN-ODD-LIST-ENTRIES
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun return-odd-list-entries (full-list)
  (if (= (list-length full-list) 0)
      '()
    (cons (car full-list)
	  (if (= (list-length full-list) 2)
	      '()
	    (return-odd-list-entries (cdr (cdr full-list))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;; 
;;;;  STRING-DELETE
;;;;  STRING-REPLACE
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun string-delete (str1 sub1) 
  (string-replace str1 sub1 "")) 

(defun string-replace (str1 sub1 sub2)
  (let ((str1 (string str1))
	(str2 "")
	(sub1 (string sub1))
	(sub2 (string sub2))
	(index1 0))
    (loop
      (if (string-equal str1 sub1
			:start1 index1
			:end1 (min (length str1)
				   (+ index1 (length sub1))))
	  (progn
	    (setq str2 (concatenate 'string str2 sub2))
	    (incf index1 (length sub1)))
	(progn
	  (setq str2 (concatenate 'string
		       str2
		       (subseq str1 index1 (1+ index1))))
	  (incf index1)))
      (unless (< index1 (length str1))
	(return str2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  GENERATE-TELL
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-tell (pred-list)
  (setf *running-tell* "")
  
  (loop
      for argument in (cdr (cdr pred-list))
      do	
	(setf current-label
	  (cdr (assoc (intern
		       (format nil "~A" argument))
		      *label-directory*)))
	(setf *running-tell* (concatenate 'string *running-tell*
					  (format nil "~A," (if current-label
								current-label
							      argument))))
	)
  (format nil "~A" (string-right-trim "," *running-tell*)))
  ;; This second line was used to maybe fix the ABORT'ed wffs problem
  ;;(format nil "~A" (string-right-trim "," (string-upcase *running-tell*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  LIST-ASSOC-VALS
;;;;  LIST-ASSOC-VALS-GT-X
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-assoc-vals (src-assoc)
  (list-assoc-vals src-assoc 0))

(defun list-assoc-vals-gt-x (src-assoc gt-value)
  (setf known-vars '())
  
  (loop for var-val-pair in src-assoc
      do
	(if (not (assoc (car var-val-pair) known-vars))
	    (setf known-vars (acons 
			      (car var-val-pair)
			      (cdr var-val-pair)
			      known-vars))))
  
  (loop for var-val-pair in known-vars
      do
	(cond
	 ((eq gt-value (1- 0))
	  (format t "~%~A:  ~A"
		  (car var-val-pair)
		  (cdr var-val-pair)))
	 ((> (cdr var-val-pair) gt-value)
	  (format t "~%~A:  ~A"
		  (car var-val-pair)
		  (cdr var-val-pair)))))
  
  (format t "~%~%Unique Values:  ~A" (length known-vars))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  SPLIT-SUBSEQ-SEQ
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-subseq-seq
    (subsequence sequence &key string-trim)
  (loop for last = 0 then (+ pos (length subsequence))
      for pos = (search subsequence sequence :start2 last)
      for subseq = (subseq sequence last
			   (or pos (length sequence)))
      when string-trim do (setf subseq
			    (string-trim string-trim subseq))
      unless (zerop (length subseq)) collect subseq
      while pos)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  TRIM-PRED-NAME
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun trim-pred-name (pred-name)
  (string-left-trim
   "_"
   (let ((pith (car (split-subseq-seq "\"" pred-name))))
     (if pith
	 pith
       (string-right-trim "," pred-name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  FUNCTIONS used for anaphora resolution
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Given a predicate PRED and a argument position TEST-POSITION, query
;; the KB to see if anything matches it.  If so, return that result.
;; TODO:  If multiple nodes/proposition match, this will probably break.
(defun check-predicate (pred test-argument test-position)
  (let ((reduced-pred
	 (subseq pred
		 0 (- (length pred) 2))) 
	(pred-arity 
	 (parse-integer
	  (subseq pred
		  (1- (length pred)) 
		  (length pred))))
	(filler (list "?a" "?b" "?x" "?y" "?z"))
	(arguments (format nil "~A(" pred))
	(current-arg 0))

    (loop
	while (< current-arg pred-arity)
	do	  
	  (setf arguments (format nil "~A~A, "
				  arguments
				  (if (= current-arg test-position)
				      test-argument
				    (nth current-arg filler))))
	  
	  (setf current-arg (1+ current-arg)))
    
    (setf arguments
      (format nil "~A)?" (string-right-trim ", " arguments)))
    
    (verbose-mode-only (format nil "~A~%" arguments))
    
    (snepslog::tell (format nil "~A" arguments)) 
    )
  )

(defun resolve-chess-subject (referant piece-type verb-type)
  (let ((referant-string (car (sneps:ns-to-lisp-list referant)))) 

    (format t "  Subject of the Verb:  ~A~%" verb-type)

    (setf *current-chess-move* (acons
				'subject
				(list referant-string piece-type)
				*current-chess-move*))
    
    (setf *current-chess-move* (acons
				'verb
				(list verb-type)
				*current-chess-move*))
    )
  )

(defun resolve-chess-object (referant piece-type verb-type)
  (let ((referant-string (car (sneps:ns-to-lisp-list referant)))) 

    (format t "  Object of the Verb:  ~A~%" verb-type)
    
    (setf *current-chess-move* (acons
				'object
				(list referant-string piece-type)
				*current-chess-move*))
    )
  )

(defun resolve-chess-indirect-object (referant piece-type verb-type)
  (let ((referant-string (car (sneps:ns-to-lisp-list referant)))) 

    (format t "  Indirect object of the Verb:  ~A~%" verb-type)
    
    (setf *current-chess-move* (acons
				'indirect-object
				(list referant-string piece-type)
				*current-chess-move*))
    )
  )

(defun resolve-chess-preposition (referant piece-type verb-type)
  (let ((referant-string (car (sneps:ns-to-lisp-list referant)))) 

    (format t "  Argument of the preposition:  ~A~%" verb-type)
    
    (setf *current-chess-move* (acons
				(if (string-equal verb-type "TO")
				    'to
				  'from)
				(list referant-string piece-type)
				*current-chess-move*))
    )
  )

(defun resolve-isa-referant (referant) 
  (let ((referant-string (car (sneps:ns-to-lisp-list referant)))) 
   
    (setf *known-type* nil)
    (let ((noun-types (list "elephant_n_1_rel_3"
			     "canary_n_1_rel_3"
			     "bird_n_1_rel_3"
			     "animal_n_1_rel_3")))
      (loop
	  for noun-type in noun-types
	  while (not *known-type*) 
	  do   
	    (if (car (sfy::check-predicate noun-type referant-string 1)) 
		(setf *known-type*
		  (car (sfy::split-subseq-seq "_" noun-type))))
	    )	    
      )

    
    (if (not *known-type*)
	(demo-mode-only
	 (format nil "WARNING:  Unknown noun type found.~%")))
	     
    (demo-mode-only
     (format nil "  lexItem(~A, ~A)~%" referant-string *known-type*))
    
    (snepslog::tell
     (format nil "lexItem(~A, ~A)." referant-string *known-type*))
    
    (let ((verb-types (list "BE_V_ID_REL_5"))) 
      
      (setf sntell-return nil)
      (loop 
	  for verb-type in verb-types
	  while (not sntell-return)
	  do 
	    (cond
	     ((car (check-predicate verb-type referant-string 2))
	      (setf *current-isa* (acons
				   'subject
				   (list referant-string *known-type*)
				   *current-isa*)))
	     ((car (check-predicate verb-type referant-string 3))
	      (setf *current-isa* (acons
				   'object
				   (list referant-string *known-type*)
				   *current-isa*)))
	     (t
	      (demo-mode-only
	       (format nil "WARNING:  No verb matched ISA construction.~%"))))
	    )
      
      )
    )
  )

(defun resolve-chess-referant (referant) 
  (let ((referant-string (car (sneps:ns-to-lisp-list referant)))) 

    (setf *known-type* nil)
    (let ((piece-types (list "bishop_n_1_rel_3"
			     "black_n_1_rel_3"
			     "castle_n_1_rel_3"
			     "king_n_of_rel_4"
			     "knight_n_1_rel_3"
			     "pawn_n_1_rel_3"
			     "queen_n_of_rel_4"
			     "white_n_1_rel_3")))
      ;;TODO:  "rook_n_1_rel_3"
      
      (setf sntell-return nil)
      (loop
	  for piece-type in piece-types
	  while (not sntell-return) 
	  do   
	    (if (car (sfy::check-predicate piece-type referant-string 1)) 
		(setf sntell-return 
		  (car (sfy::split-subseq-seq "_" piece-type)))) 
	    
	    (if sntell-return  
		(setf *known-type*  
		  (car (sfy::split-subseq-seq "_" piece-type)))) 
	    ) 
      )
    
    (if (not *known-type*)
	(let ((name-types (list "king" 
				"queen"))) 
	
	  (setf sntell-return nil) 
	  (loop   
	      for name-type in name-types  
	      while (not sntell-return) 
	      do   
		(setf sntell-return 
		  (car (snepslog::tell 
			(format nil "named_rel_4(named_rel,?X,(~A))?" 
				name-type)))) 
		
		(if sntell-return 
		    (setf *known-type* name-type)) 
		)
	
	  (verbose-mode-only
	   (if (not *known-type*)
	       (format nil
		       "WARNING:  Referant is outside of CHESSS domain~%")))
	  )
      )
    
    ;;TODO clean up known-type vs. piece-type
    (setf piece-type *known-type*)
    
    (demo-mode-only
     (format nil "  hasType(~A, ~A)~%" referant-string piece-type))
    (verbose-mode-only
     (format nil "  hasType(~A, ~A)~%" referant-string piece-type))

    (let ((verb-types (list "CAPTURE_V_1_REL_5"
			    "MAKE_V_1_REL_6"
			    "MOVE_V_1_REL_4"
			    "TAKE_V_1_REL_6"
			    "THREATEN_V_1_REL_5"))) 
      
      (setf sntell-return nil)
      (loop 
	  for verb-type in verb-types
	  while (not sntell-return)
	  do 
	    (if (car (check-predicate verb-type referant-string 2))
		(setf sntell-return
		  (car (sfy::split-subseq-seq "_" verb-type))))
	    )

      (if sntell-return
	  (resolve-chess-subject referant piece-type sntell-return))
      ) 
    
    (if (not sntell-return)
	(let ((verb-types (list "CAPTURE_V_1_REL_5"
				"MAKE_V_1_REL_6"
				"TAKE_V_1_REL_6"
				"THREATEN_V_1_REL_5"))) 
      
	  (loop 
	      for verb-type in verb-types
	      while (not sntell-return)
	      do 
		(if (car (check-predicate verb-type referant-string 3))
		    (setf sntell-return
		      (car (sfy::split-subseq-seq "_" verb-type))))
		)
	  
	  (if sntell-return
	      (resolve-chess-object referant piece-type sntell-return)
	    ) 
	  )
      )
    
    (if (not sntell-return)
	(let ((verb-types (list "MAKE_V_1_REL_6")))
	  
	  (loop 
	      for verb-type in verb-types
	      while (not sntell-return)
	      do 
		(if (car (check-predicate verb-type referant-string 4))
		    (setf sntell-return
		      (car (sfy::split-subseq-seq "_" verb-type))))
		)
	  
	  (if sntell-return
	      (resolve-chess-indirect-object referant piece-type sntell-return)
	    ) 
	  )
      )
    
    (if (not sntell-return)
	(let ((verb-types (list "FROM_P_REL_5"
				"TO_P_REL_5")))
	  
	  (loop 
	      for verb-type in verb-types
	      while (not sntell-return)
	      do 
		(if (car (check-predicate verb-type referant-string 3))
		    (setf sntell-return
		      (car (sfy::split-subseq-seq "_" verb-type))))
		)
	  
	  (if sntell-return
	      (resolve-chess-preposition referant piece-type sntell-return)
	    (format t "Referant could not be classed syntactically.~%"))
	  )
      ) 
    )
  )

(defun resolve-referant (referant)
  ;; TODO:  this needs to be generalized to the whole noun base.
  ;;(let ((referant-string (car (sneps:ns-to-lisp-list referant)))) 

  (cond
   (*sfy-chess-mode*
    (resolve-chess-referant referant))
   (*sfy-isa-mode*
    (resolve-isa-referant referant))
   (t
    (format t "WARNING:  No reference resolution mode chosen.~%"))
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  FUNCTIONS THAT INTERACT WITH OUTSIDE CODE
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-phonology (word)
  "fu"
  ;; (snepslog::get-pronunciations word)
  )

;;;; TODO update this frame to include nested as per numbers
(defun create-phonology (utterance)
  (sntell (format nil "containsWord(~A, ~A, 0).~%~%"
		  utterance (get-phonology utterance)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  FUNCTIONS FROM PIERCE'S ORIGINAL CODE    
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Caches the bindings for the current MRS structure.  The bindings
;; are stored in an alist mapping between handle ids:
;;
;;  ((1 . 1) (3 . 3) (6 . 3) (5 . 5) (18 . 5) (9 . 9) (16 . 9)
;;   (13 . 13) (15 . 13) (14 . 14) (7 . 14))
;;
(defun scope-mrs (psoa)
  (setf *bindings*
    (mrs::canonical-bindings
     (first (mrs::make-scoped-mrs psoa)))))

;; Caches the mapping from handle ids to relations for the current MRS
;; structure.  Two indices are constructed: (1) a mapping from handle
;; ids to handle objects, (2) a mapping from handles to lists of
;; relations.  The first supports FOLLOW-HANDEL, and the second
;; supports HANDEL-RELATIONS.
(defun index-handels (psoa)
  (setf *handel-rlist* (make-hash-table))
  (setf *handel-index* (make-hash-table :test #'eql))
  (loop
    for r in (mrs::psoa-liszt psoa)
    for h = (mrs::rel-handel r)
    do
    (setf (gethash (mrs::var-id h) *handel-index*) h)
    (push r (gethash h *handel-rlist*))))
