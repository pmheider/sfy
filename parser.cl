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
;;;;  PARSE
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clear-old-parse-variables ()
  t
  )

(defun parse (sentence)
  
  (clear-old-parse-variables)
  
  ;; Query CMU/CASSIE/WordNET for the phonology (in that order)
  (if *include-phonology*
      (create-phonology sentence))

  ;; Verify that all the words exist
  (if *bootstrap-lexicon*
      (verify-words sentence))
  
  ;; -- Insert dummy words for any unknown words
  (if (> (list-length *unknown-word-list*) 0)
      (format t "Unknown Words~%-------------~%~A~%" *unknown-word-list*))
  
  ;; Parse the sentences
  (lkb::do-parse-tty sentence)
  
  ;; We can either just run on the first parse which LKB has ranked
  ;; to a certain extent or just run the gamut
  (let ((parses (if *use-first-parse*
		    1
		  (length lkb::*parse-record*))))

    (verbose-mode-only
     (format nil "Parse Count:  ~A~%" (length lkb::*parse-record*)))
    (demo-mode-only
     (format nil "Parse Count:  ~A~%" (length lkb::*parse-record*)))
    
    (if (> (length lkb::*parse-record*) 0)
	(let ()
	  ;; For every parse up to our limit
	(loop for p in (subseq lkb::*parse-record* 0 parses)
		       ;; Pull out the semantic representation
		       ;; This m is frequently typed as psoa in the LKB
		       ;; functions
	    for m = (mrs::extract-mrs p)
		    ;; If it exists, keep on chugging
	    when m do
	      
	      ;; Again, we can run only the first set of scopings
	      ;; ((scopes 1))
	      ;; or run the whole gamut
	      (let ((scopes (length (mrs::make-scoped-mrs m))))
		
		(verbose-mode-only
		 (format nil "Scope Reading Count:  ~A~%" scopes))
		(demo-mode-only
		 (format nil "Scope Reading Count:  ~A~%" scopes))
		
		;; Clear the memory of past label/node associations
		(setf *label-directory* '())
		
		;; Pierce's function to cache *bindings*
		(scope-mrs m)
		
		;; Pierce's function to create a hash of handels and their
		;; relations
		(index-handels m)
		
		;; Create a SNePS representation from the MRS	    
		(mrs::output-mrs m 'mrs::indexed)
		(mrs-to-sneps m)
		
		;; Add each equivalent handle to the SNePS network
		;; via sneps:tell "Qeq(hX, hY)."
		(create-qeqs)
		
		;; Create a list of Qeq's that match all proper/named/unique
		;; nouns
		)
	      )
	    
      ;; Rank the parses and scopal lists according to expectation
      ;; -- Accuracy with prior knowledge
      ;; -- Fits discourse model
      ;; -- Probable genre/construction of utterance
      
;;;; THIS SECTION IS STILL DANGEROUS MAGIC ;;;;  
      ;; For every new definite noun referant (i.e., using THE), we want to
      ;; create a new mention in the KB
      ;; NB:  Each referant type can be separated here for special processing
      ;; or combined into one PERFORM as below in the next stage.
      ;;;; THE_Q_REL ;;;;
      (snepslog::tell "perform withall({?pred, ?referant, ?t1, ?t2}, the_q_rel_5(?pred, ?referant, ?t1, ?t2), snif({if(oldMention(?pred, ?referant), doNothing()), else(createNewMention(?pred, ?referant))}), doNothing()).")
      ;;;; A_Q_REL ;;;;
      (snepslog::tell "perform withall({?pred, ?referant, ?t1, ?t2}, a_q_rel_5(?pred, ?referant, ?t1, ?t2), snif({if(oldMention(?pred, ?referant), doNothing()), else(createNewMention(?pred, ?referant))}), doNothing()).")
      ;;;; UDEF_Q_REL ;;;;
      (snepslog::tell "perform withall({?pred, ?referant, ?t1, ?t2}, udef_q_rel_5(?pred, ?referant, ?t1, ?t2), snif({if(oldMention(?pred, ?referant), doNothing()), else(createNewMention(?pred, ?referant))}), doNothing()).")
      ;;;; NAMED_REL ;;;;
      (snepslog::tell "perform withall({?pred, ?pred2, ?referant, ?t1}, named_rel_4(?pred, ?referant, ?t1), snif({if({oldMention(?pred, ?referant), oldMention(the_q_rel, ?referant)}, doNothing()), else(createNewMention(?pred, ?referant))}), doNothing()).") 
      
      ;; After we have accumulated a satisfactory list of new mentions, we want
      ;; to analyze as many of them as necessary.
      (snepslog::tell "perform withall(?x, newMention(the_q_rel,?x), do-all({resolveReferant(?x), say(\"New mention found\")}), doNothing()).")
      (snepslog::tell "perform withall(?x, newMention(a_q_rel,?x), do-all({resolveReferant(?x), say(\"New mention found\")}), doNothing()).")
      (snepslog::tell "perform withall(?x, newMention(udef_q_rel,?x), do-all({resolveReferant(?x), say(\"New mention found\")}), doNothing()).")
      ;;  We'll treat named referants differently for now by not doing anything
      ;;  with them.
      ;;(snepslog::tell "perform withall(?x, newMention(named_rel,?x), do-all({resolveReferant(?x), say(\"New mention found\")}), doNothing()).")
      
      ;; Update discourse model
      ;;;;  The discourse model is only good for chess matches, so far
      ;;;;  Maybe it would be worth it to make a chess package to load this
      ;;;;from
      (cond
       (*sfy-chess-mode*
	(resolve-chess-move))
       (*sfy-isa-mode*
	(resolve-isa))
       (t
	(format t "WARNING:  No discourse model analysis performed.~%"))
       )
      
      ;; Update world knowledge
;;;; END OF DANGEROUS MAGIC ;;;;
      )
      (format t "WARNING:  No parses found.~%")
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  VERIFY-STEMS
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  CHECK-FOR-NEW-NAMED-ENTITIES
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-for-new-named-entities ()
  (let ((named-entities (snask "hasName(?x,?y)?")))
    (format t "~A~%" named-entities)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  MRS-TO-SNEPS
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mrs-to-sneps (m)
  (setf *pred-list* '())
  (setf *handle-directory '())
  
  (create-predicate-lists m)
  
  (verbose-mode-only
   (format nil "mrs-to-sneps:  Predicate list~%~A~%~%" *pred-list*))
  
  (parse-predicate-list)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  CREATE-PREDICATE-LISTS
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-predicate-lists (m)
  (loop
      for r in (mrs::psoa-liszt m)
      for h = (mrs::rel-handel r)
      do
	;; Research loop syntax and possibly remove this let
	;; TODO
	(let ((fpred (string-upcase (string-trim " _" (mrs::rel-pred r)))))
	  
	  ;; Version NewYear.2:
	  ;; RELATION(label-1, label-2, ..., label-n).

	  (setf *label-number* (mrs::var-id h))
	  
	  ;; Seed a new predicate list with the name of the predicate
	  (setf *running-pred* (cons fpred '()))
	  
	  ;; Verify the existence of a suitable frame for this predicate
	  ;; This function can be found in LEXICON.CL
	  (if (not (assoc (intern (car *running-pred*)) *predicate-directory*))
	      (check-frame (car *running-pred*)))
	  
	  (setf *running-pred* (cons (mrs::var-string h) *running-pred*))
	  ;; Add the handle for the predicate to the running
	  ;; *LABEL-DIRECTORY* TODO
	  (if (not (assoc (intern (mrs::var-string h)) *label-directory*))
	      (setf *label-directory*
		(acons (intern (mrs::var-string h))
		       (intern (gensym (format nil "~A" (mrs::var-type h))))
		       *label-directory*)))
	  
	  (loop
	      for fp in (mrs::rel-flist r)
	      do
		;; TODO
		(let ((fpv (mrs::fvpair-value fp)))
		  
		  ;; If the current argument is a string, append it inside of a
		  ;; list so we can differentiate between the string h1 and the
		  ;; handle h1.
		  (if (typep fpv 'string)
		      (setf *running-pred* 
			(append *running-pred*
				(cons (cons (verify-string fpv) '()) '())))
		    ;; Otherwise, just append the label
		    (setf *running-pred*
		      (append *running-pred*
			      (cons (mrs::var-string fpv) '()))))
		  
		  ;; TODO Add the current label to the *LABEL-DIRECTORY* if it
		  ;; is not there already.  Ignore string values.
		  (if (and (not (typep fpv 'string))
			   (not (assoc (intern (mrs::var-string fpv))
				       *label-directory*)))
		      (setf *label-directory*
			(acons (intern (mrs::var-string fpv))
			       (intern (gensym
					(format nil "~A" (mrs::var-type fpv))))
			       *label-directory*)))
		  ))
	  
	  (setf *pred-list* (cons *running-pred* *pred-list*))
	  
	  )
	)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  PARSE-PREDICATE-LIST
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-predicate-list ()
  (demo-mode-only
   (format nil "parse-predicate-list:  Predicates to be tell'd~%"))
  
  (loop
      for current-pred in *pred-list*
      do
	(let ((current-tell (format nil "~A_~A(~A,~A).~%"
				    (car (cdr current-pred))
				    (length current-pred)
				    (car (cdr current-pred))
				    (generate-tell current-pred))))

	  (demo-mode-only
	   (format nil "  ~A" current-tell))
	  
	  (setf *label-number*
	    (format nil "~A"
		    (car (sntell (format nil "~A" current-tell)))))
	  ;; We'll want to remove the final ! (bang) from the end of these
	  ;; node names.
	  (setf *label-number*
	    (subseq *label-number* 0 (- (length *label-number*) 1)))
	  
	  ;; Associate the interned variable created for the MRS handle with
	  ;; the WFF returned in a QEQ relation
	  (setf *handle-directory* (acons (cdr (assoc
						(intern (car current-pred))
						*label-directory*))
					  *label-number*
					  *handle-directory*))
	  
	  ;; Associate the MRS handle for the current predicate
	  ;; with the WFF returned by the handle's creation in SNePS
	  (setf *label-directory* (acons (car current-pred)
					 *label-number*
					 *label-directory*))
	  ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  CREATE-QEQS
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-qeqs ()
  (let ((lkb::*show-parse-p* nil))	  
    (loop
	for b in (subseq sfy::*bindings* 0)
	do
	  (if (not (= (first b) (cdr b)))
	      (sntell 
	       (format nil "qeq({~A, ~A}).~%~%"
		       (cdr (assoc (intern (format nil "h~A" (first b)))
				   *label-directory*))
		       (cdr (assoc (intern (format nil "h~A" (cdr b)))
				   *label-directory*)))))
	  )
    )
  
  (create-qeqs-from-directory)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  CREATE-QEQS-FROM-DIRECTORY
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-qeqs-from-directory ()
  (loop
      for b in *handle-directory*
      do
	(sntell (format nil "qeq({~A, ~A}).~%"
			(first b)
			(cdr b)))
	)
  )
