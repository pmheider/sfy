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
;;;;  CHECK-FRAME
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-frame (predicate-name)
  ;;
  ;; PREDICATE-SPEC (based on FULL-PRED-DICT-01) will be
  ;; taken from the format:
  ;;   abstr_deg_rel : ARG0 x.
  ;;   appos_rel : ARG0 e, ARG1 x { NUM pl }, ARG2 i.
  ;;   comp_rel : ARG0 e, [ ARG1 u ], [ ARG2 u ].
  ;;   etc.
  ;;
  ;; and returned in the list format:
  ;;   '(abstr_deg_rel ARG0)
  ;;   '(appos_rel ARG0 ARG1 NUM ARG2)
  ;;   '(comp_rel ARG0 ARG1 ARG2)
  ;;   etc.
  ;;
  (verbose-mode-only
   (format nil
	   "check-frame:~%  A list of new frames to be added:~%"))
  
  (let ((specs-list (get-full-args-for-pred
		     predicate-name)))
    
    (verbose-mode-only
     (format nil
	     "  get-full-args-for-pred returned the following list:~%    ~A~%"
	     specs-list))

    (loop
	for specs in specs-list
	do
	  
	  (setf frame-definition
	    (format nil "define-frame ~A_~A(PRED_NAME HANDLE"
		    (car specs)
		    (1+ (length specs))))
	  
	  (loop
	      for tok in (cdr specs)
	      do
		(setf frame-definition
		  (format nil "~A ~A" frame-definition tok))
		)
	  
	  (setf frame-definition
	    (format nil "~A)." frame-definition))
	  
	  (verbose-mode-only (format nil "    ~A" frame-definition))
	  
	  (sntell (format nil "~A" frame-definition))
	  (setf *predicate-directory*
	    (acons (intern predicate-name)
		   frame-definition
		   *predicate-directory*))
	  )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  VERIFY-WORDS
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun verify-words (sentence)
  (setf left-half "")
  (setf right-half "")
  
  (let ((word-list
	 (split-subseq-seq " " (string-trim '(#\space #\tab #\newline) sentence))))
    (loop
	for word-token in word-list
	do
	  (format t "~%~A~%----~%" word-token)
	  (let ((word-forms (lemmatize-word word-token)))
	    (cond
	     ((assoc word-token *known-word-list*)
	      (format t "Found in KNOWN-WORD-LIST~%"))
	     ((check-lexicon-files word-forms)
	      (format t "Found in ERG lexicon~%"))
	     ((get-wordnet-synonym-values word-forms)
	      ;;((pull-wordnet-entry word-forms)
	      (format t "Found in WordNet~%"))
	     (t (let* ((trimmed-sentence (string-trim
					  '(#\space #\tab #\newline)
					  sentence))
		       (splits (split-subseq-seq
				word-token
				trimmed-sentence))
		       (first-word (car (split-subseq-seq
					 " "
					 trimmed-sentence)))
		       (last-word (car (reverse (split-subseq-seq
						 " "
						 trimmed-sentence))))
		       (lh (if (string= first-word word-token)
			       ""
			     (car splits)))
		       (rh (if (string= first-word word-token)
			       (car splits)
			     (if (string= last-word word-token)
				 ""
			       (car (cdr splits))))))
		  
		  (setf right-half rh)
		  (setf left-half lh)
		  
		  (format t "Unknown word~%")
		  (fill-slot left-half word-token right-half))
		)))
	  ;;(format t "Unknown Word:  ~A~%" word-forms))))
	  ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  FILL-SLOT  
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fill-slot (left-half word-token right-half)
  (loop
      for slot-filler in *slot-fillers*
      do
	(let ((temp-sentence
	       (format nil "~A~A~A" left-half slot-filler right-half)))
	  ;; Parse the temporary sentences
	  (let ((lkb::*show-parse-p* nil))
	    (lkb::do-parse-tty temp-sentence)
	    
	    (verbose-mode-only (format nil "~%~A~%Parse Count:  ~A"
				       temp-sentence
				       (length lkb::*parse-record*)))
	    (if (> (length lkb::*parse-record*) 0)
		;; Don't forget to lemmatize this slot-filler, eventually
		(generate-from-synonym-model word-token slot-filler))
	    )
	  )
	)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  LEMMATIZE-WORD
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lemmatize-word (word-token)
  (cons (stem word-token) (cons word-token '()))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  CHECK-LEXICON-FILES
;;;;    It would be nice to separate out this function
;;;;    enough to put half of it under file-utils
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-lexicon-files (word-forms)
  "Using *LEXICON-FILES*, open the lexicon and read its contents until it finds an entry containing the string matching one of the WORD-FORMS.  Return that entry."
  (setf predicate-spec "")
  (loop for filename in *lexicon-files*
      do
	(if (string= predicate-spec "")
	    (with-open-file
		(input-stream filename :direction :input)
	      (loop for line = (read-line input-stream nil 'eof)
		  do
		    (when (or (eq 'eof line)
			      (not (string= predicate-spec "")))
		      (return))
		    ;; If we split across := and get two
		    ;; arguments, set the first to our pred-name
		    ;; and read the next line for the stem value
		    ;; -- if this stem value matches any of the
		    ;;    word forms
		    ;; -- return the predicate form
		    ;; if not, keep trucking
		    (if (cdr (split-subseq-seq
			      ":=" line))
			(let ((stem-line
			       (read-line
				input-stream
				nil
				'eof)))
			  (loop for word-tok in word-forms
			      do
				(if (string=
				     (car (cdr 
				       (split-subseq-seq "\"" stem-line)))
					     word-tok)
				    (setf *known-word-list*
				      (acons (car word-forms) t
					     *known-word-list*)))
				)))))))
  (assoc (car word-forms) *known-word-list*))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  GET-WORDNET-SYNONYM-VALUES
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun get-wordnet-synonym-values (word-forms)
  (setf noun-pos-types '())
  (setf verb-pos-types '())
  (setf adj-pos-types '())
  (setf adv-pos-types '())
  
  (setf noun-pred-names '())
  (setf verb-pred-names '())
  (setf adj-pred-names '())
  (setf adv-pred-names '())
  
  (loop
      for word-token in word-forms
      do
	(let ((synonym-list (get-all-synonyms word-token)))
	  ;;(loop
	  ;;    for pos-synonyms in synonym-list
	  ;;    do
	  ;; The above loop should be reinstated (perhaps
	  ;; with a more elegant design so we don't have to
	  ;; repeate the same algorithm for each *pos-types assoc
	  ;; list
	  (let ((pos-synonyms (car synonym-list)))
	    (loop
		for a-synonym in pos-synonyms
		do
		  (let ((pos-and-pred-pairs (get-pos-and-pred a-synonym)))
		    (loop
			for pos-and-pred-pair in pos-and-pred-pairs
			do
			  (let* ((pos-type (car pos-and-pred-pair))
				 (pred-name (car (cdr pos-and-pred-pair)))
				 (pos-count (cdr (assoc
						  (intern pos-type)
						  noun-pos-types)))
				 (pred-count (cdr (assoc
						   (intern pred-name)
						   noun-pred-names))))

			    (write-new-lexicon-to-file
			     (generate-lexicon-entry word-token pos-type pred-name))
			    (write-new-predicate-to-file
			     (generate-predicate-entry word-token pred-name))
			    
			    (setf noun-pos-types
			      (acons (intern pos-type)
				     (if pos-count
					 (+ pos-count 1)
				       1)
				     noun-pos-types))
			    
			    (setf noun-pred-names
			      (acons (intern pred-name)
				     (if pred-count
					 (+ pred-count 1)
				       1)
				     noun-pred-names))
			    )))))))
  
  ;;(format t "~%---N POS Types---")
  ;;(list-assoc-vals-gt-x noun-pos-types 0)
  
  ;;(format t "~%---N Pred Names---")
  ;;(list-assoc-vals-gt-x noun-pred-names 0)
  
  (if (> (length noun-pos-types) 0)
      t
    nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  GET-ONSET-VALUE
;;;;    given a word WORD-TOKEN, return whether the first
;;;;    character is a vowel VOC or consonant CON
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-from-synonym-model (word-token a-synonym)
  (let ((pos-and-pred-pairs (get-pos-and-pred a-synonym)))
    (loop
	for pos-and-pred-pair in pos-and-pred-pairs
	do
	  (let* ((pos-type (car pos-and-pred-pair))
		 (pred-name (car (cdr pos-and-pred-pair))))

	    (write-new-lexicon-to-file
	     (generate-lexicon-entry word-token pos-type pred-name))
	    (write-new-predicate-to-file
	     (generate-predicate-entry word-token pred-name))
		 ))))
			    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  GET-ONSET-VALUE
;;;;    given a word WORD-TOKEN, return whether the first
;;;;    character is a vowel VOC or consonant CON
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-onset-value (word-token)
  (case (char word-token 0)
    ((#\a #\e #\i #\o #\u) "VOC")
    (otherwise "CON")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  GENERATE-NEW-PRED-NAME
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-new-pred-name (word-token template-pred-name)
  (format nil "~%~A -> ~A" word-token template-pred-name)
  
  (let ((old-word-token (car (split-subseq-seq "_" template-pred-name))))
    (string-replace template-pred-name old-word-token word-token)))
		  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  GENERATE-LEXICON-ENTRY
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-lexicon-entry (word-token pos-type template-pred-name)
  (format nil "~A := ~A &~% [ STEM < \"~A\" >,~%   SYNSEM [ LKEYS.KEYREL.~A \"~A\",~%            PHON.ONSET ~A ] ]."
	  (gensym (format nil "~A_~A"
			  word-token (car (split-subseq-seq "_" pos-type))))
	  pos-type
	  word-token
	  (if (> (length (split-subseq-seq "_pn_" pos-type)) 1)
	      "CARG"
	    "PRED")
	  (generate-new-pred-name word-token template-pred-name)
	  (get-onset-value word-token)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  GENERATE-PREDICATE-ENTRY
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-predicate-entry (word-token template-pred-name)
  (format nil "~A : ~A"
	  (generate-new-pred-name word-token template-pred-name)
	  (car (cdr (split-subseq-seq
		     " : "
		     (get-full-args-for-pred 
		      (string-upcase
		       (string-left-trim "_" template-pred-name))))))))
