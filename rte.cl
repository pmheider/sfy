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
;;;;  BATCH-RTE
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *batch-rte-file*
  (concatenate 'string *sfy-directory* "data/batch-rte.txt"))

(defun batch-rte ()
  (setf pair-count 0)
  (setf *pair-list* '())

  (with-open-file 
      (output-stream2 (concatenate 'string 
			*sfy-directory* 
			"data/rte-answers.txt") 
       :direction :output 
       :if-exists :new-version))
    
  (with-open-file
      (output-stream (concatenate 'string
		       *sfy-directory*
		       ;;(gensym (format nil "data/rte-score"))
		       "data/rte-score.txt")
       :direction :output
       :if-exists :new-version)

      (write-line
       "pair entails type len text.letters text.words text.sentences hyp.letters hyp.words hyp.sentences bag-of-words bag-of-words-by-freq bag-of-lemmas-by-freq freq-guess"
       output-stream)
      
      (with-open-file
	  (input-stream *batch-rte-file* :direction :input) 
	
	(loop
	    for stats = (read-line input-stream nil 'eof)
	    do 
	      (when (eq 'eof stats) 
		(return))
	      
	      (setf pair-count (1+ pair-count))
	      ;; TODO - make this pretty
	      (setf *pair-number* (parse-integer
				   (string-trim
				    '(#\Tab)
				    (string-trim 
				     "YES"
				     (string-trim 
				      "NO"
				      (string-trim
				       '(#\Tab)
				       (subseq stats 0 5)))))))
				   
	      (let ((text-bit (read-line input-stream nil 'eof))
		    (hypothesis-bit (read-line input-stream nil 'eof)))
		
		(let ((output-line
		       ;; (ENTAILMENT? {MORE-STATS})
		       (split-subseq-seq " "
					 (rte text-bit hypothesis-bit))))
		  
		  ;; Write the pair-number and entailment value to file
		  (with-open-file
		      (output-stream2 (concatenate 'string
					*sfy-directory*
					"data/rte-answers.txt")
		       :direction :output
		       :if-exists :append)
		    (write-line
		     (format nil "~A~T~A" *pair-number* (car output-line))
		     output-stream2))
		  
		  (write-line
		   (format nil "~A~T~A~T~A"
			   stats
			   (car (cdr output-line))
			   (car output-line))
		   output-stream))
		
		))
	))
  )

;;;; This doesn't work right ;;;;
(defun write-answers-to-file (current-pair pair-count output-stream)

  (if (< current-pair pair-count)
      (let ((values (cdr (assoc (intern current-pair)
				*pair-list*))))
	(let ((pair-number (car values))
	      (entailment? (car (cdr values))))
	  
	  (write-line 
	  
	   (write-answers-to-file (+1 current-pair)
				  pair-count
				  output-stream)
	   )))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  RTE shell command
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rte (text-bit hypothesis-bit)
  (setf *rte-score-card* '())
  
  (verbose-mode-only
   (format nil "TTTT  ~A~%HHHH  ~A~%" text-bit hypothesis-bit))

  (let ((bag-of-words-score
	 (bag-of-words text-bit hypothesis-bit)))

    (let ((simple-score
	   (cdr (assoc (intern "bag-of-words") *rte-score-card*)))
	   ;;(/ (+ 0.0 (car bag-of-words-score))
	   ;;   (car (cdr bag-of-words-score))))
	  (frequency-score
	   (cdr (assoc (intern "bag-of-words-by-freq") *rte-score-card*)))
	   ;;(/ (+ 0.0 (car (cdr (cdr bag-of-words-score))))
	   ;;   (car (cdr (cdr (cdr bag-of-words-score))))))
	  (lemma-score
	   (cdr (assoc (intern "bag-of-lemmas-by-freq") *rte-score-card*)))
	    ;;(/ (+ 0.0 (car (cdr (cdr (cdr (cdr bag-of-words-score))))))
		;;	   (car (cdr (cdr (cdr (cdr (cdr bag-of-words-score))))))))
	  )

      (let ((entailment? (rte-decider frequency-score)))
      
	(demo-mode-only
	 (format nil "Pair ~A (~A):~%  Bag of words:  ~A~%  Bag of words by frequency:  ~A~%  Bag of lemmas:  ~A~%"
		 *pair-number*
		 entailment?
		 simple-score
		 frequency-score
		 lemma-score
		 ))
		
	(format nil
		"~A ~A~T~A~T~A"
		entailment?
		simple-score
		frequency-score
		lemma-score
		))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  RTE-DECIDER
;;;;
;;;;  
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *yes-bag-of-words-mu* 0.6784131)
(setf *yes-bag-of-words-sigma* 0.2139958)

(setf *no-bag-of-words-mu* 0.4805449)
(setf *no-bag-of-words-sigma* 0.2467147)

(defun rte-decider (frequency-score)
  (let ((yes-score (z-score frequency-score
			    *yes-bag-of-words-mu*
			    *yes-bag-of-words-sigma*))
	(no-score (z-score frequency-score
			   *no-bag-of-words-mu*
			   *no-bag-of-words-sigma*)))
    (if (>= yes-score 0)
	"YES"
      (if (<= no-score 0)
	  "NO"
	"UNKNOWN"))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  GENERATE-FREQUENCY-LIST
;;;;
;;;;  
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-frequency-list ()
  (setf *word-frequency-list* '())
  (setf *lemma-frequency-list* '())
  (setf *word-frequency-file*
    (concatenate 'string *sfy-directory* "data/lemma-frequency.txt"))
  
  (with-open-file
      (input-stream *word-frequency-file* :direction :input) 
    
    (loop
	for line = (read-line input-stream nil 'eof)
	do 
	  (when (eq 'eof line) 
	    (return))
	  
	  (let ((word-and-count (split-subseq-seq " " line)))
	    
	    (let ((word (car word-and-count))
		  (lemma (lemmatize (car word-and-count)))
		  (count (normalize-word-counts
			  (parse-integer (car (cdr word-and-count))))))
	      
	      (verbose-mode-only
	       (format nil "~A (~A) : ~A~%" word lemma count))
	      
	      (setf *word-frequency-list*
		(acons (intern word) count *word-frequency-list*))
	      
	      (let ((prior-count (assoc (intern lemma) *lemma-frequency-list*)))
		(if prior-count
		    (setf *lemma-frequency-list*
		      (acons (intern lemma) (+ count (cdr prior-count))
			     *lemma-frequency-list*))
		  (setf *lemma-frequency-list*
		    (acons (intern lemma) count *lemma-frequency-list*))))
	      )
	    
	    )
	  
	  ))
	  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Z-SCORE
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun z-score (score mu sigma)
  (/ (- score mu) sigma))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  GET-WORD-FREQUENCY
;;;;  
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-word-frequency (word)
  (let ((count (assoc word *word-frequency-list*)))
    (if count
	(cdr count)
      *frequency-normalize-weight*)
    )
  )

(defun get-lemma-frequency (lemma)
  (let ((count (assoc lemma *lemma-frequency-list*)))
    (if count
	(cdr count)
      *frequency-normalize-weight*)
    )
  )

(setf *frequency-normalize-weight* 69969)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  NORMALIZE-WORD-COUNTS
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun normalize-word-counts (count)
  (if (= count 0)
      *frequency-normalize-weight*
    (- *frequency-normalize-weight* count))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  STRIP-PUNCTUATION
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun strip-punctuation (text)
  (string-delete 
   (string-delete
    (string-delete 
     (string-delete
      (string-delete
       (string-delete text ".") ";") ":") ",") ")") "(")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  STRIP-FUNCTION-WORDS
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun strip-function-words (text)
  text
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  BAG-OF-WORDS
;;;;
;;;;  
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bag-of-words (text-bit hypothesis-bit)
  (let ((text-word-list
	 (split-subseq-seq " "
			   (strip-function-words
			    (string-upcase
			     (strip-punctuation text-bit)))))
	(hyp-word-list
	 (split-subseq-seq " "
			   (strip-function-words
			    (string-upcase
			     (strip-punctuation hypothesis-bit))))))
    
    (let ((new-text-word-list (mapcar 'intern text-word-list))
	  (text-lemma-list (mapcar 'intern (mapcar 'lemmatize text-word-list)))
	  (new-hyp-word-list (mapcar 'intern hyp-word-list))
	  (hyp-lemma-list (mapcar 'intern (mapcar 'lemmatize hyp-word-list))))

      (verbose-mode-only
       (format t "TTTT  ~A~%LLLL  ~A~%HHHH  ~A~%L2L2  ~A~%" new-text-word-list text-lemma-list new-hyp-word-list hyp-lemma-list))
    
      (bag-of-words-kernel new-text-word-list text-lemma-list
			   new-hyp-word-list hyp-lemma-list
			   0 0 0 0 0 0)
      
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  BAG-OF-WORDS-KERNEL
;;;;
;;;;  
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bag-of-words-kernel (text-word-list text-lemma-list
			    hyp-word-list hyp-lemma-list
			    matches hyp-word-count 
			    match-weight words-weight
			    match-lemma-weight lemmas-weight)
  
  (if (= (length hyp-word-list) 0)
      (let ((simple-score (/ (+ 0.0 matches) hyp-word-count))
	    (frequency-score (/ (+ 0.0 match-weight) words-weight))
	    (lemma-score (/ (+ 0.0 match-lemma-weight) lemmas-weight)))
	
	(setf *rte-score-card*
	  (acons (intern "bag-of-words") simple-score
		 (acons (intern "bag-of-words-by-freq") frequency-score
			(acons (intern "bag-of-lemmas-by-freq") lemma-score
			       *rte-score-card*)))))
    (let ((repeated-word? (member (car hyp-word-list)
				  (cdr hyp-word-list)))
	  (repeated-lemma? (member (car hyp-lemma-list)
				   (cdr hyp-lemma-list))))
      (let ((update-word-match?  (and 
				  (member (car hyp-word-list) text-word-list)
				  (not repeated-word?)))
	    (update-lemma-match? (and
				  (member (car hyp-lemma-list) text-lemma-list)
				  (not repeated-lemma?))))
	
	(bag-of-words-kernel text-word-list
			     text-lemma-list
			     (cdr hyp-word-list)
			     (cdr hyp-lemma-list)
			     (if update-word-match? ;;;; MATCHES
				 (1+ matches)
			       matches)
			     (if repeated-word? ;;;; HYP-WORD-COUNT
				 hyp-word-count
			       (1+ hyp-word-count))
			     (if update-word-match? ;;;; MATCH-WEIGHT
				 (+ match-weight
				    (get-word-frequency (car hyp-word-list)))
			       match-weight)
			     (if repeated-word? ;;;; WORDS-WEIGHT
				 words-weight
			       (+ words-weight
				  (get-word-frequency (car hyp-word-list))))
			     (if update-lemma-match? ;;;; MATCH-LEMMA-WEIGHT
				 (+ match-lemma-weight
				    (get-lemma-frequency (car hyp-lemma-list)))
			       match-lemma-weight)
			     (if repeated-lemma? ;;;; LEMMAS-WEIGHT
				 lemmas-weight
			       (+ lemmas-weight
				  (get-lemma-frequency (car hyp-lemma-list)))))
	))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  SCORE-RTE-ANSWERS
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun score-rte-answers ()
  (setf *gold-standard-list* '())
  (setf correct-answers 0)
  (setf incorrect-answers 0)
  (setf unknown-answers 0)
  
  (with-open-file  
      (input-stream (concatenate 'string  
		      *sfy-directory*  
		      "data/gold-standard.txt")  
       :direction :input)

    (loop 
	for line = (read-line input-stream nil 'eof) 
	do  
	  (when (eq 'eof line)
	    (return))
	  
	  (let ((split-string (split-subseq-seq " " line)))
	    (let ((pair-number (parse-integer (car split-string)))
		  (entailment? (car (cdr split-string))))
	      (setf *gold-standard-list*
		(acons
		 pair-number
		 entailment?
		 *gold-standard-list*))
	      ))
	  ))
  
    (with-open-file  
      (input-stream (concatenate 'string  
		      *sfy-directory*  
		      "data/rte-answers.txt")
       :direction :input)
      
      (loop 
	  for line = (read-line input-stream nil 'eof) 
	  do  
	    (when (eq 'eof line)
	      (return))
	    
	    (let ((split-string (split-subseq-seq " " line)))
	      (let ((pair-number (parse-integer (car split-string)))
		    (entailment? (car (cdr split-string))))
		
		(let ((gold-entailment?
		       (cdr (assoc pair-number
				   *gold-standard-list*))))

		  (verbose-mode-only
		   (format nil "~A (~A) :: ~A~%"
			   pair-number
			   gold-entailment?
			   entailment?))
		 
		  ;; TODO - make this recursive
		  (if (string-equal entailment? "UNKNOWN")
		      (setf unknown-answers (1+ unknown-answers))
		    (if (string-equal entailment? gold-entailment?)
			(setf correct-answers (1+ correct-answers))
		      (setf incorrect-answers (1+ incorrect-answers))))
		  
		  )))

	    ))
    
    (format t "~%Correct:    ~A~%Incorrect:  ~A~%Unknown:    ~A~%"
	    correct-answers
	    incorrect-answers
	    unknown-answers)
    
    (let ((total-pairs
	   (+ 0.0 correct-answers incorrect-answers unknown-answers))
	  (answered-pairs
	   (+ 0.0 correct-answers incorrect-answers)))
      (let ((precision (/ correct-answers total-pairs))
	    (recall (/ correct-answers answered-pairs)))
	
	(format nil "~%Precision:  ~A~%Recall:     ~A~%Fbeta:      ~A~%"
		precision
		recall
		(/ (* 4 precision recall)
		   (+ (* 3 precision) (* 1 recall))))
	)
      )
    )

(setf *porter-stemmer-directory* "/projects/pmheider/porterstemmer")
(load (concatenate 'string *porter-stemmer-directory* "/porterstemmer"))

;; TODO - suppress output from porter stemmer
(defun lemmatize (str)
  (let ((lemma (cl-user:stem str)))
    (if (typep lemma 'simple-string)
	lemma
      (find-shortest (car lemma) (cdr lemma))
      )
    )
  )

(defun find-shortest (best-so-far lemma)
  (if (= (length lemma) 0)
      best-so-far
    (if (> (length best-so-far)
	   (length (car lemma)))
	(find-shortest (car lemma) (cdr lemma))
      (find-shortest best-so-far (cdr lemma)))
    )
  )
