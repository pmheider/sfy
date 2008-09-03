;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                         ;;;;
;;;;                                                         ;;;;
;;;;                                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :sfy
  (:export :show :parse))

(in-package :sfy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; GLOBAL VARIABLES
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf lkb::*show-parse-p* nil)

(defparameter *running-tell* "")
(defparameter *running-pred* '())

(defparameter *pred-list* '())

(defparameter *label-directory* '())
(defparameter *handle-directory* '())
(defparameter *predicate-directory* '())

(defparameter *known-referant-directory* '())
(defparameter *entity-directory* '())

(defparameter *known-word-list* '())
(defparameter *unknown-word-list* '())

(defparameter *label-number* "")
(defparameter *label-type* "")

(defparameter *pred-args* '())
(defparameter *pred-names* '())

(defparameter *pos-names* '())
(defparameter *pos-args* '())

(defparameter *noun-args* '())
(defparameter *verb-args* '())
(defparameter *adj-args* '())
(defparameter *other-args* '())

(defparameter *pos-types* '())
(defparameter *n-pos-types* '())
(defparameter *v-pos-types* '())
(defparameter *a-pos-types* '())
(defparameter *lex-pred-names* '())
(defparameter *pos-arg-map* '())
(defparameter *pred-arg-map* '())

;;;; TODO make the directory actually useful in the code
(defvar *predicate-dictionary-files* (list "full_pred_dict_01"
					   "full_pred_dict_02"
					   "full_pred_dict_03"
					   "full_pred_dict_04"
					   "sneps_pred_dict"
					   "full_pred_dict_templates"))

;;;; TODO make the LKB directory also variable
(defvar *new-predicate-dictionary-file*
    (concatenate 'string *erg-directory*
		 "sneps.smi"))

(defvar *lexicon-files*
    (list (concatenate 'string *erg-directory*
		       "lexicon.tdl")
	  (concatenate 'string *erg-directory*
		       "sneps-lex.tdl")
	  (concatenate 'string *erg-directory*
		       "sneps-template-lex.tdl")))
(defvar *new-lexicon-file*
    (concatenate 'string *erg-directory*
		 "sneps-lex.tdl"))

(defparameter
    *slot-fillers* (list "noname"                   ;; proper noun
			 "gnab" "gneb" "gnib"       ;; common nouns
			 "gnabs" "gnebs" "gnibs"    ;; plural commons
			 "agnab" "agneb" "agnib"    ;; ditto for the vowel
			 "agnabs" "agnebs" "agnibs" ;; initial common nouns
			 ))

(defparameter *bindings* nil)

(defparameter *handel-index* nil)
(defparameter *handel-rlist* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                         ;;;;
;;;; Frames relating to the very abstract structure          ;;;;
;;;;                                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The thing referred to by [referant] is referred to by the 
;; (?Proper?) name [name].
(snepslog::tell "define-frame hasName(nil referant name)")

;; The thing referred to by [referant] is an instance of the
;; category [category] much like the type/token relationship
;; in linguistics.
(snepslog::tell "define-frame instanceOf(nil referant category)")

;; The string [ortho-form] has a LKB parsing that is headed by
;; the predicate labeled as [handle].
(snepslog::tell "define-frame hasHandle(nil ortho-form handle)")

;; The string [ortho-form] has one possible pronunciation
;; specified by the matryoshka list of CMU sounds in
;; [phono-form].
(snepslog::tell
 "define-frame soundsLike(nil ortho-form phono-form)")

;; The [pos]th word in the string [ortho-form] is the same as
;; the word [word]
(snepslog::tell
 "define-frame containsWord(nil ortho-form word pos)")

;;;; The following frame is less preferred than using the two
;;;; above because for now, ambiguity at the phonological level
;;;; cannot be resolved.  TODO
;; The string [ortho-form] is the orthographic equivalent to
;; the phonological string [phono-form].  One possible LKB
;; parsing of [ortho-form] is headed by the predicate labeled as
;; [handle].
;(snepslog::tell "define-frame Utterance(nil ortho-form phono-form handle)")

;; The label [label] can be used interchangeably with the any
;; other label [label] attached to the same base node with
;; respect to the current scoping interpretation.
(snepslog::tell "define-frame qeq(nil label)")

;; The thing referred to by [referant] has the same extension as any
;; other referant [referant] attached to the same base node.
(snepslog::tell "define-frame entity(nil referant)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                         ;;;;
;;;; Frames relating to anaphora resolution                  ;;;;
;;;;                                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; The referant [[referant]] has been used in a recently parsed sentence and 
;;;; was found in the predicate type [[predicate]]. 
(snepslog::tell "define-frame newMention(nil predicate newreferant)") 

;;;; The referant [[referant]] has been used in a previously parsed sentence
;;;; and was found in the predicate type [[predicate]]. 
(snepslog::tell "define-frame oldMention(nil predicate oldreferant)") 

;; To allow for SNEPS::FINDing later, we need to add path names used elsewhere
#!((define ~'pred_name ~'arg0))

;; This file has more frames for anaphora resolution as well as the primitive
;; actions needed to resolve anaphora
;; TODO:  For now, it focuses on the chess demo
(snepslog::tell "load \"anaphora-resolution.snepslog\"")

