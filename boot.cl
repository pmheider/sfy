;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                         ;;;;
;;;;                                                         ;;;;
;;;;                                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *lkb-directory* "/projects/pmheider/delphin/lkb/")
(defvar *erg-directory* "/projects/pmheider/delphin/erg_test/")
(defvar *sfy-directory* "/projects/pmheider/sfy-stable/")
(load (concatenate 'string *sfy-directory* "load_LKB_with_ERG"))

(defpackage :sfy
  (:export :show :parse))

(in-package :sfy)
(use-package :excl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                         ;;;;
;;;; Variables that may need localizing                      ;;;;
;;;;                                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *erg-directory* "/projects/pmheider/delphin/erg_test/")
(defvar *lkb-directory* "/projects/pmheider/lkb/")
(defvar *predicate-dictionary-dir* "/projects/pmheider/sfy-stable/")
(defvar *sfy-directory* "/projects/pmheider/sfy-stable/")

(defvar *load-sneps-command* "/projects/snwiz/bin/sneps")

(defparameter *use-first-parse* t)
(defparameter *bootstrap-lexicon* nil)
(defparameter *include-phonology* nil)

;; TODO:  make these mutually exclusive
(defparameter *sfy-verbose-mode* nil)
(defparameter *sfy-demo-mode* t)

(defparameter *sfy-chess-mode* nil)
(defparameter *sfy-isa-mode* nil)
(defparameter *sfy-isacan-mode* nil)
(defparameter *sfy-isa-complex-mode* nil)
(defparameter *sfy-isacan-complex-mode* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                         ;;;;
;;;; Loading script 
;;;;                                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not (find-package 'snepslog))
    (load *load-sneps-command*))

;;; set the snepslog mode and disable java show
(snepslog::tell "set-mode-3")
(setf cl-user:*use-gui-show* nil)

(load (concatenate 'string *sfy-directory* "init-globals"))
(load (concatenate 'string *sfy-directory* "file-utils"))
(load (concatenate 'string *sfy-directory* "utils"))
(load (concatenate 'string *sfy-directory* "parser"))
(load (concatenate 'string *sfy-directory* "lexicon"))
(load (concatenate 'string *sfy-directory* "init-anaphora-globals"))

;; Demos
(cond
 (*sfy-chess-mode*
  (load (concatenate 'string *sfy-directory* "demos/chess-game.demo")))
 (*sfy-isa-mode*
  (load (concatenate 'string *sfy-directory* "demos/isa-simple.demo")))
 )
