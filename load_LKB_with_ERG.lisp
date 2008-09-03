
(pushnew :tty *features*)

(load (concatenate 'string *lkb-directory*
		   "src/general/loadup"))

(pushnew :lkb *features*)

(load-system :lkb)

(lkb::read-script-file-aux (concatenate 'string *erg-directory*
					"lkb/script"))
