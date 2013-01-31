;;;; System definition for SOUNDLAB.

(defpackage soundlab-asd
  (:documentation
   "System definition for SOUNDLAB.")
  (:use :cl :asdf))

(in-package :soundlab-asd)

(defsystem soundlab
  :description
  "A somewhat monadic digital synthesizer. Pretty mad."
  :components ((:file "packages")
	       (:file "sampling/sample-function"
		      :depends-on ("packages"))
	       (:file "sampling/sample-graph"
		      :depends-on ("packages"
				   "sampling/sample-function"))
	       (:file "sampling/sample-wave"
		      :depends-on ("packages"
				   "sampling/sample-function"))
	       (:file "waves/sine"
		      :depends-on ("packages"))
	       (:file "waves/binary"
		      :depends-on ("packages"
				   "waves/sine"))
	       (:file "waves/flatline"
		      :depends-on ("packages"))
	       (:file "waves/envelope"
		      :depends-on ("packages"
				   "tempo/bpm"
				   "combinators/repeat"))
	       (:file "notes/western"
		      :depends-on ("packages"))
	       (:file "tempo/bpm"
		      :depends-on ("packages"))
	       (:file "envelope/curves"
		      :depends-on ("packages"))
	       (:file "envelope/adsr"
		      :depends-on ("packages"
				   "envelope/curves"))
	       (:file "combinators/algebra"
		      :depends-on ("packages"))
	       (:file "combinators/chord"
		      :depends-on ("packages"
				   "combinators/algebra"))
	       (:file "combinators/mix"
		      :depends-on ("packages"
				   "combinators/algebra"
				   "waves/flatline"))
	       (:file "combinators/repeat"
		      :depends-on ("packages"))
	       (:file "combinators/dampen"
		      :depends-on ("packages"
				   "combinators/algebra"
				   "waves/flatline"))
	       (:file "combinators/pitch"
		      :depends-on ("packages"
				   "waves/flatline"))
	       (:file "combinators/envelope"
		      :depends-on ("packages"
				   "combinators/algebra")))
  :depends-on ("riff-wave"
	       "defmacro!"))
