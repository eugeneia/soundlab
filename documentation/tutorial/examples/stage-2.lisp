(defun stage-2 (trigger note)
  "Apply VOICE-ENVELOPE to STAGE-1."
  (envelope* (stage-1 note)
             (voice-envelope)
             trigger))
