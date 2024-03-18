(defvar n *ri*)
(defvar r *robot*)

(defun test-arm ()
  (setq av (send *ri* :read-angle-vector))
  (warn ";av=~A~%" av)
  (do-until-key
   (setq tv (send *ri* :torque-vector))
   (warn ";tv=~A~%" tv)
   (if (elt tv *rshoulder-))))


(defun n2r ()
  (send *robot* :angle-vector (send *ri* :read-angle-vector))
  (send *viewer* :draw-objects)
  )

(defun r2n ()
  (send *ri* :angle-vector (send *robot* :angle-vector) 5000)
  (send *viewer* :draw-objects)
  )

