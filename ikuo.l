(defun test-arm ()
  (setq av (send *ri* :read-angle-vector))
  (warn ";av=~A~%" av)
  (do-until-key
   (setq tv (send *ri* :torque-vector))
   (warn ";tv=~A~%" tv)
   (if (elt tv *rshoulder-))))


(defun n2r () (send *robot* :angle-vector (send *ri* :read-angle-vector)))

