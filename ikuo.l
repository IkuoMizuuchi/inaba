(defvar n *ri*)
(defvar r *robot*)
(defvar *robot-viewer*
  (prog1
    (make-irtviewer :title "Robot Viewer")
    (objects *robot*)
    ))

(defvar *jname-avpos-list* ;; sign means direction to open arms
  (list (cons :rarm-shoulder-r -5)
	(cons :rarm-elbow-p 7)
	(cons :rarm-wrist-p 8)
	;;
	(cons :larm-shoulder-r 13)
	(cons :larm-elbow-p 15)
	(cons :larm-wrist-p 16)
	;;
	(cons :rarm-shoulder-p 4) ;; downward is positive
	(cons :larm-shoulder-p 12) ;; downward is positive
	))

(defvar *torque-eps* 10.0)
(defvar *joint-eps* 5.0)

(defun test-arm ()
  (let (av tv
	(rs (send *robot* :rarm :shoulder-r :no))
	(re (send *robot* :rarm :elbow-p :no))
	(rw (send *robot* :rarm :wrist-p :no))
	(ls (send *robot* :larm :shoulder-r :no))
	(le (send *robot* :larm :elbow-p :no))
	(lw (send *robot* :larm :wrist-p :no))
	)
    (setq av (send *ri* :read-angle-vector))
    (warn ";av=~A~%" av)
    (n2r)
    ;;;
    (do-until-key
     (setq tv (send *ri* :read-torque-vector))
     (warn ";(t:~A) tv=~A~%" (elt tv rs) tv)
     (warn " ;(a:~A) av=~A~%" (elt av rs) av)
     (cond
      ((< *torque-eps* (elt tv rs))
       (incf (elt av rs) *joint-eps*))
      ((< (elt tv rs) (- *torque-eps*))
       (decf (elt av rs) *joint-eps*))
      (t (warn "av is not changed~%")))
     (send *ri* :angle-vector av)
     (warn " ;(a:~A) av=~A~%" (elt av rs) av)
     (send *ri* :wait-interpolation)
     )))

(defun n2r ()
  (send *robot* :angle-vector (send *ri* :read-angle-vector))
  (send *robot-viewer* :draw-objects)
  )

(defun r2n ()
  (send *ri* :angle-vector (send *robot* :angle-vector) 5000)
  (send *robot-viewer* :draw-objects)
  )

(defun returnp (strm &key (timeout 0.001))
  (let ((fdset) ;; (make-array 256 :element-type :bit))
        (sifd))
    (setq sifd (send strm :infd))
    (if (not (integerp sifd)) (return-from returnp nil))
    (setq fdset (make-array (1+ sifd) :element-type :bit))
    (setf (elt fdset sifd) 1)
    (if (null timeout) (setq timeout 0))
    (= 1 (unix:select fdset nil nil timeout))))
