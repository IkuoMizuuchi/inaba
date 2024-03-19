(defvar n *ri*)
(defvar r *robot*)
(defvar *robot-viewer*
  (prog1
    (make-irtviewer :title "Robot Viewer")
    (objects *robot*)
    ))

;;結局使ってないが、何番目がどの関節か見やすいので置いておく
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

(if (not (boundp '*with-legs*))
    (setq *with-legs*
	  (cond
	   ((string= (unix:getenv "ROBOT_NAME") "kxrl2l6a6h2m") t)
	   ((string= (unix:getenv "ROBOT_NAME") "kxrl2l2a6h2m") nil)
	   (t (warn ";; ROBOT_NAME=~A~%" (unix:getenv "ROBOT_NAME"))))))

(defun demo ()
  (send *ri* :hold)
  (send *robot* :neutral)
  (prepare-for-bucket)
  (r2n)
  (warn "push ENTER~%")
  (do-until-key)
  (hold-arm)
  )

(setq *torque-target* (list
;; 角度もトルクも、閉じる方向：rsはプラス、ls,re,le,rw,lwはマイナス
		       (cons :rs -20.0) (cons :re 20.0) (cons :rw 20.0)
		       (cons :ls  20.0) (cons :le 20.0) (cons :lw 20.0)
;; 角度もトルクも、立ち上がる方向crotch,ankleはプラス、kneeはマイナス
		       (cons :rc 1.0) (cons :rk -1.0) (cons :ra 1.0)
		       (cons :lc 1.0) (cons :lk -1.0) (cons :la 1.0)))
(setq *torque-eps* 10.0)
(setq *torque-max* 150.0)
(setq *joint-eps* 1.0)
(setq *joint-move-time* (floor (* *joint-eps* 700)))

(defun hold-arm ()
  (let* (av tv
	 (rs (send *robot* :rarm :shoulder-r :no))
	 (re (send *robot* :rarm :elbow-p :no))
	 (rw (send *robot* :rarm :wrist-p :no))
	 (ls (send *robot* :larm :shoulder-r :no))
	 (le (send *robot* :larm :elbow-p :no))
	 (lw (send *robot* :larm :wrist-p :no))
	 (jalist (list (cons :rs rs) (cons :re re) (cons :rw rw)
		       (cons :ls ls) (cons :le le) (cons :lw lw)))
	 )
    (setq av (send *ri* :read-angle-vector))
    (warn ";(a:~5,1f ~5,1f ~5,1f | ~5,1f ~5,1f ~5,1f) av=~A~%"
	  (elt av rs) (elt av re) (elt av rw)
	  (elt av ls) (elt av le) (elt av lw) av)
    (n2r)
    ;;;
    (do-until-key
     (setq tv (send *ri* :read-torque-vector))
     (warn ";(t:~6,1f ~6,1f ~6,1f | ~6,1f ~6,1f ~6,1f) tv=~A~%"
	   (elt tv rs) (elt tv re) (elt tv rw)
	   (elt tv ls) (elt tv le) (elt tv lw) tv)
     (warn ";(a:~5,1f ~5,1f ~5,1f | ~5,1f ~5,1f ~5,1f) av=~A~%"
	   (elt av rs) (elt av re) (elt av rw)
	   (elt av ls) (elt av le) (elt av lw) av)
     (dolist (j jalist)
       (cond
	((or (< *torque-max* (elt tv (cdr j)))
	     (< (elt tv (cdr j)) (- *torque-max*)))
	 (warn "too much torque(~6,1f~A)~%" (elt tv (cdr j)) (car j)))
	((< (+ (cdr (assq (car j) *torque-target*)) *torque-eps*) (elt tv (cdr j)))
	 (incf (elt av (cdr j)) *joint-eps*)
	 (warn "av(~5,1f~A) increased~%" (elt av (cdr j)) (car j)))
	((< (elt tv (cdr j)) (- (cdr (assq (car j) *torque-target*)) *torque-eps*))
	 (decf (elt av (cdr j)) *joint-eps*)
	 (warn "av(~5,1f~A) decreased~%" (elt av (cdr j)) (car j)))
	(t
	 ;; (warn "av(~5,1f~A) is not changed~%" (elt av (cdr j)) (car j))
	 ))
       )
     (send *ri* :angle-vector av *joint-move-time*)
     (warn " ;(a:~5,1f ~5,1f ~5,1f | ~5,1f ~5,1f ~5,1f) av=~A~%"
	   (elt av rs) (elt av re) (elt av rw)
	   (elt av ls) (elt av le) (elt av lw) av)
     (send *ri* :wait-interpolation)
     )))

(defun stand-up ()
  (let* (av tv
	 (rc (send *robot* :rleg :crotch-p :no))
	 (rk (send *robot* :rleg :knee-p :no))
	 (ra (send *robot* :rleg :ankle-p :no))
	 (lc (send *robot* :lleg :crotch-p :no))
	 (lk (send *robot* :lleg :knee-p :no))
	 (la (send *robot* :lleg :ankle-p :no))
	 (jalist (list (list :crotch :rc rc :lc lc (* *joint-eps* 0.5))
		       (list :knee   :rk rk :lk lk (* *joint-eps* 1.0))
		       (list :ankle  :ra ra :la la (* *joint-eps* 0.5))))
	 t-ave t-tgt
	 )
    (setq av (send *ri* :read-angle-vector))
    (setq tv (send *ri* :read-torque-vector))
    (at-show av tv)
    (n2r)
    ;;;
    (do-until-key
     (setq tv (send *ri* :read-torque-vector))
     (dolist (js jalist)
       (setq t-ave (/ (+ (elt tv (third js)) (elt tv (fifth js))) 2.0))
       (setq t-tgt (/ (+ (cdr (assq (second js) *torque-target*))
			 (cdr (assq (fourth js) *torque-target*))) 2.0))
       (cond
	((or (< *torque-max* t-ave)
	     (< t-ave (- *torque-max*)))
	 (warn "too much torque(~6,1f~A)~%" t-ave (first js)))
	((< (+ t-tgt *torque-eps*) t-ave)
	 (incf (elt av (third js)) (sixth js))
	 (incf (elt av (fifth js)) (sixth js))
	 (warn "av(~5,1f~A) increased~%" (elt av (third js)) (first js)))
	((< t-ave (- t-tgt *torque-eps*))
	 (decf (elt av (third js)) (sixth js))
	 (decf (elt av (fifth js)) (sixth js))
	 (warn "av(~5,1f~A) decreased~%" (elt av (third js)) (first js)))
	(t
	 ;; (warn "av(~5,1f~A) is not changed~%" (elt av (cdr j)) (car j))
	 ))
       )
     (send *ri* :angle-vector av *joint-move-time*)
     (at-show av tv)
     (send *ri* :wait-interpolation)
     )))

(defun prepare-for-bucket ()
  (send *robot* :arms :shoulder-p :joint-angle -80)
  (send *robot* :rarm :shoulder-y :joint-angle  90)
  (send *robot* :larm :shoulder-y :joint-angle -90)
  (send *robot* :rarm :shoulder-r :joint-angle -30)
  (send *robot* :larm :shoulder-r :joint-angle  30)
  (send *robot* :arms :elbow-p    :joint-angle -10)
  (send *robot* :arms :wrist-p    :joint-angle  75)
  (when *with-legs*
    (send *robot* :legs :crotch-y :joint-angle   0)
    (send *robot* :legs :crotch-p :joint-angle -60)
    (send *robot* :legs :crotch-r :joint-angle   0)
    (send *robot* :legs :knee-p   :joint-angle 125)
    (send *robot* :legs :ankle-p  :joint-angle -65)
    (send *robot* :legs :ankle-r  :joint-angle   0)
    )
  (send *robot* :angle-vector)
  )

(defun arm-down ()
  (send *robot* :arms :shoulder-p :joint-angle 0)
  (send *robot* :angle-vector))

(defun n2r ()
  (send *robot* :angle-vector (send *ri* :read-angle-vector))
  (send *robot-viewer* :draw-objects)
  )

(defun r2n (&optional (msec 5000))
  (send *ri* :angle-vector (send *robot* :angle-vector) msec)
  (send *robot-viewer* :draw-objects)
  )

(defun at-show (av tv)
  (let* ((rs (send *robot* :rarm :shoulder-r :no))
	 (re (send *robot* :rarm :elbow-p :no))
	 (rw (send *robot* :rarm :wrist-p :no))
	 (ls (send *robot* :larm :shoulder-r :no))
	 (le (send *robot* :larm :elbow-p :no))
	 (lw (send *robot* :larm :wrist-p :no))
	 ;;
	 (rc (send *robot* :rleg :crotch-p :no))
	 (rk (send *robot* :rleg :knee-p :no))
	 (ra (send *robot* :rleg :ankle-p :no))
	 (lc (send *robot* :lleg :crotch-p :no))
	 (lk (send *robot* :lleg :knee-p :no))
	 (la (send *robot* :lleg :ankle-p :no))
	 )
    (warn "cr:~5,1f(~6,1f),~5,1f(~6,1f) kn:~5,1f(~6,1f),~5,1f(~6,1f) an:~5,1f(~6,1f)~5,1f(~6,1f) | sh:~5,1f(~6,1f),~5,1f(~6,1f) el:~5,1f(~6,1f)~5,1f(~6,1f) wr:~5,1f(~6,1f),~5,1f(~6,1f)~%"
	  (elt av rc) (elt tv rc) (elt av lc) (elt tv lc)
	  (elt av rk) (elt tv rk) (elt av lk) (elt tv lk)
	  (elt av ra) (elt tv ra) (elt av la) (elt tv la)
	  (elt av rs) (elt tv rs) (elt av ls) (elt tv ls)
	  (elt av re) (elt tv re) (elt av le) (elt tv le)
	  (elt av rw) (elt tv rw) (elt av lw) (elt tv lw)
	  )
    ))

(defun at-watch ()
  (let (av tv)
    (do-until-key
     (setq av (send *ri* :read-angle-vector))
     (setq tv (send *ri* :read-torque-vector))
     (at-show av tv)
     (unix:sleep 1)
     )))

(defun torque-watch ()
  (let* ((rs (send *robot* :rarm :shoulder-r :no))
	 (re (send *robot* :rarm :elbow-p :no))
	 (rw (send *robot* :rarm :wrist-p :no))
	 (ls (send *robot* :larm :shoulder-r :no))
	 (le (send *robot* :larm :elbow-p :no))
	 (lw (send *robot* :larm :wrist-p :no))
	 tv)
    (do-until-key
     (setq tv (send *ri* :read-torque-vector))
     (warn "rs:~6,1f re:~6,1f rw:~6,1f | ls:~6,1f le:~6,1f lw:~6,1f~%"
	   (elt tv rs) (elt tv re) (elt tv rw)
	   (elt tv ls) (elt tv le) (elt tv lw))
     (unix:sleep 1)
     )))

(defun returnp (strm &key (timeout 0.001))
  (let ((fdset) ;; (make-array 256 :element-type :bit))
        (sifd))
    (setq sifd (send strm :infd))
    (if (not (integerp sifd)) (return-from returnp nil))
    (setq fdset (make-array (1+ sifd) :element-type :bit))
    (setf (elt fdset sifd) 1)
    (if (null timeout) (setq timeout 0))
    (= 1 (unix:select fdset nil nil timeout))))
