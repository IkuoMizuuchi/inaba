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

(defun demo ()
  (send *ri* :hold)
  (send *robot* :neutral)
  (prepare-for-bucket)
  (r2n)
  (warn "push ENTER~%")
  (do-until-key)
  (hold-arm)
  )

;; 角度もトルクも、閉じる方向：rsはプラス、ls,re,le,rw,lwはマイナス
(setq *torque-target* (list (cons :rs -20.0) (cons :re 20.0) (cons :rw 20.0)
			    (cons :ls  20.0) (cons :le 20.0) (cons :lw 20.0)))
(setq *torque-eps* 10.0)
(setq *torque-max* 100.0)
(setq *joint-eps* 5.0)
(setq *joint-move-time* (floor (* *joint-eps* 500)))

(defun hold-arm ()
  (let* (av tv
	 (rs (send *robot* :rarm :shoulder-r :no))
	 (re (send *robot* :rarm :elbow-p :no))
	 (rw (send *robot* :rarm :wrist-p :no))
	 (ls (send *robot* :larm :shoulder-r :no))
	 (le (send *robot* :larm :elbow-p :no))
	 (lw (send *robot* :larm :wrist-p :no))
	 (jlist (list rs re rw ls le lw))
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

(defun prepare-for-bucket ()
  (send *robot* :arms :shoulder-p :joint-angle -80)
  (send *robot* :rarm :shoulder-y :joint-angle  90)
  (send *robot* :larm :shoulder-y :joint-angle -90)
  (send *robot* :rarm :shoulder-r :joint-angle -30)
  (send *robot* :larm :shoulder-r :joint-angle  30)
  (send *robot* :arms :elbow-p    :joint-angle -10)
  (send *robot* :arms :wrist-p    :joint-angle  75)
  (send *robot* :angle-vector)
  )

(defun n2r ()
  (send *robot* :angle-vector (send *ri* :read-angle-vector))
  (send *robot-viewer* :draw-objects)
  )

(defun r2n ()
  (send *ri* :angle-vector (send *robot* :angle-vector) 5000)
  (send *robot-viewer* :draw-objects)
  )

(defun at-watch ()
  (let* ((rs (send *robot* :rarm :shoulder-r :no))
	 (re (send *robot* :rarm :elbow-p :no))
	 (rw (send *robot* :rarm :wrist-p :no))
	 (ls (send *robot* :larm :shoulder-r :no))
	 (le (send *robot* :larm :elbow-p :no))
	 (lw (send *robot* :larm :wrist-p :no))
	 av tv)
    (do-until-key
     (setq av (send *ri* :read-angle-vector))
     (setq tv (send *ri* :read-torque-vector))
     (warn "rs:~5,1f(~6,1f) re:~5,1f(~6,1f) rw:~5,1f(~6,1f) | ls:~5,1f(~6,1f) le:~5,1f(~6,1f) lw:~5,1f(~6,1f)~%"
	   (elt av rs) (elt tv rs) (elt av re) (elt tv re) (elt av rw) (elt tv rw)
	   (elt av ls) (elt tv ls) (elt av le) (elt tv le) (elt av lw) (elt tv lw)
	   )
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
