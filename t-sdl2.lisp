(ql:quickload "sdl2")
(ql:quickload "sdl2-image")


(defclass sprite ()
  ((picture :initarg :picture :accessor sprite-picture)
   (x :initarg :x :accessor sprite-x)
   (y :initarg :y :accessor sprite-y)
   (w :initarg :w :accessor sprite-w)
   (h :initarg :h :accessor sprite-h)
   (center :initarg :center :accessor sprite-center :initform '(0 0))
   (image :accessor sprite-image :initform nil)
   (rect :accessor sprite-rect :initform nil)))


(defclass player (sprite)
  ((speed :initarg :speed :accessor player-speed)
   (vel-x :initarg :vel-x :accessor player-vel-x :initform 0)
   (vel-y :initarg :vel-y :accessor player-vel-y :initform 0)
   (move-vel-x :accessor player-move-vel-x :initform 0)
   (move-vel-y :accessor player-move-vel-y :initform 0)
   (future :accessor player-future :initform nil)
   (airborne :accessor player-airborne :initform t)
   ))



(defun get-center (x y w h)
  `(,(float (+ x (/ w 2)))
    ,(float (+ y (/ h 2)))
    ))

(defmethod sum-vel ((e player))
  `(,(+ (player-vel-x e) (player-move-vel-x e))
    ,(+ (player-vel-y e) (player-move-vel-y e)))
  )

(defmethod sum-vel-x ((e player))
  (car (sum-vel e)))

(defmethod sum-vel-y ((e player))
  (cadr (sum-vel e)))

(defclass cell (sprite)
  ((type :initarg :t :accessor cell-t)
   (passable :initarg :passable? :accessor cell-passable? :initform nil)
   (visible :initarg :visible? :accessor cell-visible? :initform t)))




(defparameter *screen-width* 800)
(defparameter *screen-height* 600)
(defparameter *cell-w* (/ *screen-width* 20));делаем ячейки такими
(defparameter *cell-h* (/ *screen-height* 20));чтобы карта была 20х20
(defparameter *g* 5)



(defun make-cell-list (end &key y visible passable)
  (do ((i 0 (+ i 1))
       (ret '()))
      ((>= i end) ret)
    (push (make-instance 'cell
			 :x (* i *cell-w*)
			 :y y
			 :picture "rock.png"
			 :w *cell-w*
			 :h *cell-h*
			 :center (get-center (* i *cell-w*) y *cell-w* *cell-h*)
			 :passable? passable
			 :visible? visible)
	  ret)
    )
  )



(defun some-strange-shit (end)
  (do ((j 0 (+ j 1))
       (ret '()))
      ((>= j end) ret)
    (if (or (< j 2) (>= j (- end 2)))
	(push
	 (make-cell-list end
			 :y (* j *cell-h*)
			 :visible t
			 )
	 ret)
	(push
	 (make-cell-list end
			 :y (* j *cell-h*)
			 :passable t
			 )
	 ret)
	)
    ))



(defparameter *map* (make-array '(20 20) :initial-contents (some-strange-shit 20)))



(defparameter *player* (make-instance 'player :x 50 :y 100 :w 30 :h 50 :speed 8 :picture "cobra.png"))

(defmethod load-texture (renderer (e sprite))
  (setf (sprite-image e)
	(sdl2:create-texture-from-surface
	 renderer
	 (sdl2-image:load-image (sprite-picture e)))))

(defmethod make-rect ((e sprite) &key (vel-x 0) (vel-y 0))
  (setf (sprite-rect e)
	(sdl2:make-rect
	 (+ (sprite-x e) vel-x)
	 (+ (sprite-y e) vel-y)
	 (sprite-w e)
	 (sprite-h e))))



(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
	 ,@body))))

(defun load-surface (filename &optional pixel-format)
  (sdl2:convert-surface-format (sdl2-image:load-image filename) pixel-format))


(defmethod load-texture (renderer filename)
  (sdl2:create-texture-from-surface renderer (sdl2-image:load-image filename)))

(defmacro test-check(var-m then-m else-m)
  `(if (some (lambda (x) (equal t x))
	     (mapcar #'check-collision ,var-m))
      ,then-m
      ,else-m))




(defmethod get-center-s ((e sprite))
  `(,(float (+ (sprite-x e) (/ (sprite-w e) 2)))
    ,(float (+ (sprite-y e) (/ (sprite-h e) 2)))))

(defun main(argv)
  (declare (ignore argv))
  (map-mod-wall 0)
  (with-window-renderer (window renderer)
    (sdl2-image:init '(:png))
    (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
    (init-player renderer)
    (init-image-and-rect-in-map renderer)
    (let ((unpass (make-list-unpassable-cell)))
      ;(format t "GGGG ~A~%" unpass)
      (sdl2:with-event-loop (:method :poll)
	(:quit () t)
	(:keydown (:keysym keysym)
		  (case (sdl2:scancode keysym)
		    (:scancode-space (if(= *g* 1)
					(progn
					  (setf *g* 0)
					  (setf (player-vel-y *player*) 0))
					(setf *g* 1)))
		    (:scancode-up (progn
				    (setf (player-airborne *player*) t)
				    (setf (player-vel-y *player*) (- 10))))
		    (:scancode-down t)
		    (:scancode-left (setf (player-vel-x *player*) (- (player-speed *player*))))
		    (:scancode-right (setf (player-vel-x *player*)  (player-speed *player*)))
		    (t ()))
		  
		  )
	(:idle ()
	       ;; (when (player-airborne *player*)
	       ;; 	 (when (< (player-move-vel-y *player*) *g*)
	       ;; 	   (setf (player-move-vel-y *player*) *g*)))
	       
	       ;(when (or (/= (sum-vel-x *player*) 0) (/= (sum-vel-y *player*) 0))
	       (check-future unpass);)
	       ;; (unless (player-airborne *player*)
	       ;; 	 (unless (/= (player-move-vel-y *player*) 0)
	       ;; 	   (progn
	       ;; 	     (format t "GROUND ~A ~A~%" (player-vel-y *player*) (player-move-vel-y *player*))
	       ;; 	     (setf (player-move-vel-y *player*) 0))))
	       
	       (when (player-future *player*)
	       	 (setf (sprite-rect *player*) (player-future *player*)))
	       (sdl2:render-clear renderer)
	       (render-map renderer)
	       (sdl2:render-copy renderer (sprite-image *player*) :dest-rect (sprite-rect *player*))
	       (sdl2:render-present renderer)
	       (sdl2:delay 20)
	       )))))


(defun render-map (renderer)
  (dotimes (i (car (array-dimensions *map*)))
    (dotimes (j (cadr (array-dimensions *map*)))
      (when (cell-visible? (aref *map* i j))
	(sdl2:render-copy
	 renderer
	 (sprite-image (aref *map* i j))
	 :dest-rect (make-rect (aref *map* i j))))
      )))


(defun check-future (unpass)
  (let* ((colis-t nil)
	(pl-center (get-center-s *player*))
	(ground (remove-if
		   (lambda (x)(<= (cadr (sprite-center x)) (+ (cadr pl-center) (/ (sprite-h *player*) 2))))
		   unpass))
	(not-ground (remove-if-not
		   (lambda (x)(<= (cadr (sprite-center x)) (+ (cadr pl-center) (/ (sprite-h *player*) 2))))
		   unpass)))
    (setf (player-future *player*)
	  (make-rect *player*
		     :vel-x (sum-vel-x *player*)
		     :vel-y (sum-vel-y *player*)))
    (let ((check-g (remove-if-not (lambda (x) (check-collision x)) ground)))
      (if check-g
	  (progn
	    (setf colis-t t)
	    (setf (sprite-y *player*) (- (sprite-y (car check-g)) (sprite-h *player*) 0))
	    (setf (sprite-rect *player*) (make-rect *player*))
	    (format t "DNO ~A~%" (mapcar (lambda (x) (sprite-rect x)) check-g))
	    (setf (player-airborne *player*) nil))
	  t)
    )
    (test-check not-ground
		(setf colis-t t)
		t)
    (if colis-t
	(setf (player-future *player*) nil)
	(progn
	  (setf (sprite-x *player*)
		(+ (sprite-x *player*) (sum-vel-x *player*)))
	  (setf (sprite-y *player*)
		(+ (sprite-y *player*) (sum-vel-y *player*))))
	)
    
    (unless (player-airborne *player*)(setf (player-vel-x *player*) 0))
    (if (and (player-airborne *player*)(< (player-vel-y *player*) *g*))
	(setf (player-vel-y *player*) (+ (player-vel-y *player*) *g*))
	(progn
	  (setf (player-move-vel-y *player*) 0)
	  (setf (player-vel-y *player*) 0))))
  )

(defun init-image-and-rect-in-map (renderer)
  (dotimes (i (car (array-dimensions *map*)))
    (dotimes (j (cadr (array-dimensions *map*)))
      (load-texture renderer (aref *map* i j))
      (make-rect (aref *map* i j))
      )
    ))

(defun init-player (renderer)
  (load-texture renderer *player*)
  (make-rect *player*))


(defun check-collision (rect)
  (sdl2:has-intersect (player-future *player*) (sprite-rect rect)))

(defun make-list-unpassable-cell ()
  (let ((ret '()))
   (dotimes (i (car (array-dimensions *map*)))
    (dotimes (j (cadr (array-dimensions *map*)))
      (unless (cell-passable? (aref *map* i j))
	(push (aref *map* i j) ret))
      ))
    ret))

(defun map-mod-wall (y)
  (dotimes (i (car (array-dimensions *map*)))
    (when (cell-passable? (aref *map* i y))
      (setf (cell-passable? (aref *map* i y)) nil)
      (setf (cell-visible? (aref *map* i y)) t)
      )))
