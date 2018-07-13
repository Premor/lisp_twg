(ql:quickload "sdl2")
(ql:quickload "sdl2-image")


(defclass sprite ()
  ((picture :initarg :picture :accessor sprite-picture)
   (x :initarg :x :accessor sprite-x)
   (y :initarg :y :accessor sprite-y)
   (w :initarg :w :accessor sprite-w)
   (h :initarg :h :accessor sprite-h)
   (image :accessor sprite-image :initform nil)
   (rect :accessor sprite-rect :initform nil)))


(defclass player (sprite)
  ((speed :initarg :speed :accessor player-speed)))

(defclass cell (sprite)
  ((type :initarg :t :accessor cell-t)
   (passable :initarg :passable? :accessor cell-passable? :initform nil)
   (visible :initarg :visible? :accessor cell-visible? :initform t)))




(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *cell-w* (/ *screen-width* 20));делаем ячейки такими
(defparameter *cell-h* (/ *screen-height* 20));чтобы карта была 20х20




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



(defparameter *player* (make-instance 'player :x 15 :y 200 :w 30 :h 30 :speed 8 :picture "cobra.png"))

(defmethod load-texture (renderer (e sprite))
  (setf (sprite-image e)
	(sdl2:create-texture-from-surface
	 renderer
	 (sdl2-image:load-image (sprite-picture e)))))

(defmethod make-rect ((e sprite))
  (setf (sprite-rect e)
	(sdl2:make-rect
	 (sprite-x e)
	 (sprite-y e)
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
	     (mapcar 'check-collision ,var-m))
      ,then-m
      ,else-m))

(defun main(argv)
  (declare (ignore argv))
  (with-window-renderer (window renderer)
    (sdl2-image:init '(:png))
    (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
    (init-player renderer)
    (init-image-and-rect-in-map renderer)
    (let ((unpass (make-list-unpassable-cell)))
      (format t "GGGG ~A~%" unpass)
      (sdl2:with-event-loop (:method :poll)
	(:quit () t)
	(:keydown (:keysym keysym)
		  (case (sdl2:scancode keysym)
		    (:scancode-up (test-check unpass
					   (setf (sprite-y *player*)
						 (+ (sprite-y *player*) (player-speed *player*)))
					   (setf (sprite-y *player*)
						 (- (sprite-y *player*) (player-speed *player*)))))
		    (:scancode-down (setf (sprite-y *player*)
					  (+ (sprite-y *player*) (player-speed *player*))))
		    (:scancode-left (setf (sprite-x *player*)
					  (- (sprite-x *player*) (player-speed *player*))))
		    (:scancode-right (setf (sprite-x *player*)
					   (+ (sprite-x *player*) (player-speed *player*))))
		    (t ()))
		  )
	(:idle ()
	       (setf (sprite-rect *player*) (make-rect *player*))
	       (sdl2:render-clear renderer)
	       (render-map renderer)
	       (sdl2:render-copy renderer (sprite-image *player*) :dest-rect (sprite-rect *player*))
	       (sdl2:render-present renderer)
	       (sdl2:delay 10)
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


(defun check-collision (rects)
  (sdl2:has-intersect (sprite-rect *player*) rects))

(defun make-list-unpassable-cell ()
  (let ((ret '()))
   (dotimes (i (car (array-dimensions *map*)))
    (dotimes (j (cadr (array-dimensions *map*)))
      (unless (cell-passable? (aref *map* i j))
	(push (sprite-rect (aref *map* i j)) ret))
      ))
    ret))
