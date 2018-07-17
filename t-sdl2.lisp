(ql:quickload "sdl2")
(ql:quickload "sdl2-image")
;(ql:quickload "sdl2-ttf")


(defclass sprite ()
  ((picture :initarg :picture :accessor sprite-picture)
   (x :initarg :x :accessor sprite-x)
   (y :initarg :y :accessor sprite-y)
   (w :initarg :w :accessor sprite-w)
   (h :initarg :h :accessor sprite-h)
   (center :initarg :center :accessor sprite-center :initform '(0 0))
   (image :accessor sprite-image :initform nil :initarg :image)
   (rect :accessor sprite-rect :initform nil)
   (visible :initarg :visible? :accessor sprite-visible? :initform t)))


(defclass player (sprite)
  ((speed :initarg :speed :accessor player-speed)
   (vel-x :initarg :vel-x :accessor player-vel-x :initform 0)
   (vel-y :initarg :vel-y :accessor player-vel-y :initform 0)
   (move-vel-x :accessor player-move-vel-x :initform 0)
   (move-vel-y :accessor player-move-vel-y :initform 0)
   (future :accessor player-future :initform nil)
   (airborne :accessor player-airborne :initform t)
   (animation :initarg :animation :accessor player-animation)
   (number :initarg :number :accessor player-number)
   ))

(defclass game-event (sprite)
  ((type :initform nil :accessor game-event-type)
  (on-colision :initarg :colision :accessor game-event-on-colision)
  (on-use :initarg :use :accessor game-event-on-use))
  )


(defparameter *font* nil)

;; (defclass tex ()
;;   ((w
;;     :accessor tex-w
;;     :initform 0 )
;;    (h
;;     :accessor tex-h
;;     :initform 0)
;;    (image
;;     :accessor tex-texture
;;     :initform nil)))

;; (defun load-texture-from-text (renderer text)
;;   (let ((surface (sdl2-ttf:render-text-solid *font* text 0 0 0 0)))
;;     (let ((tex (make-instance 'sprite
;; 			      :h (sdl2:surface-height surface)
;; 			      :w (sdl2:surface-width surface)
;; 			      :x 100
;; 			      :y 100
;; 			      :image (sdl2:create-texture-from-surface renderer surface))))
      
      
      
;;       tex)))

;; (defmethod render-text ((e sprite) renderer &key clip angle center flip)
;;   (sdl2:render-copy-ex renderer
;; 		       (sprite-image e)
;; 		       :source-rect clip
;; 		       :dest-rect (sdl2:make-rect (sprite-x e)
;; 						  (sprite-y e)
;; 						  (if clip (sdl2:rect-width clip) (sprite-w e))
;; 						  (if clip (sdl2:rect-height clip) (sprite-h e)))
;; 		       :angle angle
;; 		       :center center
;; 		       :flip (list flip)))




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
   (event :initarg :event :accessor cell-event :initform nil)))




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

(defparameter *events* (vector (make-instance 'game-event
				      :picture "terminal.png"
				      :x (sprite-x (aref *map* 6 15))
				      :y (sprite-y (aref *map* 6 15))
				      :h (* 2 (sprite-h (aref *map* 5 15)))
				      :w (sprite-w (aref *map* 5 15)))))

;(defparameter *texts* '())
(defparameter *playable* (vector (make-instance 'player
						:x 50
						:y 100
						:w 30
						:h 50
						:speed 8
						:picture "man.png"
						:number 0
						:animation (vector (sdl2:make-rect 1229 411 136 407)
								   (sdl2:make-rect 874 403 179 408)
								   (sdl2:make-rect 249 403 223 406)
								   (sdl2:make-rect 1 1 263 400)
								   (sdl2:make-rect 1402 1 119 406)
								   (sdl2:make-rect 1367 411 134 408)
								   (sdl2:make-rect 1055 403 172 408)
								   (sdl2:make-rect 474 403 210 405)
								   (sdl2:make-rect 1 403 246 401)
								   (sdl2:make-rect 1229 1 171 408)
								   (sdl2:make-rect 1503 409 102 407)))
				 (make-instance 'player
						:x 150
						:y 100
						:w 30
						:h 50
						:number 1
						:speed 8
						:picture "man.png"
						:animation (vector (sdl2:make-rect 1229 411 136 407)
								   (sdl2:make-rect 874 403 179 408)
								   (sdl2:make-rect 249 403 223 406)
								   (sdl2:make-rect 1 1 263 400)
								   (sdl2:make-rect 1402 1 119 406)
								   (sdl2:make-rect 1367 411 134 408)
								   (sdl2:make-rect 1055 403 172 408)
								   (sdl2:make-rect 474 403 210 405)
								   (sdl2:make-rect 1 403 246 401)
								   (sdl2:make-rect 1229 1 171 408)
								   (sdl2:make-rect 1503 409 102 407)))))

(defparameter *player* (svref *playable* 0))

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


(defun check-collision (rect)
  (sdl2:has-intersect (player-future *player*) (sprite-rect rect)))

(defun check-collision-r (src-rect dest-rect)
  (sdl2:has-intersect src-rect (sprite-rect dest-rect)))

(defmethod get-center-s ((e sprite))
  `(,(float (+ (sprite-x e) (/ (sprite-w e) 2)))
    ,(float (+ (sprite-y e) (/ (sprite-h e) 2)))))




(defun render-map (renderer)
  (dotimes (i (car (array-dimensions *map*)))
    (dotimes (j (cadr (array-dimensions *map*)))
      (when (sprite-visible? (aref *map* i j))
	(sdl2:render-copy
	 renderer
	 (sprite-image (aref *map* i j))
	 :dest-rect (sprite-rect (aref *map* i j))))
      )))



(defun check-ground (unpass)
  (let* ((ground-rect (sdl2:make-rect (sprite-x *player*)
				      (+ (sprite-y *player*) (sprite-h *player*))
				      (sprite-w *player*)
				      1))
	 (pl-center (get-center-s *player*))
	 (ground (remove-if
		  (lambda (x)(<= (cadr (sprite-center x))
				 (+ (cadr pl-center) (/ (sprite-h *player*) 2))))
		  unpass)))
    (let ((check-g (remove-if-not (lambda (x) (check-collision-r ground-rect x)) ground)))
      (if check-g
	  t
	  nil)
    )
    ))

(defun render-event (renderer)
  (dotimes (i (length *events*))
    (when (sprite-visible? (svref *events* i))
	(sdl2:render-copy
	 renderer
	 (sprite-image (svref *events* i))
	 :dest-rect (sprite-rect (svref *events* i))))))

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
	    ;(format t "DNO ~A~%" (mapcar (lambda (x) (sprite-rect x)) check-g))
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
    
    ;; (unless (player-airborne *player*)(setf (player-vel-x *player*) 0))
    (if (player-airborne *player*)
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


(defun init-events (renderer)
  (dotimes (i (length *events*))
    (load-texture renderer (svref *events* i))
    (make-rect (svref *events* i))))

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
      (setf (sprite-visible? (aref *map* i y)) t)
      )))

(defun map-mod-floor-with-whole (x y-whole w-whole)
  (dotimes (j (cadr (array-dimensions *map*)))
    (unless (and (>= j y-whole) (< j (+ y-whole w-whole)))
      (when (cell-passable? (aref *map* x j))
	(setf (cell-passable? (aref *map* x j)) nil)
	(setf (sprite-visible? (aref *map* x j)) t)
	))))



(defun main(argv)
  (declare (ignore argv))
  (map-mod-wall 0)
  (map-mod-floor-with-whole 4 10 3)
  (with-window-renderer (window renderer)
    (sdl2-image:init '(:png))
;;    (sdl2-ttf:init)
    (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
    (init-player renderer)
    (init-events renderer)
    (init-image-and-rect-in-map renderer)
    ;;(setf *font* (sdl2-ttf:open-font "GOST.ttf" 28))
    (let ((unpass (make-list-unpassable-cell))
	  (animation-n 0)
	  (frame 0)
	  (ground-animation nil)
	  ;;(text-test (load-texture-from-text renderer "WHAT DOES THE FOX SAY???"))
	  )
      (sdl2:with-event-loop (:method :poll)
	(:quit () t)
	(:keydown (:keysym keysym)
		  (case (sdl2:scancode keysym)
		    (:scancode-e
		     ;; (render text-test
		     ;; 	     renderer
		     ;; 	     :x 100
		     ;; 	     :y 100)
		     )
		    (:scancode-space
		     (if (>= (player-number *player*) (1- (length *playable*)))
			 (setf *player* (svref *playable* 0))
			 (progn
			   (format t "PL N ~A~%LENGTH ~A~%" (player-number *player*) (length *playable*))
			   (setf *player* (svref *playable* (1+ (player-number *player*))))
			   (init-player renderer)))
		     )
		    (:scancode-up 
		     (unless (player-airborne *player*)
		       (progn
		       (setf (player-airborne *player*) t)
		       (setf (player-vel-y *player*) (- 35)))))
		    (:scancode-down t)
		    (:scancode-left 
		     (setf (player-vel-x *player*) (- (player-speed *player*))))
		  (:scancode-right
		   (setf (player-vel-x *player*)  (player-speed *player*)))
		  (t ()))
		  
		  )
	(:keyup (:keysym keysym)
		(case (sdl2:scancode keysym)
		  (:scancode-left
		   (when (minusp (player-vel-x *player*))(setf (player-vel-x *player*) 0)))
		  (:scancode-right
		   (when (plusp (player-vel-x *player*))(setf (player-vel-x *player*) 0)))
		  (t ())
		  ))
	(:idle ()
	       (setf frame (1+ frame))
	       ;(setf animation-n (1+ animation-n))
	       (unless (check-ground unpass)
		 (setf (player-airborne *player*) t))
	       (check-future unpass);)
	       
	       (when (player-future *player*)
	       	 (setf (sprite-rect *player*) (player-future *player*)))
	       (sdl2:render-clear renderer)
	       (render-map renderer)
	       (render-event renderer)
	       (unless (or (player-airborne *player*) (= (sum-vel-x *player*) 0))
		 (setf ground-animation t))
	       (if ground-animation
		   (progn
		     (sdl2:render-copy renderer
				       (sprite-image *player*)
				       :source-rect (svref (player-animation *player*) animation-n)
				       :dest-rect (sprite-rect *player*))
		     (when (>= frame 4)
		       (setf frame 0)
		       (if (>= animation-n (1- (length (player-animation *player*))))
			   (setf animation-n 0)
			   (setf animation-n (1+ animation-n))))
		     )
		   (progn
		     (setf animation-n 0)
		     (sdl2:render-copy renderer
				       (sprite-image *player*)
				       :source-rect (svref (player-animation *player*) animation-n)
				       :dest-rect (sprite-rect *player*))))
	       
	       (sdl2:render-present renderer)
	       (sdl2:delay 20)
	       )))
    (sdl2-ttf:quit)
    (sdl2-image:quit)))
