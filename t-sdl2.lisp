(ql:quickload "sdl2")
(ql:quickload "sdl2-image")


(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defstruct sprite image x y w h)
(defparameter *player* (make-sprite :image "cobra.png" :x 15 :y 200 :w 30 :h 30))


(defstruct rock image w h)
(defparameter *rock* (make-rock :image "rock.png" :w 60 :h 60))


(defmacro with-window-surface ((window surface) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (let ((,surface (sdl2:get-window-surface ,window)))
         ,@body))))


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


(defun load-texture (renderer filename)
  (sdl2:create-texture-from-surface renderer (sdl2-image:load-image filename)))

(defun make-sprite-rect (s)
  (sdl2:make-rect
   (sprite-x s)
   (sprite-y s)
   (sprite-w s)
   (sprite-h s)))

(defun main()
  (with-window-renderer (window renderer)
    (sdl2-image:init '(:png))
    (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
    (let (
	  (player-image (load-texture renderer (sprite-image *player*)))
	  (player-rect (make-sprite-rect *player*))
	  (rock-image (load-texture renderer (rock-image *rock*)))
	  (rock-rects `(,(sdl2:make-rect 0 0 (rock-w *rock*) (rock-h *rock*))
			,(sdl2:make-rect 0 240 (rock-w *rock*) (rock-h *rock*))
			))
	  )
      (sdl2:with-event-loop (:method :poll)
	(:quit () t)
	(:keydown (:keysym keysym)
                  (case (sdl2:scancode keysym)
                    (:scancode-up (setf (sprite-y *player*) (- (sprite-y *player*) 10)))
                    (:scancode-down (setf (sprite-y *player*) (+ (sprite-y *player*) 10)))
                    (:scancode-left (setf (sprite-x *player*) (- (sprite-x *player*) 10)))
                    (:scancode-right (setf (sprite-x *player*) (+ (sprite-x *player*) 10)))
		    (t ()))
		  )
	(:idle ()
	       (setf player-rect (make-sprite-rect *player*))
	       ;(sdl2:fill-rect screen-surface
		;	       nil
		;	       (sdl2:map-rgb (sdl2:surface-format screen-surface) 255 255 255))

	       (sdl2:render-clear renderer)
	       (dolist (el rock-rects 'done)  ;цикл выводит камень 
		 (sdl2:render-copy renderer rock-image :dest-rect el))
	       (sdl2:render-copy renderer player-image :dest-rect player-rect)
	       (sdl2:render-present renderer)
	       (sdl2:delay 10)
	       )))))
