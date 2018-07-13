(ql:quickload "sdl2")
(ql:quickload "sdl2-image")


(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defstruct sprite image x y w h)
(defparameter *player* (make-sprite :image "cobra.png" :x 15 :y 200 :w 30 :h 30))


(defstruct rock image w h)
(defparameter *rock* (make-rock :image "rock.png" :w 120 :h 120));w и h пока не робят


(defmacro with-window-surface ((window surface) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (let ((,surface (sdl2:get-window-surface ,window)))
         ,@body))))


(defun load-surface (filename &optional pixel-format)
  (sdl2:convert-surface-format (sdl2-image:load-image filename) pixel-format))


(defun make-sprite-rect (s)
  (sdl2:make-rect
   (sprite-x s)
   (sprite-y s)
   (sprite-w s)
   (sprite-h s)))

(defun main()
  (with-window-surface (window screen-surface)
    (sdl2-image:init '(:png))
    (let (
	  (player-image (load-surface (sprite-image *player*) (sdl2:surface-format-format screen-surface)))
	  (player-rect (make-sprite-rect *player*))
	  (rock-image (load-surface (rock-image *rock*) (sdl2:surface-format-format screen-surface)))
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
	       (sdl2:fill-rect screen-surface
			       nil
			       (sdl2:map-rgb (sdl2:surface-format screen-surface) 255 255 255))
	       (dolist (el rock-rects 'done)  ;цикл выводит камень 
		 (sdl2:blit-surface rock-image ;во все ректанглы в списке rock-rects
				    nil
				    screen-surface
				    el))
	       (sdl2:blit-surface player-image
				  nil
				  screen-surface
				  player-rect)
	       (sdl2:update-window window)
	       (sdl2:delay 10)
	       )))))
