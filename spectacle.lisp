;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.
;;;
;;; A simple viewer for opticl images
;;;
;;; Note: Originally based on Troels' Henriksen's
;;; clim-demo:image-viewer
;;;

(in-package :spectacle)

(defclass spectacle-pane (application-pane) 
  ((image :initarg :image :accessor image)
   (transform-parameters :accessor transform-parameters :initarg :transform-parameters)
   (transform :accessor transform :initarg :transform)
   (lock-x-and-y-scale :accessor lock-x-and-y-scale :initarg :lock-x-and-y-scale)
   (clear-background-needed-p :accessor clear-background-needed-p
                              :initarg :clear-background-needed-p)
   (filters :accessor filters :initarg :filters))
  (:default-initargs :image nil
    :transform-parameters #(1d0 1d0 0d0 0d0 0d0 0d0 0d0)
    :lock-x-and-y-scale t
    :clear-background-needed-p nil
    :transform nil
    :filters nil))

(defun y-scale-callback (gadget scale)
  (declare (ignore gadget))
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image)
                     (transform-parameters transform-parameters)
                     (transform transform)
                     (lock-x-and-y-scale lock-x-and-y-scale)
                     (clear-background-needed-p clear-background-needed-p))
        viewer
      (let ((y-scale-param 0)
            (x-scale-param 1))
        (unless (equal (elt transform-parameters y-scale-param) scale)
          (let ((x-scale-slider (find-pane-named *application-frame* 'x-scale)))
            (when lock-x-and-y-scale
              (setf (gadget-value x-scale-slider) scale
                    (elt transform-parameters x-scale-param) scale)))
          (setf (elt transform-parameters y-scale-param) scale
                transform nil
                ;; FIXME! This isn't strictly needed unless we're
                ;; resizing the image to be smaller than the screen --
                ;; we should fix this by clearing the screen in
                ;; handle-repaint if we're not completely drawing over
                ;; the viewport!
                clear-background-needed-p t)
          (handle-repaint viewer (or (pane-viewport-region viewer)
                                     (sheet-region viewer))))))))

(defun x-scale-callback (gadget scale)
  (declare (ignore gadget))
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image)
                     (transform-parameters transform-parameters)
                     (transform transform)
                     (lock-x-and-y-scale lock-x-and-y-scale)
                     (clear-background-needed-p clear-background-needed-p))
        viewer
      (let ((y-scale-param 0)
            (x-scale-param 1))
        (unless (equal (elt transform-parameters x-scale-param) scale)
          (let ((y-scale-slider (find-pane-named *application-frame* 'y-scale)))
            (when lock-x-and-y-scale
              (setf (gadget-value y-scale-slider) scale
                    (elt transform-parameters y-scale-param) scale)))
          (setf (elt transform-parameters x-scale-param) scale
                transform nil
                ;; FIXME! This isn't strictly needed unless we're
                ;; resizing the image to be smaller than the screen --
                ;; we should fix this by clearing the screen in
                ;; handle-repaint if we're not completely drawing over
                ;; the viewport!
                clear-background-needed-p t)
          (handle-repaint viewer (or (pane-viewport-region viewer)
                                     (sheet-region viewer))))))))

(defun theta-callback (gadget degrees)
  (declare (ignore gadget))
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image)
                     (transform-parameters transform-parameters)
                     (transform transform)
                     (clear-background-needed-p clear-background-needed-p))
        viewer
      (let ((param 6)
            (rads (mod (* pi degrees (/ 180)) (* 2 pi))))
        (unless (equal (elt transform-parameters param) rads)
          (setf (elt transform-parameters param) rads
                transform nil
                clear-background-needed-p t)

          (handle-repaint viewer (or (pane-viewport-region viewer)
                                     (sheet-region viewer))))))))

(defun y-shear-callback (gadget shear)
  (declare (ignore gadget))
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image)
                     (transform-parameters transform-parameters)
                     (transform transform)
                     (clear-background-needed-p clear-background-needed-p))
        viewer
      (let ((param 4))
        (unless (equal (elt transform-parameters param) shear)
          (setf (elt transform-parameters param) shear
                transform nil
                clear-background-needed-p t)
          (handle-repaint viewer (or (pane-viewport-region viewer)
                                     (sheet-region viewer))))))))

(defun x-shear-callback (gadget shear)
  (declare (ignore gadget))
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image)
                     (transform-parameters transform-parameters)
                     (transform transform)
                     (clear-background-needed-p clear-background-needed-p))
        viewer
      (let ((param 5))
        (unless (equal (elt transform-parameters param) shear)
          (setf (elt transform-parameters param) shear
                transform nil
                clear-background-needed-p t)
          (handle-repaint viewer (or (pane-viewport-region viewer)
                                     (sheet-region viewer))))))))

(define-application-frame spectacle ()
  ()
  (:menu-bar menubar-command-table)
  (:panes
   (viewer (scrolling ()
             (make-pane 'spectacle-pane
                        :name 'spectacle-pane
                        :incremental-redisplay t
                        :background +black+
                        :foreground +white+)))
   (lock-scale :toggle-button
               :label "Lock X and Y Scale"
               :value t
               :value-changed-callback
               (lambda (gadget value)
                 (declare (ignore gadget))
                 (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
                   (with-accessors ((lock-x-and-y-scale lock-x-and-y-scale))
                       viewer
                     (setf lock-x-and-y-scale value)))))
   (y-scale :slider
            :min-value 0.1
            :max-value 4
            :decimal-places 2
            :value 1.0d0
            :show-value-p t
            :orientation :horizontal
            :drag-callback 'y-scale-callback
            :value-changed-callback 'y-scale-callback)
   (x-scale :slider
            :min-value 0.1
            :max-value 4
            :decimal-places 2
            :value 1.0d0
            :show-value-p t
            :orientation :horizontal
            :drag-callback 'x-scale-callback
            :value-changed-callback 'x-scale-callback)
   (theta :slider
          :min-value -180
          :max-value 180
          :decimal-places 1
          :value 0
          :show-value-p t
          :orientation :horizontal
          :drag-callback 'theta-callback
          :value-changed-callback 'theta-callback)
   (y-shear :slider
            :min-value -5
            :max-value 5
            :decimal-places 2
            :value 0.0d0
            :show-value-p t
            :orientation :horizontal
            :drag-callback 'y-shear-callback
            :value-changed-callback 'y-shear-callback)
   (x-shear :slider
            :min-value -5
            :max-value 5
            :decimal-places 2
            :value 0.0d0
            :show-value-p t
            :orientation :horizontal
            :drag-callback 'x-shear-callback
            :value-changed-callback 'x-shear-callback)
   (reset-button :push-button
                 :label "Reset Parameters"
                 :activate-callback (lambda (gadget)
                                      (declare (ignore gadget))
                                      (com-reset)))
   (interactor :interactor
               :text-style (make-text-style :sans-serif nil nil)
               :min-height 100))
  (:layouts
   (default (vertically (:height 250)
              (4/5 (horizontally (:width 600)
                     (5/6 viewer)
                     (1/6 (vertically ()
                            (vertically ()
                              lock-scale
                              (labelling (:label "Y Scale")
                                y-scale)
                              (labelling (:label "X Scale")
                                x-scale))
                            (labelling (:label "Theta")
                              theta)
                            (labelling (:label "Y Shear")
                              y-shear)
                            (labelling (:label "X Shear")
                              x-shear)
                            reset-button))))
              (1/5 interactor)))))

(defun reset-sliders ()
  (let ((y-scale-slider (find-pane-named *application-frame* 'y-scale))
        (x-scale-slider (find-pane-named *application-frame* 'x-scale))
        (theta-slider (find-pane-named *application-frame* 'theta))
        (y-shear-slider (find-pane-named *application-frame* 'y-shear))
        (x-shear-slider (find-pane-named *application-frame* 'x-shear)))
    (setf (gadget-value y-scale-slider) 1.0d0)
    (setf (gadget-value x-scale-slider) 1.0d0)
    (setf (gadget-value y-shear-slider) 0.0d0)
    (setf (gadget-value x-shear-slider) 0.0d0)
    (setf (gadget-value theta-slider) 0.0d0)))

(defun reset-transform-parameters (transform-parameters)
  (setf (elt transform-parameters 0) 1d0
        (elt transform-parameters 1) 1d0
        (elt transform-parameters 4) 0d0
        (elt transform-parameters 5) 0d0
        (elt transform-parameters 6) 0d0))

(defun opticl-image-to-climi-rgb-pattern (image y x)
  (etypecase image

    (8-bit-gray-image
     (locally (declare (optimize (speed 3))
                       (type 8-bit-gray-image image))
       (let ((cimg (make-32-bit-gray-image y x)))
         (declare (type 32-bit-gray-image cimg))
         (set-pixels (i j) cimg
           (let ((k (pixel image i j)))
             (+ (ash k 0) (ash k 8) (ash k 16))))
         (make-instance 'clim-internals::rgb-pattern
                        :image (make-instance 'clim-internals::rgb-image
                                              :height y
                                              :width x
                                              :data cimg)))))
    (1-bit-gray-image
     (locally (declare (optimize (speed 3))
                       (type 1-bit-gray-image image))
       (let ((cimg (make-32-bit-gray-image y x)))
         (declare (type 32-bit-gray-image cimg))
         (set-pixels (i j) cimg
           (let ((k (* 255 (pixel image i j))))
             (+ (ash k 0) (ash k 8) (ash k 16))))
         (make-instance 'clim-internals::rgb-pattern
                        :image (make-instance 'clim-internals::rgb-image
                                              :height y
                                              :width x
                                              :data cimg)))))
    (8-bit-rgb-image
     (locally (declare (optimize (speed 3))
                       (type 8-bit-rgb-image image))
       (let ((cimg (make-32-bit-gray-image y x)))
         (declare (type 32-bit-gray-image cimg))
         (set-pixels (i j) cimg
           (multiple-value-bind (r g b)
               (pixel image i j)
             (+ (ash r 0) (ash g 8) (ash b 16))))
         (make-instance 'clim-internals::rgb-pattern
                        :image (make-instance 'clim-internals::rgb-image
                                              :height y
                                              :width x
                                              :data cimg)))))
    (8-bit-rgba-image
     (locally (declare (optimize (speed 3))
                       (type 8-bit-rgba-image image))
       (let ((cimg (make-32-bit-gray-image y x)))
         (declare (type 32-bit-gray-image cimg))
         (set-pixels (i j) cimg
           (multiple-value-bind (r g b)
               (pixel image i j)
             (+ (ash r 0) (ash g 8) (ash b 16))))
         (make-instance 'clim-internals::rgb-pattern
                        :image (make-instance 'clim-internals::rgb-image
                                              :height y
                                              :width x
                                              :data cimg)))))

    (16-bit-rgb-image
     (locally (declare (optimize (speed 3))
                       (type 16-bit-rgb-image image))
       (let ((cimg (make-32-bit-gray-image y x)))
         (declare (type 32-bit-gray-image cimg))
         (set-pixels (i j) cimg
           (multiple-value-bind (r g b)
               (pixel image i j)
             (+ (ash (ash r -8) 0) (ash (ash g -8) 8) (ash (ash b -8) 16))))
         (make-instance 'clim-internals::rgb-pattern
                        :image (make-instance 'clim-internals::rgb-image
                                              :height y
                                              :width x
                                              :data cimg)))))
    (16-bit-rgba-image
     (locally (declare (optimize (speed 3))
                       (type 16-bit-rgba-image image))
       (let ((cimg (make-32-bit-gray-image y x)))
         (declare (type 32-bit-gray-image cimg))
         (set-pixels (i j) cimg
           (multiple-value-bind (r g b)
               (pixel image i j)
             (+ (ash (ash r -8) 0) (ash (ash g -8) 8) (ash (ash b -8) 16))))
         (make-instance 'clim-internals::rgb-pattern
                        :image (make-instance 'clim-internals::rgb-image
                                              :height y
                                              :width x
                                              :data cimg)))))))

(defmethod handle-repaint ((pane spectacle-pane) region)
  (with-accessors ((image image)
                   (transform-parameters transform-parameters)
                   (transform transform)
                   (clear-background-needed-p clear-background-needed-p)
                   (filters filters))
      pane
    ;; Ok, we have 5 "regions" to consider here:
    ;;
    ;; A. The untransformed image -- 0, 0, image-height, image-width
    ;;  
    ;; B. The transformed image
    ;;
    ;; C. The sheet -- this is the underlying pane, around which we
    ;; can scroll to view some or all of the image
    ;;
    ;; D. The pane -- this is the currently visible portion of the
    ;; sheet, on which we will actually draw the image
    ;;
    ;; E. The region of transformed portion of the image
    ;; which we will actually want to view
    
    ;; we'll compute the opticl transform if it doesn't exist yet
    (unless transform
      (destructuring-bind (y-scale x-scale y-shift x-shift y-shear x-shear rotate)
          (coerce transform-parameters 'list)
        (setf transform
              (make-affine-transformation :y-scale y-scale :x-scale x-scale
                                          :y-shift y-shift :x-shift x-shift
                                          :y-shear y-shear :x-shear x-shear
                                          :theta rotate))))
    (when image
      ;; 1. a is the dimensions of the original (untransformed image)
      (with-image-bounds (a-height a-width) 
          image
        #+(or) (print (list 'a a-height a-width) *trace-output*)
        (updating-output (pane :unique-id image)
          ;; 2. compute the coordinates and dimension of the
          ;; transformed image the b coordinates the bounds of the
          ;; transformed image, PRIOR to shifting the transformed
          ;; image such that all of the coordinates are positive
          (multiple-value-bind (b-y1 b-x1 b-y2 b-x2)
              (opticl::compute-bounds 0 0 a-height a-width transform)
            (let ((b-height (abs (- b-y2 b-y1)))
                  (b-width (abs (- b-x2 b-x1))))
              #+(or) (print (list 'b b-y1 b-x1 b-y2 b-x2 b-height b-width) *trace-output*)
              ;; 3. get the coordinates of the pane
              ;; d-y1 and d-y2 will range over [0, b-height),
              ;; d-x1 and d-x2 over [0, b-width)
              (with-bounding-rectangle* (d-x1 d-y1 d-x2 d-y2)
                  (pane-viewport-region pane)
                (let ((d-height (abs (- d-y2 d-y1)))
                      (d-width (abs (- d-x2 d-x1))))
                  #+(or) (print (list 'd d-y1 d-x1 d-y2 d-x2 d-height d-width) *trace-output*)
                  ;; 4. e is the region of the transformed image, in
                  ;; the coordinates of B, that will actually be
                  ;; drawn.
                  ;; e-y1 and e-y2 will range over [b-y1, b-y2) x-y1
                  ;; and x-y2 will range over [b-x1, b-x2)
                  (let ((e-y1 (+ d-y1 b-y1))
                        (e-x1 (+ d-x1 b-x1))
                        (e-y2 (min (+ d-y2 b-y1) (+ d-y1 b-y2)))
                        (e-x2 (min (+ d-x2 b-x1) (+ d-x1 b-x2))))
                    ;; FIXME! We should be able to get rid of e-y1,
                    ;; e-x1, e-y2, and e-x2 and just compute the
                    ;; e-height and e-width directly!
                    (let ((e-height (- e-y2 e-y1))
                          (e-width (- e-x2 e-x1)))
                      #+(or) (print (list 'e e-y1 e-x1 e-y2 e-x2 e-height e-width) *trace-output*)
                      (resize-sheet pane
                                    (max d-x2 b-width)
                                    (max d-y2 b-height))
                      ;; 5. if needed, draw the background over the window so that
                      ;; we don't leave trails
                      (when (and clear-background-needed-p
                                 (or (< e-height d-height)
                                     (< e-width d-width)))
                        (clim:draw-rectangle* (sheet-medium pane)
                                              d-x1 d-y1 d-x2 d-y2
                                              :ink +background-ink+)
                        (setf clear-background-needed-p nil))
                    
                      ;; 6. Now we should be able to transform the image into
                      ;; the appropriately requested destination image and display that
                      (let ((shift (make-affine-transformation
                                    :y-shift (- b-y1)
                                    :x-shift (- b-x1))))
                        (let ((transformed-image
                               (transform-image image 
                                                (opticl::matrix-multiply
                                                 shift transform)
                                                :post-y-bounds (cons d-y1 d-y2)
                                                :post-x-bounds (cons d-x1 d-x2))))
                          #+(or) (print (array-dimensions transformed-image) *trace-output*)
                          (let ((pattern
                                 (opticl-image-to-climi-rgb-pattern
                                  (reduce (lambda (a b) (funcall b a))
                                          (cons transformed-image filters))
                                  (floor e-height) (floor e-width))))
                            (clim:draw-pattern* pane pattern 
                                                (max d-x1 (/ (- d-x2 b-width) 2))
                                                (max d-y1 (/ (- d-y2 b-height) 2))))))))
                  (change-space-requirements pane :height b-height :width b-width)
                  #+(or)
                  (loop for i below b-height by 100
                     do (clim:draw-line* pane 0 i b-width i :ink +yellow+))
                  #+(or)
                  (loop for j below b-width by 100
                     do (clim:draw-line* pane j 0 j b-height :ink +red+)))))))))))

(defun spectacle (&key (new-process t))
  (flet ((run ()
           (let ((frame (make-application-frame 'spectacle)))
             (run-frame-top-level frame))))
    (if new-process
        (clim-sys:make-process #'run :name "Spectacle")
        (run))))

(define-spectacle-command (com-load-image :name t)
    ((image-pathname 'pathname
                     :default (user-homedir-pathname)
                     :insert-default t))
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane))
        (img (read-image-file image-pathname)))
    (with-accessors ((image image)
                     (transform-parameters transform-parameters)
                     (transform transform))
        viewer
      (reset-sliders)
      (reset-transform-parameters transform-parameters)
      (setf transform nil
            image img)
      (handle-repaint viewer (or (pane-viewport-region viewer)
                                 (sheet-region viewer))))))

(define-spectacle-command (com-save-image :name t)
    ((image-pathname 'pathname
                     :default (user-homedir-pathname)
                     :insert-default t))
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image))
        viewer
      (write-image-file image-pathname image))))

(define-spectacle-command (com-fit-image-to-window :name t)
    ()
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (when viewer
      (let ((y-scale-slider (find-pane-named *application-frame* 'y-scale))
            (x-scale-slider (find-pane-named *application-frame* 'x-scale))
            (pvr (pane-viewport-region viewer)))
        (let ((bounding-rectangle-height (bounding-rectangle-height pvr))
              (bounding-rectangle-width (bounding-rectangle-width pvr)))
          (with-accessors ((image image)
                           (transform-parameters transform-parameters)
                           (transform transform)
                           (clear-background-needed-p clear-background-needed-p))
              viewer
            (when image
              (with-image-bounds (oldy oldx) image
                (let ((scale (apply #'min
                                    (append (list (/ bounding-rectangle-height oldy))
                                            (list (/ bounding-rectangle-width oldx))))))
                  (setf (gadget-value y-scale-slider) scale
                        (gadget-value x-scale-slider) scale)
                  (setf (elt transform-parameters 0) scale
                        (elt transform-parameters 1) scale
                        clear-background-needed-p t
                        transform nil))
                (change-space-requirements viewer :height oldy :width oldx))
              (handle-repaint viewer (or (pane-viewport-region viewer)
                                         (sheet-region viewer))))))))))

(define-spectacle-command (com-redraw :name t) ()
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (handle-repaint viewer (or (pane-viewport-region viewer)
                               (sheet-region viewer)))))

(define-spectacle-command (com-blur :name t) ()
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image))
        viewer
      (setf image (blur-image image))
      (handle-repaint viewer (or (pane-viewport-region viewer)
                                 (sheet-region viewer))))))

(define-spectacle-command (com-add-blur-filter :name t) ()
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image)
                     (filters filters))
        viewer
      (push #'blur-image filters)
      (handle-repaint viewer (or (pane-viewport-region viewer)
                                 (sheet-region viewer))))))

(define-spectacle-command (com-add-sharpen-filter :name t) ()
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image)
                     (filters filters))
        viewer
      (push #'sharpen-image filters)
      (handle-repaint viewer (or (pane-viewport-region viewer)
                                 (sheet-region viewer))))))

(define-spectacle-command (com-remove-filter :name t) ()
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image)
                     (filters filters))
        viewer
      (pop filters)
      (handle-repaint viewer (or (pane-viewport-region viewer)
                                 (sheet-region viewer))))))

(define-spectacle-command (com-sharpen :name t) ()
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image))
        viewer
      (setf image (sharpen-image image))
      (handle-repaint viewer (or (pane-viewport-region viewer)
                                 (sheet-region viewer))))))

(define-spectacle-command (com-apply-gamma :name t)
    ((gamma 'number
            :default 1.0
            :insert-default t))
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image))
        viewer
      (setf image (apply-gamma image gamma))
      (handle-repaint viewer (or (pane-viewport-region viewer)
                                 (sheet-region viewer))))))

(define-spectacle-command (com-transpose :name t)
    ()
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image)
                     (clear-background-needed-p clear-background-needed-p))
        viewer
      (setf image (opticl::transpose-image image)
            clear-background-needed-p t)
      (handle-repaint viewer (or (pane-viewport-region viewer)
                                 (sheet-region viewer))))))

(define-spectacle-command (com-horizontal-flip :name t)
    ()
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image)
                     (clear-background-needed-p clear-background-needed-p))
        viewer
      (setf image (opticl::horizontal-flip-image image)
            clear-background-needed-p t)
      (handle-repaint viewer (or (pane-viewport-region viewer)
                                 (sheet-region viewer))))))

(define-spectacle-command (com-vertical-flip :name t)
    ()
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image)
                     (clear-background-needed-p clear-background-needed-p))
        viewer
      (setf image (opticl::vertical-flip-image image)
            clear-background-needed-p t)
      (handle-repaint viewer (or (pane-viewport-region viewer)
                                 (sheet-region viewer))))))

(define-spectacle-command (com-reset :name t) ()
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image)
                     (transform-parameters transform-parameters)
                     (transform transform))
        viewer
      (reset-sliders)
      (reset-transform-parameters transform-parameters)
      (setf transform nil))
    (handle-repaint viewer (or (pane-viewport-region viewer)
                               (sheet-region viewer)))))

(define-spectacle-command (com-quit :name t :keystroke (#\q :meta)) ()
  (frame-exit *application-frame*))

(make-command-table 'file-command-table
		    :errorp nil
		    :menu '(("Load Image" :command com-load-image)
                            ("Save Image" :command com-save-image)
                            ("Quit" :command com-quit)))

(make-command-table 'image-command-table
		    :errorp nil
		    :menu '(("Fit Image to Window" :command com-fit-image-to-window)
                            ("Blur" :command com-blur)
                            ("Sharpen" :command com-sharpen)
                            ("Transpose" :command com-transpose)
                            ("Flip Horizontal" :command com-horizontal-flip)
                            ("Flip Vertical" :command com-vertical-flip)
                            ("Apply Gamma" :command com-apply-gamma)
                            ("Redraw" :command com-redraw)
                            ("Reset" :command com-reset)))

(make-command-table 'menubar-command-table
		    :errorp nil
		    :menu '(("File" :menu file-command-table)
                            ("Image" :menu image-command-table)))


(define-spectacle-command (com-display-image-from-symbol :name t)
    ((image-symbol 'symbol))
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane))
        (img (symbol-value image-symbol)))
    (with-accessors ((image image)
                     (transform-parameters transform-parameters)
                     (transform transform))
        viewer
      (reset-sliders)
      (reset-transform-parameters transform-parameters)
      (setf transform nil
            image img)
      (handle-repaint viewer (or (pane-viewport-region viewer)
                                 (sheet-region viewer))))))
