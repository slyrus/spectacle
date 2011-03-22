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
   (lock-x-and-y-scale :accessor lock-x-and-y-scale :initarg :lock-x-and-y-scale))
  (:default-initargs :image nil
    :transform-parameters #(1d0 1d0 0d0 0d0 0d0 0d0 0d0)
    :lock-x-and-y-scale t
    :transform nil))

(defun y-scale-callback (gadget scale)
  (declare (ignore gadget))
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image)
                     (transform-parameters transform-parameters)
                     (transform transform)
                     (lock-x-and-y-scale lock-x-and-y-scale))
        viewer
      (let ((y-scale-param 0)
            (x-scale-param 1))
        (unless (equal (elt transform-parameters y-scale-param) scale)
          (let ((x-scale-slider (find-pane-named *application-frame* 'x-scale)))
            (when lock-x-and-y-scale
              (setf (gadget-value x-scale-slider) scale
                    (elt transform-parameters x-scale-param) scale)))
          (setf (elt transform-parameters y-scale-param) scale
                transform nil)
          (handle-repaint viewer (or (pane-viewport-region viewer)
                                     (sheet-region viewer))))))))

(defun x-scale-callback (gadget scale)
  (declare (ignore gadget))
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image)
                     (transform-parameters transform-parameters)
                     (transform transform)
                     (lock-x-and-y-scale lock-x-and-y-scale))
        viewer
      (let ((y-scale-param 0)
            (x-scale-param 1))
        (unless (equal (elt transform-parameters x-scale-param) scale)
          (let ((y-scale-slider (find-pane-named *application-frame* 'y-scale)))
            (when lock-x-and-y-scale
              (setf (gadget-value y-scale-slider) scale
                    (elt transform-parameters y-scale-param) scale)))
          (setf (elt transform-parameters x-scale-param) scale
                transform nil)
          (handle-repaint viewer (or (pane-viewport-region viewer)
                                     (sheet-region viewer))))))))

(defun theta-callback (gadget degrees)
  (declare (ignore gadget))
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image)
                     (transform-parameters transform-parameters)
                     (transform transform))
        viewer
      (let ((param 6)
            (rads (mod (* pi degrees (/ 180)) (* 2 pi))))
        (unless (equal (elt transform-parameters param) rads)
          (setf (elt transform-parameters param) rads
                transform nil)
          (handle-repaint viewer (or (pane-viewport-region viewer)
                                     (sheet-region viewer))))))))

(defun y-shear-callback (gadget shear)
  (declare (ignore gadget))
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image)
                     (transform-parameters transform-parameters)
                     (transform transform))
        viewer
      (let ((param 4))
        (unless (equal (elt transform-parameters param) shear)
          (setf (elt transform-parameters param) shear
                transform nil)
          (handle-repaint viewer (or (pane-viewport-region viewer)
                                     (sheet-region viewer))))))))

(defun x-shear-callback (gadget shear)
  (declare (ignore gadget))
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (with-accessors ((image image)
                     (transform-parameters transform-parameters)
                     (transform transform))
        viewer
      (let ((param 5))
        (unless (equal (elt transform-parameters param) shear)
          (setf (elt transform-parameters param) shear
                transform nil)
          (handle-repaint viewer (or (pane-viewport-region viewer)
                                     (sheet-region viewer))))))))

(define-application-frame spectacle ()
  ()
  (:menu-bar menubar-command-table)
  (:panes
   (viewer (scrolling ()
             (make-pane 'spectacle-pane
                        :name 'spectacle-pane
                        :width 500
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
            :drag-callback #'y-scale-callback
            :value-changed-callback 'y-scale-callback)
   (x-scale :slider
            :min-value 0.1
            :max-value 4
            :decimal-places 2
            :value 1.0d0
            :show-value-p t
            :orientation :horizontal
            :drag-callback #'x-scale-callback
            :value-changed-callback 'x-scale-callback)
   (theta :slider
          :min-value -180
          :max-value 180
          :decimal-places 1
          :value 0
          :show-value-p t
          :orientation :horizontal
          :drag-callback #'theta-callback
          :value-changed-callback 'theta-callback)
   (y-shear :slider
            :min-value -5
            :max-value 5
            :decimal-places 2
            :value 0.0d0
            :show-value-p t
            :orientation :horizontal
            :drag-callback #'y-shear-callback
            :value-changed-callback 'y-shear-callback)
   (x-shear :slider
            :min-value -5
            :max-value 5
            :decimal-places 2
            :value 0.0d0
            :show-value-p t
            :orientation :horizontal
            :drag-callback #'x-shear-callback
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
              (4/5 (horizontally ()
                     (4/5 viewer)
                     (1/5 (vertically ()
                            (labelling ()
                              (vertically ()
                                lock-scale
                                (labelling (:label "Y Scale")
                                  y-scale)
                                (labelling (:label "X Scale")
                                  x-scale)))
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
    (8-bit-rgb-image
     (locally (declare (optimize (speed 3))
                       (type 8-bit-rgb-image image)))
     #+debug (print (list 'image-bounds y x) *trace-output*)
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
                                            :data cimg))))
    ))

(defmethod handle-repaint ((pane spectacle-pane) region)
  (with-accessors ((image image)
                   (transform-parameters transform-parameters)
                   (transform transform))
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
    ;; 1. get the image dimensions
    (when image
      (with-image-bounds (a-height a-width) 
          image
        #+debug (print (list 'a a-height a-width) *trace-output*)
        (updating-output (pane :unique-id image)
        ;; 2. compute the coordinates and dimension of the transformed image
          (multiple-value-bind (b-y1 b-x1 b-y2 b-x2)
              (opticl::compute-bounds 0 0 a-height a-width transform)
            (let ((b-height (abs (- b-y2 b-y1)))
                  (b-width (abs (- b-x2 b-x1))))
              #+debug (print (list 'b b-y1 b-x1 b-y2 b-x2 b-height b-width) *trace-output*)

              ;; 3. get the coordinates of the sheet (x1 and y1 should always be 0!)
              (with-bounding-rectangle* (c-x1 c-y1 c-x2 c-y2)
                  (sheet-region pane)
                #+debug (print (list 'c c-y1 c-x1 c-y2 c-x2) *trace-output*)

                ;; 4. get the coordinates of the pane
                (with-bounding-rectangle* (d-x1 d-y1 d-x2 d-y2)
                    (pane-viewport-region pane)
                  (let ((d-height (- d-y2 d-y1))
                        (d-width (- d-x2 d-x1)))
                    #+debug (print (list 'd d-y1 d-x1 d-y2 d-x2) *trace-output*)

                    (let ((e-y1 (+ d-y1 b-y1))
                          (e-x1 (+ d-x1 b-x1))
                          (e-y2 (min (+ d-y2 b-y1) (+ d-y1 b-y2)))
                          (e-x2 (min (+ d-x2 b-x1) (+ d-x1 b-x2))))
                      (let ((e-height (- e-y2 e-y1))
                            (e-width (- e-x2 e-x1)))
                        #+debug (print (list 'e e-y1 e-x1 e-y2 e-x2 e-height e-width) *trace-output*)
                        (resize-sheet pane
                                      (max d-x2 b-width)
                                      (max d-y2 b-height))
                        ;; 4A. fill the region with red so that we can figure
                        ;; out what we're doing here, this will probably become
                        ;; +background-ink+ at some point.
                        
                        (clim:draw-rectangle* (sheet-medium pane)
                                              d-x1 d-y1 d-x2 d-y2
                                              :ink +background-ink+)

                        ;; 5. Now we should be able to transform the image into
                        ;; the appropriately requested destination image and display that
                        
                        (let ((shift (make-affine-transformation
                                      :y-shift (- b-y1)
                                      :x-shift (- b-x1))))
                        
                          (let ((transformed-image
                                 (if transform
                                     (transform-image image 
                                                      (opticl::matrix-multiply
                                                       shift transform)
                                                      :y (cons d-y1 d-y2)
                                                      :x (cons d-x1 d-x2))
                                     #+nil
                                     (transform-image image transform
                                                      :y (cons e-y1 e-y2)
                                                      :x (cons e-x1 e-x2))
                                     image)))
                            #+debug (print (array-dimensions transformed-image) *trace-output*)
                            (let ((pattern
                                   (opticl-image-to-climi-rgb-pattern
                                    transformed-image (floor e-height) (floor e-width))))
                              (clim:draw-pattern* pane pattern 
                                                  (max d-x1 (/ (- d-x2 b-width) 2))
                                                  (max d-y1 (/ (- d-y2 b-height) 2))))))))
                    (change-space-requirements pane :height b-height :width b-width))))
              #+debug
              (loop for i below b-height by 100
                 do (clim:draw-line* pane 0 i b-width i :ink +yellow+))
              #+debug
              (loop for j below b-width by 100
                 do (clim:draw-line* pane j 0 j b-height :ink +red+)))))))))

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

(define-spectacle-command (com-zoom :name t)
    ()
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (let ((y-scale-slider (find-pane-named *application-frame* 'y-scale))
          (x-scale-slider (find-pane-named *application-frame* 'x-scale))
          (bounding-rectangle-height (bounding-rectangle-height viewer))
          (bounding-rectangle-width (bounding-rectangle-width viewer)))
      (with-accessors ((image image)
                       (transform-parameters transform-parameters)
                       (transform transform))
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
                    transform nil))
            (change-space-requirements viewer :height oldy :width oldx))
          (handle-repaint viewer (or (pane-viewport-region viewer)
                                     (sheet-region viewer))))))))

(define-spectacle-command (com-redraw :name t)
    ()
  (let ((viewer (find-pane-named *application-frame* 'spectacle-pane)))
    (handle-repaint viewer (or (pane-viewport-region viewer)
                               (sheet-region viewer)))))

(define-spectacle-command (com-reset :name t)
    ()
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
                            ("Quit" :command com-quit)))

(make-command-table 'image-command-table
		    :errorp nil
		    :menu '(("Zoom" :command com-zoom)
                            ("Redraw" :command com-redraw)
                            ("Reset" :command com-reset)))

(make-command-table 'menubar-command-table
		    :errorp nil
		    :menu '(("File" :menu file-command-table)
                            ("Image" :menu image-command-table)))
