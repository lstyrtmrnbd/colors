;;;; Colors
;;;  use rainbow-mode

(in-package #:colors)

;;; Types

(deftype unit-real ()
  "Real number in [0,1]"
  '(real 0 1))

;;; Constants

(defvar black (vec 0 0 0))
(defvar white (vec 1 1 1))
(defvar red   (vec 1 0 0))
(defvar green (vec 0 1 0))
(defvar blue  (vec 0 0 1))
(defvar gray  (vec 0.5 0.5 0.5))

;;; Util

(defun random-color ()
  (vec3-random 0.0 1.0))

(defun invert (a)
  (v- white a))

(defun repeat (fn colors))

;;; Blend modes

(defun average (a b)
  (v/ (v+ a b) (vec 2 2 2)))

(defun multiply (a b)
  (v* a b))

(defun screen (a b)
  (invert (v* (invert a) (invert b))))

(defun darken (a b)
  (if (< a b)
      a
      b))

(defun lighten (a b)
  (if (< a b)
      b
      a))

(defun difference (a b)
  (vabs (v- a b)))

(defun negation (a b)
  (invert (vabs (v- white a b))))

(defun exclusion (a b)
  (let ((2ab (v* a b (vec 2 2 2))))
    (v+ a b (v- 2ab))))

(defun pegtop-softlight (a b)
  (v+ (v* a b (invert a)) (v* a (screen a b))))

(defun color-dodge (a b)
  (v/ a (invert b)))

;;; Rendering
(defun render (img &optional filename (bit-depth 8))
  (write-png filename (array-to-png img bit-depth) bit-depth))

;;; PNG Output

(defun unit-real-to-unsigned-byte (real bit-depth)
  "Turns RGB value into unsigned byte for png output"
  (let ((max (1- (expt 2 bit-depth))))
    (floor (* real max))))

(defun array-to-png (arr bit-depth)
  "Maps 2D array of colors to PNG image,
   which is a column-major array of unsigned bytes 8 or 16 bits deep"
  (let* ((w (array-dimension arr 0))
         (h (array-dimension arr 1))
         (result (png:make-image w h 3 bit-depth)))
    (dotimes (x w)
      (dotimes (y h)
        (let* ((color (aref arr x y))
               (red (unit-real-to-unsigned-byte (vx color) bit-depth))
               (green (unit-real-to-unsigned-byte (vy color) bit-depth))
               (blue (unit-real-to-unsigned-byte (vz color) bit-depth)))
          (setf (aref result x y 0) red
                (aref result x y 1) green
                (aref result x y 2) blue))))
    result))

;; File output
(defun write-png (path png &optional (bit-depth 8))
  (let ((filename (or path (generate-filename))))
    (with-open-file (output filename :element-type `(unsigned-byte ,bit-depth)
                            :direction :output :if-exists :supersede)
      (png:encode png output))))

(project-pathname:define project-path (:asdf "colors")
  (:renders "renders"))

(defun generate-filename ()
  "Generate a filename for render output tagged with current time"
  (project-path (concatenate 'string
                             "render-"
                             (write-to-string (get-universal-time))
                             ".png")
                :renders))
