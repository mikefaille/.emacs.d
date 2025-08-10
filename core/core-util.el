;; -*- lexical-binding: t; -*-
;; core-util.el --- Core utility functions and macros

;; Custom macro for eval-after-load.
;; Note: Consider using built-in `with-eval-after-load 'FEATURE ...`
;; or `:config` blocks in `use-package` for similar functionality
;; in modern configurations.
(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun) (debug t)) ; Added debug spec
  `(eval-after-load ,feature
     '(progn ,@body)))

;; Function to inspect face at point
(defun what-face (&optional pos)
  "Display the name of the face at POS (defaults to point)."
  ;; Use (interactive) to operate at the current point.
  ;; Use "P" if you want to optionally use prefix arg for position.
  (interactive)
  (let* ((position (or pos (point)))
         (face-prop (get-char-property position 'face))
         (read-face (get-char-property position 'read-face-name)) ; Used by overlays
         (face (or read-face face-prop)))
    (if face
        (message "Face at %d: %s%s"
                 position
                 face
                 (if (and read-face face-prop (not (equal read-face face-prop)))
                     (format " (underlying: %s)" face-prop)
                   ""))
      (message "No face at %d" position))))


;; Mark this file as provided
(provide 'core-util)
;;; core-util.el ends here
