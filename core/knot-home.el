(defun rh/center-ascii-art-buffer (file)
  "Center the contents of FILE both horizontally and vertically in the current buffer."
  (let* ((lines (with-temp-buffer
                  (insert-file-contents file)
                  (split-string (buffer-string) "\n" t)))
         (max-line-len (apply #'max (mapcar #'string-width lines)))
         (frame-width (frame-width))
         (frame-height (frame-height))
         (left-padding (max 0 (/ (- frame-width max-line-len) 2)))
         (top-padding (max 0 (/ (- frame-height (length lines)) 2))))
    (erase-buffer)
    (dotimes (_ top-padding) (insert "\n"))
    (dolist (line lines)
      (insert (make-string left-padding ?\s))
      (insert line)
      (insert "\n"))))

(defun rh/welcome-home ()
  "Custom startup screen with ASCII art centered both horizontally and vertically."
  (let ((buf (get-buffer-create "*Home*"))
        (banner (expand-file-name "logo/Emacs-Bloody.txt" user-emacs-directory)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (fundamental-mode)
        (setq-local cursor-type nil)
        (setq-local display-line-numbers nil)
        (setq-local display-line-numbers-mode nil)
        (read-only-mode -1)
        (rh/center-ascii-art-buffer banner)
        (read-only-mode 1)))
    (switch-to-buffer buf)))

(add-hook 'emacs-startup-hook #'rh/welcome-home)

;; Optional: Recenter on new frame creation (for Hyprland etc.)
(defun rh/recenter-home-buffer ()
  (when (get-buffer "*Home*")
    (with-current-buffer "*Home*"
      (let ((inhibit-read-only t))
        (rh/center-ascii-art-buffer
         (expand-file-name "logo/Emacs-Bloody.txt" user-emacs-directory))
        (read-only-mode 1)))))

(add-hook 'server-after-make-frame-hook #'rh/recenter-home-buffer)
(add-hook 'window-size-change-functions
          (lambda (_frame) (rh/recenter-home-buffer)))





;; (defun rh/insert-centered-ascii-art (file)
;;   "Insert FILE contents (ASCII art) centered in the current buffer."
;;   (when (file-exists-p file)
;;     (let* ((lines (with-temp-buffer
;;                     (insert-file-contents file)
;;                     (split-string (buffer-string) "\n")))
;;            (max-len (apply #'max (mapcar #'string-width lines)))
;;            (window-width (window-width))
;;            (padding (max 0 (/ (- window-width max-len) 2))))
;;       (setq-local rh/ascii-art-lines lines
;;                   rh/ascii-art-max-len max-len)
;;       (erase-buffer)
;;       (dolist (line lines)
;;         (insert (make-string padding ?\s))
;;         (insert line)
;;         (insert "\n")))))

;; (defun rh/recenter-ascii-art-on-resize (_frame)
;;   "Redraw centered ASCII art when frame is resized."
;;   (when (and (eq major-mode 'fundamental-mode)
;;              (bound-and-true-p rh/ascii-art-lines))
;;     (let* ((window-width (window-width))
;;            (padding (max 0 (/ (- window-width rh/ascii-art-max-len) 2))))
;;       (let ((inhibit-read-only t))
;;         (erase-buffer)
;;         (dolist (line rh/ascii-art-lines)
;;           (insert (make-string padding ?\s))
;;           (insert line)
;;           (insert "\n"))))))

;; (defun rh/welcome-home ()
;;   "Show custom startup screen with dynamically centered ASCII art."
;;   (let ((buf (get-buffer-create "*Home*"))
;;         (banner (expand-file-name "logo/Emacs-Bloody.txt" user-emacs-directory)))
;;     (with-current-buffer buf
;;       (let ((inhibit-read-only t))
;;         (erase-buffer)
;;         (fundamental-mode)
;;         (setq-local cursor-type nil)
;;         (setq-local display-line-numbers nil)
;;         (read-only-mode -1)
;;         (rh/insert-centered-ascii-art banner)
;;         (read-only-mode 1)))
;;     (switch-to-buffer buf)))

;; (add-hook 'emacs-startup-hook #'rh/welcome-home)
;; (add-hook 'server-after-make-frame-hook #'rh/welcome-home)
;; (add-hook 'window-size-change-functions #'rh/recenter-ascii-art-on-resize)

(provide 'knot-home)
