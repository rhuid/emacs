;;; knot-welcome-home --- A minimal and somewhat dry startup page -*- lexical-binding: t; -*-

(defun rh/welcome-home ()
  "Show custom startup screen with ASCII art and welcome message."
  (let ((buf (get-buffer-create "*Home*"))
        (banner (expand-file-name "banner.txt" user-emacs-directory)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)

        ;; ;; Insert top text
        ;; (insert "\n\n\n\n\n")
        ;; (insert (make-string 132 ?\s))
        ;; (insert (propertize "Welcome home, Ronald" 'face '(:height 2.2 :weight normal)))
        ;; (insert "\n")
        ;; (insert "\n\n")

        ;; ;; Insert ASCII art
        ;; (when (file-exists-p banner)
        ;;   (insert-file-contents banner))

        ;; ;; Insert bottom text
        ;; (goto-char (point-max))
        ;; (insert "\n")
        ;; (insert (make-string 134 ?\s))
        ;; (insert (propertize "The Editor & Operating System" 'face '(:height 1.3 :slant italic)))
        ;; (insert "\n\n")

	;; ;; Quote
        ;; (insert (make-string 123 ?\s))
        ;; (insert (propertize "> A mathematician is a machine for turning coffee into theorems" 'face '(:height 1.0 :slant italic)))
        ;; (insert "\n\n")

	;; (dashboard-insert-center
	;;  "Hello, just checking out if this is working correctly"
	;;  "You are telling me that this is a working solution")

	;; (dashboard-insert-centered-line "Emacs is life.")
	;; (dashboard-insert-centered-line "This is truly centered.")
	;; (dashboard-insert-centered-line "Enjoy!")



	;; (insert-centered-ascii-art-dynamically (expand-file-name "logo/Emacs-Bloody.txt" user-emacs-directory))

	(dashboard-load-ascii-art-file (expand-file-name "logo/Emacs-Bloody.txt" user-emacs-directory))


	;; (insert-centered-line "Emacs is life.")
	;; (insert-centered-line "Truly centered using spaces.")
	;; (insert-centered-line "Monospace FTW.")


	;; Final touches
	(goto-char (point-min))
	(read-only-mode 1)
	(fundamental-mode)
	(setq-local cursor-type nil)
	(setq-local display-line-numbers nil)
	(setq-local display-line-numbers-mode nil)))
    (switch-to-buffer buf)))

(add-hook 'emacs-startup-hook #'rh/welcome-home)
(add-hook 'server-after-make-frame-hook #'rh/welcome-home)


;; (defun dashboard-string-pixel-width (str)
;;   "Return the pixel width of string STR using current buffer font."
;;   (car (window-text-pixel-size nil nil nil nil str)))

;; (defun dashboard-str-len (str)
;;   "Calculate STR in pixel width."
;;   (let ((width (frame-char-width))
;;         (len (dashboard-string-pixel-width str)))
;;     (+ (/ len width)
;;        (if (zerop (% len width)) 0 1))))  ; add one if exceeed

;; (defun dashboard--find-max-width (start end)
;;   "Return the max width within the region START and END."
;;   (save-excursion
;;     (goto-char start)
;;     (let ((width 0))
;;       (while (< (point) end)
;;         (let* ((line-str (buffer-substring (line-beginning-position) (line-end-position)))
;;                (line-length (dashboard-str-len line-str)))
;;           (setq width (max width line-length)))
;;         (forward-line 1))
;;       width)))

;; (defun dashboard-center-text (start end)
;;   "Center the text between START and END."
;;   (let* ((width (dashboard--find-max-width start end))
;;          (prefix (propertize " " 'display `(space . (:align-to (- center ,(/ (float width) 2)))))))
;;     (add-text-properties start end `(line-prefix ,prefix indent-prefix ,prefix))))

;; (defun dashboard-insert-center (&rest strings)
;;   "Insert STRINGS in the center of the buffer."
;;   (let ((start (point)))
;;     (apply #'insert strings)
;;     (dashboard-center-text start (point))))


;; (defun dashboard-center-line (str)
;;   "Return STR with enough leading space to center it horizontally."
;;   (let* ((win-width (window-pixel-width))
;;          (str-width (dashboard-string-pixel-width str))
;;          (left-pad (/ (- win-width str-width) 2)))
;;     (concat (propertize " " 'display `(space :width (,left-pad))) str "\n")))

;; (defun dashboard-insert-center (&rest strings)
;;   "Insert each string in STRINGS centered horizontally."
;;   (dolist (str strings)
;;     (insert (dashboard-center-line str))))




;; (defun dashboard-insert-centered-line (str)
;;   "Insert STR centered horizontally in the current window."
;;   (let* ((win-width (window-pixel-width))
;;          (str-width (dashboard-string-pixel-width str))
;;          (left-pad (max 0 (/ (- win-width str-width) 2))))
;;     (insert (propertize " " 'display `(space :width (,left-pad))))
;;     (insert str "\n")))




;; (defun insert-centered-line (str)
;;   "Insert STR centered in the current window using spaces. Assumes monospace font."
;;   (let* ((win-cols (window-width))
;;          (str-len (length str))
;;          (pad (/ (max 0 (- win-cols str-len)) 2)))
;;     (insert (make-string pad ?\s))
;;     (insert str "\n")))







;; (defcustom dashboard-image-banner-max-width 0
;;   "Maximum width of banner image.

;; This setting applies if Emacs supports image transforms or compiled
;; with Imagemagick support.  When value is non-zero the image banner
;; will be resized to the specified width in pixels, with aspect ratio
;; preserved."
;;   :type 'integer
;;   :group 'dashboard)

;; (defcustom dashboard-image-banner-max-height 0
;;   "Maximum height of banner image.

;; This setting applies only if Emacs supports image transforms or
;; compiled with Imagemagick support.  When value is non-zero the image
;; banner will be resized to the specified height in pixels, with aspect
;; ratio preserved."
;;   :type 'integer
;;   :group 'dashboard)

;; (defcustom dashboard-image-extra-props nil
;;   "Additional image attributes to assign to the image.
;; This could be useful for displaying images with transparency,
;; for example, by setting the `:mask' property to `heuristic'.
;; See `create-image' and Info node `(elisp)Image Descriptors'."
;;   :type 'plist
;;   :group 'dashboard)

;; (defcustom dashboard-banner-ascii "EMACS"
;;   "String to be shown in place of the startup banner
;; if `dashboard-startup-banner' is set to `ascii'."
;;   :type 'string
;;   :group 'dashboard)

;; (defcustom dashboard-startup-banner 'official
;;   "Specify the banner type to use.
;; Value can be
;; - \\='official  displays the official Emacs logo.
;; - \\='logo  displays an alternative Emacs logo.
;; - an integer which displays one of the text banners.
;; - a string that specifies the path of an custom banner
;; supported files types are gif/image/text/xbm.
;; - a cons of 2 strings which specifies the path of an image to use
;; and other path of a text file to use if image isn't supported.
;; - a list that can display an random banner, supported values are:
;; string (filepath), \\='official, \\='logo and integers."
;;   :type '(choice (const   :tag "official"  official)
;;                  (const   :tag "logo"      logo)
;;                  (const   :tag "ascii"     ascii)
;;                  (integer :tag "index of a text banner")
;;                  (string  :tag "path to an image or text banner")
;;                  (cons    :tag "image and text banner"
;;                           (string :tag "image banner path")
;;                           (string :tag "text banner path"))
;;                  (repeat :tag "random banners"
;;                          (choice (string  :tag "a path to an image or text banner")
;;                                  (const   :tag "official" official)
;;                                  (const   :tag "logo"     logo)
;;                                  (const   :tag "ascii"    ascii)
;;                                  (integer :tag "index of a text banner"))))
;;   :group 'dashboard)

;; (defface dashboard-text-banner
;;   '((t (:inherit font-lock-keyword-face)))
;;   "Face used for text banners."
;;   :group 'dashboard)

;; (defun dashboard--find-max-width (start end)
;;   "Return the max width within the region START and END."
;;   (save-excursion
;;     (goto-char start)
;;     (let ((width 0))
;;       (while (< (point) end)
;;         (let* ((line-str (buffer-substring (line-beginning-position) (line-end-position)))
;;                (line-length (dashboard-str-len line-str)))
;;           (setq width (max width line-length)))
;;         (forward-line 1))
;;       width)))

;; (defun dashboard-choose-banner (banner)
;;   "Return a plist specifying the chosen banner based on BANNER."
;;   (pcase banner
;;     ('official
;;      (append (when (image-type-available-p 'png)
;;                (list :image dashboard-banner-official-png))
;;              (list :text (dashboard-get-banner-path 1))))
;;     ('logo
;;      (append (when (image-type-available-p 'png)
;;                (list :image dashboard-banner-logo-png))
;;              (list :text (dashboard-get-banner-path 1))))
;;     ('ascii
;;      (append (list :text dashboard-banner-ascii)))
;;     ((pred integerp)
;;      (list :text (dashboard-get-banner-path banner)))
;;     ((pred stringp)
;;      (pcase banner
;;        ((pred (lambda (f) (not (file-exists-p f))))
;;         (message "could not find banner %s, use default instead" banner)
;;         (list :text (dashboard-get-banner-path 1)))
;;        ((pred (string-suffix-p ".txt"))
;;         (list :text (if (file-exists-p banner)
;;                         banner
;;                       (message "could not find banner %s, use default instead" banner)
;;                       (dashboard-get-banner-path 1))))
;;        ((pred dashboard--image-supported-p)
;;         (list :image banner
;;               :text (dashboard-get-banner-path 1)))
;;        (_
;;         (message "unsupported file type %s" (file-name-nondirectory banner))
;;         (list :text (dashboard-get-banner-path 1)))))
;;     ((and
;;       (pred listp)
;;       (pred (lambda (c)
;;               (and (not (proper-list-p c))
;;                    (not (null c)))))
;;       `(,img . ,txt))
;;      (list :image (if (dashboard--image-supported-p img)
;;                       img
;;                     (message "could not find banner %s, use default instead" img)
;;                     dashboard-banner-official-png)
;;            :text (if (and (file-exists-p txt) (string-suffix-p ".txt" txt))
;;                      txt
;;                    (message "could not find banner %s, use default instead" txt)
;;                    (dashboard-get-banner-path 1))))
;;     ((and
;;       (pred proper-list-p)
;;       (pred (lambda (l) (not (null l)))))

;;      (let* ((max (length banner))
;;             (choose (nth (random max) banner)))
;;        (dashboard-choose-banner choose)))
;;     (_
;;      (user-error "Unsupported banner type: `%s'" banner)
;;      nil)))

;; (defun dashboard--image-animated-p (image-path)
;;   "Return if image is a gif or webp.
;; String -> bool.
;; Argument IMAGE-PATH path to the image."
;;   (memq (image-type image-path) '(gif webp)))

;; (defun dashboard--type-is-xbm-p (image-path)
;;   "Return if image is a xbm.
;; String -> bool.
;; Argument IMAGE-PATH path to the image."
;;   (eq 'xbm (image-type image-path)))

;; (defun dashboard-insert-banner ()
;;   "Insert the banner at the top of the dashboard."
;;   (goto-char (point-max))
;;   (when-let* ((banner (dashboard-choose-banner dashboard-startup-banner)))
;;     (insert "\n")
;;     (when (display-graphic-p) (insert "\n"))
;;     (let ((start (point))
;;           buffer-read-only
;;           text-width
;;           image-spec)
;;       ;; If specified, insert a text banner.
;;       (when-let* ((txt (plist-get banner :text)))
;;         (save-excursion
;;           (if (file-exists-p txt)
;;               (insert-file-contents txt)
;;             (insert txt)))
;;         (put-text-property start (point-max) 'face 'dashboard-text-banner)
;;         (setq text-width (dashboard--find-max-width start (point-max)))
;;         (goto-char (point-max)))
;;       ;; If specified, insert an image banner. When displayed in a graphical frame, this will
;;       ;; replace the text banner.
;;       (when-let* ((img (plist-get banner :image)))
;;         (let ((img-props
;;                (append (when (> dashboard-image-banner-max-width 0)
;;                          (list :max-width dashboard-image-banner-max-width))
;;                        (when (> dashboard-image-banner-max-height 0)
;;                          (list :max-height dashboard-image-banner-max-height))
;;                        dashboard-image-extra-props)))
;;           (setq image-spec
;;                 (cond ((dashboard--image-animated-p img)
;;                        (create-image img))
;;                       ((dashboard--type-is-xbm-p img)
;;                        (create-image img))
;;                       ((image-type-available-p 'imagemagick)
;;                        (apply 'create-image img 'imagemagick nil img-props))
;;                       (t
;;                        (apply 'create-image img nil nil
;;                               (when (and (fboundp 'image-transforms-p)
;;                                          (memq 'scale (funcall 'image-transforms-p)))
;;                                 img-props))))))
;;         (add-text-properties start (point) `(display ,image-spec))
;;         (when (ignore-errors (image-multi-frame-p image-spec)) (image-animate image-spec 0 t)))

;;       ;; Finally, center the banner (if any).
;;       (when-let* ((text-align-spec `(space . (:align-to (- center ,(/ text-width 2)))))
;;                   (image-align-spec `(space . (:align-to (- center (0.5 . ,image-spec)))))
;;                   (prop
;;                    (cond
;;                     ;; Both an image & text banner.
;;                     ((and image-spec text-width)
;;                      ;; The quoting is intentional. This is a conditional display spec that will
;;                      ;; align the banner at redisplay time.
;;                      `((when (display-graphic-p) . ,image-align-spec)
;;                        (when (not (display-graphic-p)) . ,text-align-spec)))
;;                     ;; One or the other.
;;                     (text-width text-align-spec)
;;                     (image-spec image-align-spec)
;;                     ;; No banner.
;;                     (t nil)))
;;                   (prefix (propertize " " 'display prop)))
;;         (add-text-properties start (point) `(line-prefix ,prefix wrap-prefix ,prefix)))
;;       (insert "\n")
;;       (add-text-properties start (point) '(cursor-intangible t inhibit-isearch t)))))








;; (defvar-local ascii-art--lines nil
;;   "Stored lines of ASCII art to re-render on window resize.")

;; (defun ascii-art--render-centered ()
;;   "Render stored ASCII art centered in the current buffer."
;;   (when ascii-art--lines
;;     (let ((inhibit-read-only t))
;;       (erase-buffer)
;;       (let* ((max-len (apply #'max (mapcar #'string-width ascii-art--lines)))
;;              (window-width (window-body-width)))
;;         (dolist (line ascii-art--lines)
;;           (let* ((padding (max 0 (/ (- window-width (string-width line)) 2)))
;;                  (spaces (make-string padding ?\s)))
;;             (insert spaces line "\n")))))))

;; (defun insert-centered-ascii-art-dynamically (file)
;;   "Load ASCII art from FILE and center it dynamically on resize."
;;   (interactive "fASCII art file: ")
;;   (let ((lines (split-string (with-temp-buffer
;;                                (insert-file-contents file)
;;                                (buffer-string))
;;                              "\n" t)))
;;     (setq ascii-art--lines lines)
;;     (ascii-art--render-centered)
;;     ;; Add dynamic resize handling
;;     (add-hook 'window-size-change-functions
;;               (lambda (_frame) (ascii-art--render-centered))
;;               nil t))) ;; buffer-local



(defvar dashboard-ascii-art nil
  "List of lines of ASCII art to be centered dynamically.")

(defun dashboard-center-ascii-art ()
  "Clear buffer and insert `dashboard-ascii-art` centered based on current frame width."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (when dashboard-ascii-art
      (let* ((max-len (apply #'max (mapcar #'string-width dashboard-ascii-art)))
             (cols (window-width))
             (padding (max 0 (/ (- cols max-len) 2))))
        (dolist (line dashboard-ascii-art)
          (insert (make-string padding ?\s) line "\n"))))))

(defun dashboard-load-ascii-art-file (file)
  "Load ASCII art from FILE and store it in `dashboard-ascii-art`.
Then insert it centered."
  (with-temp-buffer
    (insert-file-contents file)
    (setq dashboard-ascii-art (split-string (buffer-string) "\n" t)))
  (dashboard-center-ascii-art))

(defun dashboard-setup-ascii-art-frame-hook ()
  "Set up hook to re-center ASCII art when frame resizes."
  (add-hook 'window-size-change-functions
            (lambda (_frame) (when (eq major-mode 'dashboard-mode)
                          (dashboard-center-ascii-art)))))

;; Usage example:
(with-current-buffer (get-buffer-create "*Home*")
  (dashboard-mode) ;; assume you have a custom major mode or just use fundamental-mode
  (switch-to-buffer (current-buffer))
  (setq buffer-read-only t)
  (dashboard-load-ascii-art-file "~/ascii/dragon.txt")
  (dashboard-setup-ascii-art-frame-hook))




(provide 'knot-welcome-home)
