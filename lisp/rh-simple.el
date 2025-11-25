;;; rh-simple.el --- My personal editing commands and other extensions to simple.el -*- lexical-binding: t; -*-

(defun rh/act-inside (&optional arg)
  "Kill or copy the content inside the current balanced expression.
With \\[universal-argument] prefix, it copies. Otherwise, it kills."
  (interactive "P")
  (save-mark-and-excursion
    (backward-up-list 1 t t)
    (forward-char 1)
    (let ((beg (point)))
      (up-list 1 t t)
      (backward-char 1)
      (if arg
          (kill-ring-save beg (point))
        (kill-region beg (point))))))

(defun rh/visit-next-sexp (&optional arg)
  "Move the point to the inside of the next sexp of the same level.
With ARG, it moves forward ARG times.
With negative ARG, it moves backward that many times."
  (interactive "p")
  (up-list 1 t t)
  (forward-sexp arg nil)
  (backward-sexp 1 nil)
  (forward-char 1))

(defun rh/visit-previous-sexp (&optional arg)
  "Move the point to the inside of the previous sexp of the same level.
With ARG, it moves backward ARG times.
With negative ARG, it moves forward that many times."
  (interactive "p")
  (rh/visit-next-sexp (- arg)))

(defun rh/forward-opening-delimiter (&optional arg)
  "Jump forward to the next opening delimiter.
Opening delimiters are one of: ( [ { < ` \" '.
With optional ARG, jump |ARG| times; negative ARG means jump backward.

This command may not work well for nested expressions inside strings."
  (interactive "p")
  (let* ((arg (or arg 1))
         (delimiters "(\\|\\[\\|{\\|<\\|[`\"']"))
    (if (< arg 0)
        ;; when argument is negative, look backward
        (dotimes (_ (- arg))
          ;; search back for the delimiter once
          (unless (re-search-backward delimiters nil t)
            (cl-return))
          ;; if landed inside a string, do it again
          ;; because we landed on the closing delimiter, not on the opening one
          (when (eq (syntax-ppss-context (syntax-ppss)) 'string)
            (unless (re-search-backward delimiters nil t)
              (cl-return))))
      ;; when argument is positive, look forward
      (dotimes (_ arg)
        (if (eq (syntax-ppss-context (syntax-ppss)) 'string)
            ;; if the point is inside a string, do it twice
            ;; because strings use the same delimiter for both opening and closing
            (progn
              (dotimes (_ 2)
                (unless (re-search-forward delimiters nil t)
                  (cl-return))))
          ;; if the point is not inside a string, do it only once
          (unless (re-search-forward delimiters nil t)
            (cl-return)))))))

(defun rh/backward-opening-delimiter (&optional arg)
  "Jump backward to the previous opening delimiter."
  (interactive "p")
  (rh/forward-opening-delimiter (- (or arg 1))))

(defun rh/change-inside-forward (&optional arg)
  "Jump to the next delimiter and kill contents of the parent balanced expression.
Prefix argument \\[universal-argument] does the same but on the previous delimiter."
  (interactive "P")
  (if arg
      ;; jump one opening delimiter backward once
      (progn
        (rh/backward-opening-delimiter 2)
        (forward-char 1))
    (rh/forward-opening-delimiter 1))
  (rh/act-inside))

(defun rh/unwrap-parent-sexp ()
  "Remove delimiters of the parent sexp."
  (interactive)
  (save-excursion
    (backward-up-list 1 t t)
    (delete-pair 1)))

(defun rh/copy-sexp-at-or-around-point (&optional arg)
  "Copy the balanced expression at the point or around the point.
With \\[universal-argument] ARG, copy that many balanced expressions.

If the point is at the start of a balanced expression, it copies that.
Otherwise it copies the parent balanced expression that contains the point."
  (interactive "p")
  (save-mark-and-excursion
    (unless (or
             ;; match the start of a parenthesis
             (looking-at "\\s(")
             ;; match the start of a string
             (and (looking-at "\\s\"")
                  (not (eq (syntax-ppss-context (syntax-ppss)) 'string))))
      (backward-up-list 1 t t))
    (mark-sexp arg nil)
    (kill-ring-save nil nil t)))

(defun rh/chop-off-sexp (&optional arg)
  "Chop off the rest of the higher level sexp.
With \\[universal-argument], it chops backward."
  (interactive "P")
  (set-mark (point))
  (if arg
      (progn
        (backward-up-list 1 t t)
        (forward-char 1))
    (up-list 1 t t)
    (backward-char 1))
  (when (use-region-p)
    (kill-region (region-beginning) (region-end))))

(defun rh/backward-chop-off-sexp (&optional arg)
  "Chop off backward the rest of the higher level sexp."
  (interactive "P")
  (rh/chop-off-sexp (not arg)))

(defun rh/kill-whole-word (&optional arg)
  "Kill current word and try to fix up whitespace after killing.
With ARG, perform this action that many times.
Negative ARG kills that many previous words.
Also kills word backward if the point is at the end of the word."
  (interactive "p")
  (let ((arg (or arg 1)))
    (if (< arg 0)
        (progn
          (let ((b (bounds-of-thing-at-point 'word)))
            (unless (and b (= (point) (cdr b)))
              (forward-word 1)))
          (kill-word arg))
      (let ((b (bounds-of-thing-at-point 'word)))
        (unless (and b (<= (point) (car b)))
          (if b
              (goto-char (car b))
            (backward-word 1))))
      (kill-word arg)))
  (when (looking-at " +")
    (just-one-space)))

(defun rh/copy-whole-word (&optional arg)
  "Copy current word without moving point.
With ARG, copy that many words; negative ARG copies backward."
  (interactive "p")
  (let ((arg (or arg 1)))
    (save-excursion
      (let* ((start (point))
             (bounds (bounds-of-thing-at-point 'word)))
        (if (< arg 0)
            (progn
              (when (and bounds (< (point) (cdr bounds)))
                (goto-char (cdr bounds)))
              (kill-ring-save (point)
                              (progn (backward-word (- arg)) (point))))
          (progn
            (when (and bounds (> (point) (car bounds)))
              (goto-char (car bounds)))
            (kill-ring-save (point)
                            (progn (forward-word arg) (point)))))))))

(defun rh/isearch-remote-copy (&optional arg)
  "In `isearch', copy ARG words and return to the original point.
ARG defaults to 1. Negative ARG copies backward."
  (interactive "p")
  (let* ((count (or arg 1))
         (in-isearch (bound-and-true-p isearch-mode)))
    (if in-isearch
        (let* ((isearch-pos (point))
               (other-pos (or (and (boundp 'isearch-other-end) isearch-other-end)
                              isearch-pos))
               (match-start (min isearch-pos other-pos))
               (match-end   (max isearch-pos other-pos))
               (copy-pos (if (< count 0) match-end match-start))
               (orig-point (or (and (boundp 'isearch-opoint) isearch-opoint)
                               (and (boundp 'isearch--opoint) isearch--opoint)
                               (point))))
          (save-excursion
            (goto-char copy-pos)
            (rh/copy-whole-word count))
          (isearch-exit)
          (goto-char orig-point))
      (message "This command should be invoked in isearch mode."))))

(defun rh/isearch-remote-yank (&optional arg)
  "While in `isearch-mode', yanks the word in search match at the original point.
With ARG, yank that many words; negative ARG yanks that many previous words."
  (interactive "p")
  (if isearch-mode
      (progn
        (rh/isearch-remote-copy arg)
        (yank 1))
    (message "This command should be invoked in isearch-mode.")))

(defun rh/kill-whole-sentence (&optional arg)
  "Kill current sentence.
With ARG, perform this action that many times.
Negative ARG kills that many previous sentences."
  (interactive "p")
  (unless (bobp)
    (backward-char 1))
  (forward-sentence 1)
  (backward-sentence 1)
  (kill-sentence arg)
  (just-one-space))

(defun rh/copy-whole-sentence (&optional arg)
  "Copy current sentence.
With ARG, perform this action that many times.
Negative ARG copies that many previous sentences."
  (interactive "p")
  (save-excursion
    (unless (bobp)
      (backward-char 1))
    (forward-sentence 1)
    (backward-sentence 1)
    (mark-end-of-sentence arg)
    (kill-ring-save (region-beginning) (region-end))))

(defun rh/break-sentence (&optional arg)
  "Start the next sentence in a new line and move the cursor there.
With ARG, perform this action that many times."
  (interactive "p")
  (dotimes (_ arg)
    (forward-sentence 1)
    (newline 1)
    (delete-horizontal-space nil)))

(defun rh/kill-whole-paragraph (&optional arg)
  "Kill ARG whole paragraphs. ARG defaults to 1."
  (interactive "p")
  (mark-paragraph arg t)
  (kill-region (region-beginning) (region-end) nil))

(defun rh/chop-off-buffer (&optional arg)
  "Kill the rest of the buffer after point.
With ARG, it deletes instead (does not save to the kill-ring)."
  (interactive "P")
  (if arg
      (delete-region (point) (point-max))
    (kill-region (point) (point-max))))

(defun rh/backward-chop-off-buffer (&optional arg)
  "Kill the rest of the buffer before point.
With ARG, it deletes instead (does not save to the kill-ring)."
  (interactive "P")
  (if arg
      (delete-region (point-min) (point))
    (kill-region (point-min) (point))))

(defun rh/mark-whole-line (&optional arg)
  "Mark ARG whole lines.
If there is an active region, extend the region by
ARG whole lines at the other side of the point.
ARG defaults to 1."
  (interactive "p")
  (if (region-active-p)
      (if (eq (point) (use-region-beginning))
          ;; if point is at lower end, extend upwards
          (progn
            (exchange-point-and-mark nil)
            (forward-line arg)
            (end-of-line 1)
            (exchange-point-and-mark nil))
        ;; if point is at upper end, extend downwards
        (exchange-point-and-mark nil)
        (forward-line (- arg))
        (beginning-of-line)
        (exchange-point-and-mark nil))
    ;; if no active region
    (beginning-of-line)
    (set-mark (point))
    (forward-line (1- arg))
    (end-of-line)
    (exchange-point-and-mark nil)))

(defun rh/open-line-below (&optional arg)
  "Create a new line below and move the cursor there.
With ARG, perform this action that many times."
  (interactive "p")
  (move-end-of-line 1)
  (newline-and-indent arg))

(defun rh/open-line-above (&optional arg)
  "Create a new line above and move the cursor there.
With ARG, perform this action that many times."
  (interactive "p")
  (back-to-indentation)
  (open-line arg))

(defun rh/join-line (&optional arg)
  "Join the current line to the following line.
With ARG, perform this action that many times."
  (interactive "p")
  (save-excursion
    (dotimes (_ arg)
      (join-line -1))))

(defun rh/backward-zap-up-to-char (arg char &optional interactive)
  "Like `zap-up-to-char' but backwards, to avoid typing negative argument."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (read-char-from-minibuffer "Backward zap up to char: "
                                    nil 'read-char-history)
         t))
  (zap-up-to-char (- arg) char interactive))

(defun rh/backward-zap-to-char (arg char &optional interactive)
  "Like `zap-to-char' but backwards, to avoid typing negative argument."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (read-char-from-minibuffer "Backward zap up to char: "
                                    nil 'read-char-history)
         t))
  (zap-to-char (- arg) char interactive))

;;; GNU Emacs, out of the box, lacks commands for marking symbol and going back a symbol
;;; `rh/mark-symbol' is like `mark-word' but for symbols
;;; `rh/backward-symbol' is backward version of `forward-symbol'

(defun rh/mark-symbol (&optional arg allow-extend)
  "Mark ARG symbols at point. ARG defaults to 1."
  (interactive "P\np")
  (cond ((and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (region-active-p)))
         (setq arg (if arg (prefix-numeric-value arg)
                     (if (< (mark) (point)) -1 1)))
         (set-mark
          (save-excursion
            (goto-char (mark))
            (forward-symbol arg)
            (point))))
        (t
         (push-mark
          (save-excursion
            (forward-symbol (prefix-numeric-value arg))
            (point))
          nil t))))

(defun rh/backward-symbol (&optional arg)
  "Move point to the previous position that is the beginning of a symbol.
With ARG, perform this action that many times."
  (interactive "p")
  (forward-symbol (- arg)))

;; Global keybindings for the commands defined in this module
(global-set-key (kbd "M-i") 'rh/act-inside)
(global-set-key (kbd "M-I") 'rh/change-inside-forward)
(global-set-key (kbd "H-f") 'rh/visit-next-sexp)
(global-set-key (kbd "H-b") 'rh/visit-previous-sexp)
(global-set-key (kbd "C-H-f") 'rh/forward-opening-delimiter)
(global-set-key (kbd "C-H-b") 'rh/backward-opening-delimiter)
(global-set-key (kbd "C-M-y") 'up-list)
(global-set-key (kbd "C-M-)") 'rh/chop-off-sexp)
(global-set-key (kbd "C-M-(") 'rh/backward-chop-off-sexp)
(global-set-key (kbd "M-D") 'rh/kill-whole-word)
(global-set-key (kbd "C-;") 'rh/copy-whole-word)
(global-set-key (kbd "C-H-d") 'rh/kill-whole-sentence)
(global-set-key (kbd "C-H-w") 'rh/copy-whole-sentence)
(global-set-key (kbd "C-M-S-<backspace>") 'rh/kill-whole-paragraph)
(global-set-key (kbd "C-M-S-k") 'rh/chop-off-buffer)
(global-set-key (kbd "C-M-S-h") 'rh/backward-chop-off-buffer)
(global-set-key (kbd "C-'") 'rh/mark-whole-line)
(global-set-key (kbd "C-<return>") 'rh/open-line-below)
(global-set-key (kbd "C-S-<return>") 'rh/open-line-above)
(global-set-key (kbd "C-j") 'rh/join-line)
(global-set-key (kbd "M-j") 'rh/break-sentence)
(global-set-key (kbd "C-M-S-u") 'rh/unwrap-parent-sexp)
(global-set-key (kbd "C-M-w") 'rh/copy-sexp-at-or-around-point)
(global-set-key (kbd "M-M") 'rh/mark-symbol)
(global-set-key (kbd "<Ci>") 'forward-symbol)
(global-set-key (kbd "C-S-i") 'rh/backward-symbol)
(global-set-key (kbd "H-z") 'rh/backward-zap-up-to-char)
(global-set-key (kbd "H-Z") 'rh/backward-zap-to-char)

;; `isearch-mode' keybindings
(define-key isearch-mode-map (kbd "C-;") 'rh/isearch-remote-copy)
(define-key isearch-mode-map (kbd "C-Y") 'rh/isearch-remote-yank)

(provide 'rh-simple)
