;;; rh-edit.el --- My personal editing mode, where every editing task is a single keystroke. -*- lexical-binding: t; -*-

;; Author: Ronald Huidrom <ronhuidrom@gmail.com>
;; Maintainer: Ronald Huidrom <ronhuidrom@gmail.com>
;; Created: 08 Nov 2025
;; Keywords: convenience, efficiency, editing

;;; Commentary:

;; `rh-edit' is a small package for efficient editing. It can also be used as a guide
;; by beginners in Emacs Lisp in their journey to write efficient custom Lisp commands.
;; This mini package has no external dependencies whatsoever.

;;; Code:

;;;###autoload
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

;;;###autoload
(defun rh/visit-next-sexp (&optional arg)
  "Move the point to the inside of the next sexp of the same level.
With ARG, it moves forward ARG times.
With negative ARG, it moves backward that many times."
  (interactive "p")
  (up-list 1 t t)
  (forward-sexp arg nil)
  (backward-sexp 1 nil)
  (forward-char 1))

;;;###autoload
(defun rh/visit-previous-sexp (&optional arg)
  "Move the point to the inside of the previous sexp of the same level.
With ARG, it moves backward ARG times.
With negative ARG, it moves forward that many times."
  (interactive "p")
  (rh/visit-next-sexp (- arg)))

;;;###autoload
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

;;;###autoload
(defun rh/backward-opening-delimiter (&optional arg)
  "Jump backward to the previous opening delimiter."
  (interactive "p")
  (rh/forward-opening-delimiter (- (or arg 1))))

;;;###autoload
(defun rh/change-inside-forward (&optional arg)
  "Move the point to the next balanced expression and kill its contents.
A positive prefix argument \\[universal-argument] can be supplied
to signify how many balanced expressions to jump forward.
A negative argument jumps backwards."
  (interactive "p")
  (let ((arg (or arg 1)))
    (rh/forward-opening-delimiter arg)
    (when (< arg 0)
      (forward-char 1))
    (rh/act-inside)))

;;;###autoload
(defun rh/unwrap-parent-sexp (&optional arg)
  "Remove delimiters of the parent sexp.
With ARG, climb up the sexp hierarchy."
  (interactive "p")
  (save-excursion
    (backward-up-list arg t t)
    (delete-pair 1)))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun rh/backward-chop-off-sexp (&optional arg)
  "Chop off backward the rest of the higher level sexp."
  (interactive "P")
  (rh/chop-off-sexp (not arg)))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun rh/isearch-remote-yank (&optional arg)
  "While in `isearch-mode', yanks the word in search match at the original point.
With ARG, yank that many words; negative ARG yanks that many previous words."
  (interactive "p")
  (if isearch-mode
      (progn
        (rh/isearch-remote-copy arg)
        (yank 1))
    (message "This command should be invoked in isearch-mode.")))

;;;###autoload
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

;;;###autoload
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

(defun rh--break-thing (motion-fn char &optional arg)
  "Break the text at each boundary detected by MOTION-FN with CHAR.
For instance, if MOTION-FN is `forward-sentence' and CHAR is a newline character,
then it starts the next sentence in a newline and move the point there.

This function is also region aware. For instance, when there is an active region,
it inserts CHAR after each boundary detected by MOTION-FN inside the region."
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (positions '()))
        ;; collect positions within the region
        (save-excursion
          (goto-char beg)
          (while (< (point) end)
            (let ((p (progn (funcall motion-fn 1) (point))))
              (when (<= p end)
                (push p positions)))))
        ;; insert breaks (newlines) at the positions
        (save-excursion
          (dolist (pos positions)
            (goto-char pos)
            (unless (looking-at-p "\n")
              (insert char))
            (delete-horizontal-space nil))))
    ;; if no active region
    (dotimes (_ (or arg 1))
      (funcall motion-fn 1)
      (unless (looking-at-p "\n")
        (insert char))
      (delete-horizontal-space nil))))

;;;###autoload
(defun rh/break-sexp (&optional arg)
  "Start the next sexp in a newline and move the cursor there.
  With ARG, perform this action that many times.
  If there is an active region, break all sexps in the region."
  (interactive "p")
  (rh--break-thing #'forward-sexp "\n" arg))

;;;###autoload
(defun rh/break-sentence (&optional arg)
  "Start the next sentence in a newline and move the cursor there.
  With ARG, perform this action that many times.
  If there is an active region, break all sentences in the region."
  (interactive "p")
  (rh--break-thing #'forward-sentence "\n" arg))

;;;###autoload
(defun rh/dash-backwards (&optional arg)
  "Go back to insert ARG dashes and then restore original point.

With there is an active region, put a dash between each subword.
Otherwise, when a prefix argument \\[universal-argument] is given,
insert dashes at that many subword boundaries.
When ran interactively without argument, go back a subword and
put a dash unless there is already one, in which case keep going
backwards by a subword until a dash is inserted.
ARG defaults to 1."
  (interactive "p")
  (save-excursion
    (cond
     ;; if region active
     ((use-region-p)
      (let ((end (region-end)))
        (rh--break-thing #'subword-forward "-")
        ;; then delete extra dash inserted
        (when (thing-at-point-looking-at "-")
          (delete-char 1))))
     ;; if ran interactively without arguments
     ((eq arg 1)
      (progn
        (catch 'done
          (while t
            (when (bobp)
              (insert "-")
              (throw 'done t))
            (subword-backward)
            (delete-horizontal-space)
            (let ((cb (char-before)))
              (if (or (null cb) (/= cb ?-))
                  (progn
                    (insert "-")
                    (throw 'done t))))))))
     ;; with prefix argument
     (t (progn
          (dotimes (_ arg)
            (subword-backward)
            (delete-horizontal-space)
            (insert "-")))))))

;;;###autoload
(defun rh/kill-whole-paragraph (&optional arg)
  "Kill ARG whole paragraphs. ARG defaults to 1."
  (interactive "p")
  (mark-paragraph arg t)
  (kill-region (region-beginning) (region-end) nil))

;;;###autoload
(defun rh/chop-off-buffer (&optional arg)
  "Kill the rest of the buffer after point.
  With ARG, it deletes instead (does not save to the kill-ring)."
  (interactive "P")
  (if arg
      (delete-region (point) (point-max))
    (kill-region (point) (point-max))))

;;;###autoload
(defun rh/backward-chop-off-buffer (&optional arg)
  "Kill the rest of the buffer before point.
  With ARG, it deletes instead (does not save to the kill-ring)."
  (interactive "P")
  (if arg
      (delete-region (point-min) (point))
    (kill-region (point-min) (point))))

;;;###autoload
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

;;;###autoload
(defun rh/open-line-below (&optional arg)
  "Create a new line below and move the cursor there.
  With ARG, perform this action that many times."
  (interactive "p")
  (move-end-of-line 1)
  (newline-and-indent arg))

;;;###autoload
(defun rh/open-line-above (&optional arg)
  "Create a new line above and move the cursor there.
  With ARG, perform this action that many times."
  (interactive "p")
  (back-to-indentation)
  (open-line arg))

;;;###autoload
(defun rh/join-line (&optional arg)
  "Join the current line to the following line.
  With positive ARG, perform this action that many times.
  With negative ARG, join the current line to the previous and
  the magnitude of ARG determines how many times this action is performed.
  If there is an active region, join all lines in the region."
  (interactive "p")
  (if (use-region-p)
      (join-line nil (region-beginning) (region-end))
    (save-excursion
      (if (> arg 0)
          (dotimes (_ arg)
            (join-line -1))
        (dotimes (_ (- arg))
          (join-line nil))))))

;;;###autoload
(defun rh/backward-zap-up-to-char (arg char &optional interactive)
  "Like `zap-up-to-char' but backwards, to avoid typing negative argument."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (read-char-from-minibuffer "Backward zap up to char: "
                                    nil 'read-char-history)
         t))
  (zap-up-to-char (- arg) char interactive))

;;;###autoload
(defun rh/backward-zap-to-char (arg char &optional interactive)
  "Like `zap-to-char' but backwards, to avoid typing negative argument."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (read-char-from-minibuffer "Backward zap to char: "
                                    nil 'read-char-history)
         t))
  (zap-to-char (- arg) char interactive))

;;; `Do What I Mean' commands for killing, copying and commenting

;;;###autoload
(defun rh/kill-region-dwim (&optional arg)
  "Kill ARG whole lines or region.
  ARG can be either positive or negative."
  (interactive "p")
  (if (use-region-p)
      (kill-region nil nil t)
    (kill-whole-line arg)))

;;;###autoload
(defun rh/kill-ring-save-dwim (&optional arg)
  "Copy ARG whole lines or region.
  ARG can be either positive or negative."
  (interactive "p")
  (if (use-region-p)
      (kill-ring-save nil nil t)
    (save-mark-and-excursion
      (beginning-of-line)
      (let ((beg (point)))
        (end-of-line arg)
        (kill-ring-save beg (point) nil)))))

;;;###autoload
(defun rh/comment-whole-line-or-region (&optional arg)
  "Comment or uncomment ARG whole lines or region.

  Comment or uncomment the active region if there is one.
  Otherwise, comment or uncomment ARG whole lines.
  ARG can be either positive or negative.
  If ARG is zero, add a comment at the end of the current line
  and move the point there."
  (interactive "p")
  (cond
   ;; if region active
   ((use-region-p) (comment-dwim nil))
   ;; if ARG is zero
   ((zerop arg) (progn
                  (comment-dwim nil)
                  (insert " ")
                  (just-one-space)))
   ;; if ARG is one and line is empty
   ((and (eq arg 1)
         (eq (line-beginning-position) (line-end-position)))
    (progn
      (comment-dwim nil)
      (end-of-line)))
   ;; any other case
   (t (save-mark-and-excursion
        (beginning-of-line)
        (set-mark (point))
        (end-of-line arg)
        (comment-dwim nil)))))

;;;###autoload
(defun rh/replace-line-or-region (&optional arg)
  "Replace current line with the ARGth most recent kill.
  If there is active region, replace the active region instead.
  ARG defaults to 1."
  (interactive "p")
  (if (use-region-p)
      (progn
        (kill-region (region-beginning) (region-end) nil)
        (yank (1+ arg)))
    (kill-whole-line 1)
    (yank (1+ arg))
    (insert "\n")))

;;; GNU Emacs, out of the box, lacks commands for marking symbol and going back a symbol
;;; `rh/mark-symbol' is like `mark-word' but for symbols
;;; `rh/backward-symbol' is backward version of `forward-symbol'

;;;###autoload
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

;;;###autoload
(defun rh/backward-symbol (&optional arg)
  "Move point to the previous position that is the beginning of a symbol.
  With ARG, perform this action that many times."
  (interactive "p")
  (forward-symbol (- arg)))

;;;###autoload
(defun rh-edit-default-bindings ()
  "Enable default keybindings for `rh-edit'."
  (interactive)
  ;; Global keys
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
  (global-set-key (kbd "M-j") 'rh/break-sexp)
  (global-set-key (kbd "M-J") 'rh/break-sentence)
  (global-set-key (kbd "C-\"") 'rh/dash-backwards)
  (global-set-key (kbd "C-M-S-u") 'rh/unwrap-parent-sexp)
  (global-set-key (kbd "C-M-w") 'rh/copy-sexp-at-or-around-point)
  (global-set-key (kbd "M-M") 'rh/mark-symbol)
  (global-set-key (kbd "<Ci>") 'forward-symbol)
  (global-set-key (kbd "C-S-i") 'rh/backward-symbol)
  (global-set-key (kbd "H-z") 'rh/backward-zap-up-to-char)
  (global-set-key (kbd "H-Z") 'rh/backward-zap-to-char)
  (global-set-key (kbd "C-w") 'rh/kill-region-dwim)
  (global-set-key (kbd "M-w") 'rh/kill-ring-save-dwim)
  (global-set-key (kbd "M-;") 'rh/comment-whole-line-or-region)
  (global-set-key (kbd "C-S-y") 'rh/replace-line-or-region)
  ;; `isearch-mode' keys
  (define-key isearch-mode-map (kbd "C-;") 'rh/isearch-remote-copy)
  (define-key isearch-mode-map (kbd "C-x C-y") 'rh/isearch-remote-yank))

(provide 'rh-edit)

;;; rh-edit.el ends here
