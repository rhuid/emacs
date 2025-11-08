;;; knot-misc.el --- Miscellaneous stuffs, that do not fit in other modules -*- lexical-binding: t; -*-

(defun rh--read-inline-count-and-sequence ()
  "Read optional leading digits as a COUNT, then a key sequence.
Return a cons (COUNT . KEYS), where KEYS is a string/vector suitable for
`execute-kbd-macro`."
  (let ((evt (read-event)))                 ; read first event
    (if (and (integerp evt) (>= evt ?0) (<= evt ?9))
        ;; first event is a digit: gather more digits until non-digit
        (let ((digits (list evt))
              next)
          (while (progn
                   (setq next (read-event))
                   (and (integerp next) (>= next ?0) (<= next ?9)))
            (push next digits))
          ;; next is non-digit; push it back so read-key-sequence can include it
          (setq unread-command-events (cons next unread-command-events))
          (let ((count (string-to-number (apply #'string (nreverse digits)))))
            (cons count (read-key-sequence nil))))
      ;; first event not a digit: unread it and read a normal key sequence
      (setq unread-command-events (cons evt unread-command-events))
      (cons 1 (read-key-sequence nil)))))

(defun rh/repeat-next-key-inline ()
  "Repeat a key sequence N times.

Usage: press the bound key (e.g. `H-r`), then optionally type a decimal
count (one or more digits), then type the key sequence to repeat.

Examples:
- `H-r 6 C-n`   → run `C-n` 6 times
- `H-r 12 f`    → run `f` 12 times
- `H-r C-n`     → run `C-n` once
- `H-r 3 C-x C-f` → run `C-x C-f` three times

This works by reading inline digits without needing RET."
  (interactive)
  (let* ((pair (rh--read-inline-count-and-sequence))
         (count (or (car pair) 1))
         (keys (cdr pair)))
    (dotimes (_ (max 0 count))
      (execute-kbd-macro keys))
    (message "Repeated %s %d time(s)" (key-description keys) count)))

(bind-key "H-r" 'rh/repeat-next-key-inline)

;; Custom scratch buffers
(use-package emacs
  :bind (("C-c u o" . rh/toggle-org-scratch)
         ("C-c u e" . rh/toggle-lisp-scratch))
  :config
  (defun rh/toggle-scratch-buffer (name mode initial-content)
    "Toggle a named scratch buffer NAME in MODE with optional INITIAL-CONTENT."
    (let ((buf (get-buffer name)))
      (if buf
          (if (eq (current-buffer) buf)
              (switch-to-buffer (other-buffer))
            (switch-to-buffer buf))
        (let ((new-buf (get-buffer-create name)))
          (with-current-buffer new-buf
            (funcall mode)
            (unless (string= (buffer-string) "") (erase-buffer))
            (insert initial-content)
            (goto-char (point-max)))
          (switch-to-buffer new-buf)))))
  (defun rh/toggle-org-scratch ()
    "Toggle `*org-scratch*` buffer."
    (interactive)
    (rh/toggle-scratch-buffer "*org-scratch*" #'org-mode "#+TITLE: Org Scratch\n\n"))
  (defun rh/toggle-lisp-scratch ()
    "Toggle `*lisp-scratch*'."
    (interactive)
    (rh/toggle-scratch-buffer "*lisp-scratch*" #'lisp-interaction-mode ";; Lisp Scratch\n\n")))

;; Date Formats for use in `yasnippet'
(defun rh/date-format-candidates ()
  "Return an alist of (display . format-string) for yasnippet date choices."
  (mapcar (lambda (fmt)
            (cons (format "%-20s → %s" fmt (format-time-string fmt)) fmt))
          '("%Y-%m-%d"                                                           ; 2025-09-19
            "%d/%m/%Y"                                                           ; 19/09/2025
            "%A, %B %d, %Y"                                                      ; Friday, September 19, 2025
            "%b %d, %Y"                                                          ; Sep 19, 2025
            "%Y-%m-%d %H:%M")))                                                  ; 2025-09-19 20:31

(provide 'knot-misc)
