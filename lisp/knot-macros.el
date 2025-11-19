;;; knot-macros.el --- Mostly macros and other related things -*- lexical-binding: t; -*-

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
- `H-r 6 C-n`   run `C-n` 6 times
- `H-r 12 f`    run `f` 12 times."
  (interactive)
  (let* ((pair (rh--read-inline-count-and-sequence))
         (count (or (car pair) 1))
         (keys (cdr pair)))
    (dotimes (_ (max 0 count))
      (execute-kbd-macro keys))
    (message "Repeated %s %d time(s)" (key-description keys) count)))

(bind-key "H-r" 'rh/repeat-next-key-inline)

;; Extend commands such that they work as usual if there is an active region, otherwise they apply to the whole buffer.
(defmacro rh/define-region-or-buffer-command (func)
  "Define a new command named `rh/region-or-buffer--<func>' that calls FUNC on region or whole buffer if no region active."
  (let ((new-func (intern (concat "rh/region-or-buffer--" (symbol-name func)))))
    `(defun ,new-func ()
       ,(format "Call `%s' on region or whole buffer if no region active." func)
       (interactive)
       (if (use-region-p)
           (call-interactively ',func)
         (save-excursion
           (call-interactively 'mark-whole-buffer)
           (call-interactively ',func))))))

(defmacro lamb (&rest body)
  "Return an interactive lambda that ignore errors."
  `(lambda ()
     "This is an anonymous command defined using `lamb'. Check config for more info."
     (interactive)
     (ignore-errors ,@body)))

(defmacro times (reps command)
  "Return an interactive lambda that runs COMMAND REPS times interactively."
  `(lambda ()
     "This repeats a command a fixed number of times. Check `times' in the config."
     (interactive)
     (ignore-errors
       (dotimes (_ ,reps)
         (call-interactively #',command)))))

(provide 'knot-macros)
