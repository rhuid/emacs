;;; knot-macros.el --- Some useful macros -*- lexical-binding: t; -*-

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
