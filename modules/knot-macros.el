;;; knot-macros.el --- Some useful macros -*- lexical-binding: t; -*-

(defmacro times (reps command)
  "Return an interactive lambda that runs COMMAND REPS times interactively."
  `(lambda ()
     "This repeats a command a fixed number of times. Check `times' in the config."
     (interactive)
     (ignore-errors
       (dotimes (_ ,reps)
         (call-interactively #',command)))))

(defmacro lamb (&rest body)
  "Return an interactive lambda that ignore errors."
  `(lambda ()
     "This is an anonymous command defined using `lamb'. Check config for more info."
     (interactive)
     (ignore-errors ,@body)))

(provide 'knot-macros)
