;;; quick-mx.el --- Toggle seamlessly between command launchers with M-x -*- lexical-binding: t; -*-

(defvar rh/quick-commands
  '(jinx-correct-word
    delete-duplicate-lines
    recover-this-file
    rename-visited-file
    avy-copy-line
    avy-copy-region
    avy-move-line
    avy-move-region
    rh/return-home
    sort-lines
    just-one-space
    whitespace-cleanup
    delete-matching-lines
    delete-non-matching-lines
    dictionary-search
    dictionary-lookup-definition
    follow-mode)
  "List of commands for `quick-mx`.")

(defun rh/quick-launcher ()
  "Run a hand-picked quick command using completion."
  (interactive)
  (let ((cands (mapcar #'symbol-name rh/quick-commands)))
    (let ((name (completing-read "Quick M-x " cands nil t)))
      (call-interactively (intern name)))))

(defun rh--current-minibuffer-prompt ()
  "Return the prompt text of the active minibuffer as a string."
  (when (minibufferp)
    (buffer-substring-no-properties (point-min) (minibuffer-prompt-end))))

(defun rh/quick-mx ()
  "Toggle between full `M-x` and `rh/quick-launcher`, preserving input. "
  (interactive)
  (unless (minibufferp)
    (user-error "Not in a minibuffer"))
  (let ((typed (minibuffer-contents-no-properties))
        (is-quick
         (and (minibufferp)
              (string-match-p "Quick M-x" (or (rh--current-minibuffer-prompt) "")))))
    ;; Use setup hook so the new minibuffer gets the typed text inserted.
    (minibuffer-with-setup-hook (lambda () (insert typed))
      (if is-quick
          (call-interactively #'execute-extended-command) ;; open full M-x
        (call-interactively #'rh/quick-launcher)))))

;; Important for `quick-mx' to work
(setq enable-recursive-minibuffers t)

(provide 'quick-mx)
