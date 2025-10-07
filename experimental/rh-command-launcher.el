;;; rh-command-launcher.el --- Toggle seamlessly between command launchers with M-x -*- lexical-binding: t; -*-

(defvar rh/quick-commands
  '(jinx-correct-word
    delete-duplicate-lines
    recover-this-file
    rename-visited-file
    avy-copy-line
    avy-copy-region
    avy-move-line
    avy-move-region
    copy-from-above-command
    rh/return-home
    sort-lines
    just-one-space
    whitespace-cleanup
    delete-matching-lines
    delete-non-matching-lines
    dictionary-search
    dictionary-lookup-definition
    follow-mode
    visual-fill-column-mode)
  "List of commands for `rh/quick-launcher`.")

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

(defun rh/toggle-launcher ()
  "Toggle between full `M-x` and `rh/quick-launcher`, preserving input.
When called in a minibuffer:
 - If the current minibuffer prompt contains \"Quick command\" we open full M-x.
 - Otherwise we open the quick launcher.
The text already typed is reinserted into the new minibuffer so you don't retype."
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

(define-key vertico-map (kbd "M-x") #'rh/toggle-launcher)

;; (defun rh/command-launcher ()
;;   "A slimmed-down `M-x` that only shows `rh/quick-commands`."
;;   (interactive)
;;   (let* ((completion-ignore-case t)
;;          (execute-extended-command--last-command nil)
;;          (minibuffer-completion-table rh/quick-commands)
;;          (cmd (completing-read
;;                "C-x C-m "
;;                (lambda (string pred action)
;;                  (if (eq action 'metadata)
;;                      '(metadata (category . command))
;;                    (complete-with-action action rh/quick-commands string pred)))
;;                nil t nil 'extended-command-history)))
;;     (setq cmd (intern cmd))
;;     (call-interactively cmd)))

(provide 'rh-command-launcher)
