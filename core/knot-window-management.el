;;; knot-window-management.el --- Most things about windows -*- lexical-binding: t; -*-

;;; Adjust the other of the first two windows showing the same buffer,
;;; without moving the selected window's point.
(require 'cl-lib)

(defun rh/continuous-two-windows ()
  "Make two windows of the same buffer continuous."
  (interactive)
  (let* ((wins (window-list (selected-frame) 'no-mini))
         ;; only windows that show the same buffer as the selected buffer
         (same-buf-wins
          (cl-remove-if-not
           (lambda (w) (eq (window-buffer w) (current-buffer)))
           wins)))
    (if (< (length same-buf-wins) 2)
        (progn
          (split-window-right)
          (rh/continuous-two-windows))

      ;; sort windows in visual order (top-to-bottom, left-to-right)
      (setq same-buf-wins
            (sort same-buf-wins
                  (lambda (w1 w2)
                    (let ((e1 (window-edges w1))
                          (e2 (window-edges w2)))
                      (or (< (cadr e1) (cadr e2))    ; compare top edge
                          (< (car e1) (car e2))))))) ; then left edge
      (let* ((w1 (nth 0 same-buf-wins))
             (w2 (nth 1 same-buf-wins))
             (sel (selected-window)))
        (cond
         ;; selected is the first window -> update window 2 to start after w1's end
         ((eq sel w1)
          (let* ((end-pos (or (window-end w1 t) (point-max)))
                 (end-line (line-number-at-pos end-pos))
                 ;; start the second window at (end-line - 1) to create a one-more-line overlap.
                 (start-line (max 1 (1- end-line)))
                 (start2
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- start-line))
                    (point))))
            (set-window-start w2 start2)
            (message "Updated second window to begin after first window's end.")))

         ;; selected is the second window -> update window 1 to end just before w2's start
         ((eq sel w2)
          (let* ((s2 (window-start w2))
                 ;; line number of window-start of w2
                 (ln-s2 (line-number-at-pos s2))
                 ;; desired first-line number for w1 so that w1's page has length of (window-body-height w1)
                 (desired-first-line (max 1 (- ln-s2 (window-body-height w1))))
                 ;; compute character position of desired-first-line (safe, won't leave buffer)
                 (start1
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- desired-first-line))
                    (point))))
            (set-window-start w1 start1)
            (message "Updated first window to end before second window's start.")))

         ;; selected is neither w1 nor w2
         (t
          (message "Select either the first or second window (of the two showing this buffer).")))))))

(global-set-key (kbd "C-S-c") #'rh/continuous-two-windows)

(provide 'knot-window-management)
