;; Snippet engine (language-agnostic)

;; Shared placeholder position tracker, buffer-local
(defvar-local rh/snippet-placeholder-positions nil
  "Buffer-local list of snippet placeholder marker positions.")

(defun rh/jump-or-indent (snippet-alist placeholder-positions)
  "Expand snippet if keyword matches; else jump to next `?`; else indent.
Returns the updated placeholder-positions list."              
  (let* ((word (thing-at-point 'symbol t))                     ; get the symbol (word) at point, e.g., "def" — returns nil if none
         (word (and word (string-trim word)))                  ; if a word was found, trim surrounding whitespace
         (expansion (assoc-default word snippet-alist))        ; lookup the corresponding snippet in the alist
         new-posns)                                            ; create a local variable `new-posns` to store updated markers

    (cond
     ;; CASE 1: Snippet expansion
     ((and word expansion)                                    ; If there was a word and it has a matching snippet
      (let* ((bounds (bounds-of-thing-at-point 'symbol))      ; Get (start . end) positions of the word under cursor
             (start (car bounds)))                            ; Extract the start position

        (delete-region (car bounds) (cdr bounds))             ; Delete the word from the buffer
        (insert expansion)                                    ; Insert the corresponding snippet text at the point

        (setq new-posns nil)                                  ; Initialize the placeholder list to empty

        (save-excursion                                       ; Don't move point while searching through the snippet
          (goto-char start)                                   ; Move to where the snippet was inserted
          (while (search-forward "?" nil t)                   ; Search for all `?` placeholders in the inserted text
            (replace-match "")                                ; Replace the literal "?" with nothing (remove it)
            (let ((m (point-marker)))                         ; Create a marker at the placeholder position
              (push m new-posns))))                           ; Add the marker to the list (LIFO stack)

        (setq new-posns (nreverse new-posns))                 ; Reverse to maintain original order of placeholders
        (when new-posns
          (goto-char (pop new-posns)))))                      ; If any placeholders were found, jump to the first one

     ;; CASE 2: Jump to next placeholder if available
     (placeholder-positions                                   ; If the placeholder list is non-nil
      (let ((next (pop placeholder-positions)))               ; Pop the next marker from the list
        (when next (goto-char next)))                         ; If it's valid, move point there
      (setq new-posns placeholder-positions))                 ; Update the local state with remaining markers

     ;; CASE 3: Default behavior — just indent line
     (t
      (indent-for-tab-command)                                ; Run normal indent (default TAB behavior)
      (setq new-posns placeholder-positions)))                ; Keep placeholder state unchanged

    new-posns))                                               ; Return the new placeholder list (possibly updated)

(provide 'rh-snip)
