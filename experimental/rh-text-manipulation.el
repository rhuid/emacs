(defun delete-enclosing-delimiters ()
  "Remove the nearest enclosing delimiter pair around point.
Supports (), [], {}, <>, double-quote (\"),
single-quote (') and backtick (`).
If no matching enclosing pair is found, signals an error."
  (interactive)
  (let* ((pairs '(("(" . ")") ("[" . "]") ("{" . "}") ("<" . ">")
                  ("\"" . "\"") ("'" . "'") ("`" . "`")))
         (open-chars (mapconcat #'car pairs ""))
         (pt (point))
         (found nil)
         opener-pos opener-char closer-pos)
    (save-excursion
      ;; search backward for a candidate opener
      (let ((pos (1- pt))
            (min (point-min)))
        (while (and (>= pos min) (not found))
          (let ((c (char-after pos)))
            (when c
              (let ((s (string c)))
                (when (assoc s pairs)
                  ;; try to find matching closer for this opener
                  (let ((close (cdr (assoc s pairs))))
                    (cond
                     ;; Quote types: find next unescaped same char
                     ((member s '("\"" "'" "`"))
                      (let ((i (1+ pos)) (eob (point-max)) match)
                        (while (and (<= i eob) (not match))
                          (let ((cc (char-after i)))
                            (when (and cc (= cc (string-to-char s)))
                              ;; check escaped with preceding backslash
                              (let ((prev (and (> i (point-min)) (char-before i))))
                                (unless (and prev (= prev ?\\))
                                  (setq match i))))
                            (setq i (1+ i))))
                        (when match
                          (setq found t
                                opener-pos pos
                                opener-char s
                                closer-pos match)) ))
                     ;; Non-quote types: scan forward counting same openers
                     (t
                      (let* ((open-c (string-to-char s))
                             (close-c (string-to-char close))
                             (i (1+ pos))
                             (eob (point-max))
                             (depth 1)
                             (match nil))
                        (while (and (<= i eob) (not match))
                          (let ((cc (char-after i)))
                            (cond
                             ((null cc) (setq i (1+ i)))
                             ((= cc open-c) (setq depth (1+ depth)))
                             ((= cc close-c)
                              (setq depth (1- depth))
                              (when (= depth 0) (setq match i))))
                            (setq i (1+ i))))
                        (when match
                          (setq found t
                                opener-pos pos
                                opener-char s
                                closer-pos match)))))))))
            (setq pos (1- pos))))))
    (if (not found)
        (user-error "No enclosing delimiter pair found around point")
      ;; delete closer then opener (delete closer first to not shift opener)
      (goto-char closer-pos) (delete-char 1)
      (goto-char opener-pos) (delete-char 1)
      (message "Removed %s...%s" opener-char (cdr (assoc opener-char pairs))))))
