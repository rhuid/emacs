;;; knot-misc.el --- Miscellaneous stuffs, that do not fit in other modules -*- lexical-binding: t; -*-

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
