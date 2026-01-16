(setenv "LD_LIBRARY_PATH"
        (concat "/opt/z3/bin:"
                (or (getenv "LD_LIBRARY_PATH") "")) nil)

(setenv "CLASSPATH"
        (concat "/opt/z3/bin/com.microsoft.z3.jar:"
                (or (getenv "CLASSPATH") "")) nil)

(add-to-list 'exec-path "/sbin")
(setenv "PATH" (concat "/sbin:" (getenv "PATH")))

(defun eshell/addsub (video)
  "Mux VIDEO with VIDEO.srt into VIDEO.mkv using ffmpeg."
  (let* ((base (file-name-sans-extension video))
         (subs (concat base ".srt"))
         (out  (concat base ".mkv")))
    (unless (file-exists-p video)
      (error "Video file not found: %s" video))
    (unless (file-exists-p subs)
      (error "Subtitle file not found: %s" subs))
    (eshell-command
     (format
      "ffmpeg -i %s -i %s -map 0 -map 1 -c:v copy -c:a copy -c:s srt -metadata:s:s:0 language=eng %s"
      (shell-quote-argument video)
      (shell-quote-argument subs)
      (shell-quote-argument out)))))
