;; Emacs daemon
;; Remove all lines starting with ;; and save it
;; at ~/.config/systemd/user/emacs.service

[Unit]
Description=Emacs Daemon

[Service]
Type=forking
ExecStart=/usr/bin/emacs --daemon
ExecStop=/usr/bin/emacsclient --eval "(kill-emacs)"
Restart=always

[Install]
WantedBy=default.target

;; To enable and start do the following
;; systemctl --user enable emacs
;; systemctl --user start emacs

;; To open Emacs (GUI), use
;; emacsclient -c -a ""
;; -c for new frame, -a "" starts daemon if needed

;; To open Emacs (terminal), use
;; emacsclient -t

;; To edit a file
;; emacsclient -c <filepath>

;; Making alias (optional)
;; alias emacs='emacsclient -c -a ""'
