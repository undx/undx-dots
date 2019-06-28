(message "Loading local configuration file for system %s" system-name)
(set-face-attribute 'default nil :font "Meslo LG M DZ for Powerline-13")
(setq user-full-name    "Emmanuel GALLOIS"
      user-mail-address "egallois@free.fr"
      user-login-name   "undx")
(let ((tmp  "~/tmp/emacs"))
  (prin1 tmp)
  (unless (file-exists-p tmp)
    (make-directory tmp t)))
(setq temporary-file-directory "~/tmp/emacs/")
(setq package-user-dir (concat temporary-file-directory "/elpa/"))
