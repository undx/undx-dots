(message "Loading local configuration file for system %s" system-name)
;; some informations
(setq user-full-name    "Emmanuel GALLOIS"
      user-mail-address "egallois@gtalend.com"
      user-login-name   "egallois")

(message "Loading local configuration file for system %s" (system-name))

(add-to-list 'exec-path "/usr/local/bin")
;; org-directory
(setq org-directory "~/Dropbox/Notes/")

(set-face-attribute 'default nil :font "Ubuntu Mono-14")

