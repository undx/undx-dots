;;
;;
;;
(defvar undx-conf-dir         (concat user-emacs-directory "conf/"))
(defvar undx-local-dir        (concat user-emacs-directory "local/"))
(defvar undx-private-dir      (concat user-emacs-directory "private/"))
(defvar undx-lib-dir          (concat user-emacs-directory "lib/"))
(defvar undx-system-conf-file (concat undx-conf-dir (system-name) "-local-init.el"))
(defvar undx-custom-file      (concat undx-conf-dir (system-name) "-custom.el"))
(defvar undx-user-init-common (concat undx-private-dir            "common-user-init.el"))
(defvar undx-user-init-file   (concat undx-private-dir (system-name) "-user-init.el"))
;;
;;
;; default home dir - override if needed in undx-system-conf-file
(defvar undx-home-dir (concat (getenv "HOME") "/"))
;; keep machine settings in their own file
;; also sets where temporary files and org directory will be located.
;; needed to load now for persistence location.
(load-file undx-system-conf-file)
(defvar undx-tmp-dir          (concat undx-home-dir "tmp/emacs/"))
(defvar undx-persistence-dir  (concat undx-tmp-dir  "persistence/"))
(defvar undx-backups-dir      (concat undx-tmp-dir  "backups/"))
;;
;;
;; for other packages that need it...
(setq temporary-file-directory undx-tmp-dir)
;; private user informations on this system
(load undx-user-init-common)
(load undx-user-init-file)
;;
;; packages location
(setq package-user-dir (concat undx-tmp-dir "/elpa/"))
;;
;;
;; create dir if needed
(dolist (d `(
             ,undx-conf-dir
             ,undx-local-dir
             ,undx-private-dir
             ,undx-tmp-dir
             ,undx-backups-dir
             ,undx-persistence-dir
             ))
  (message d)
  (unless (file-exists-p d)
    (mkdir d))
  )

;;
;;
;; private user informations on this system
(load undx-user-init-file)
;;
;;
;; add local libs path
(add-to-list 'load-path undx-lib-dir)
;;
;; keep customize settings in their own file
;;
(setq custom-file undx-custom-file)
;;
;;
;;
;;
;;
;;
(provide 'setup-paths)
