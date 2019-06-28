(message "Loading local configuration file for system %s" (system-name))
;; temporary storage
(let ((tmp  "~/tmp/emacs"))
  (prin1 tmp)
  (unless (file-exists-p tmp)
    (make-directory tmp t)))
(setq temporary-file-directory "~/tmp/emacs/")
;; packages location
(setq package-user-dir (concat temporary-file-directory "/elpa/"))
;; default font. To select font : (insert (prin1-to-string (w32-select-font)))
;;(set-face-attribute 'default nil :font "Ubuntu Mono-13")
(set-face-attribute 'default nil :font "Consolas-12")
;;(set-face-attribute 'default nil :font "Roboto Mono-11")
;;(set-face-attribute 'variable-pitch nil :family "Ubuntu" :width 'normal :height 160 :weight 'normal :slant 'normal)
;; specific execution paths
(add-to-list 'exec-path "C:/Tools/cmder/vendor/git-for-windows/bin")
(add-to-list 'exec-path "C:/Tools/cmder/vendor/git-for-windows/usr/bin")
(add-to-list 'exec-path "C:/Tools/gnu/bin")
;;
;; org-directory
(setq org-directory "~/OneDrive/Org/")


