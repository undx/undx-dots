;; Mode line setup
;; Extra mode line faces
;; (make-face 'mode-line-80col-face)
;; (make-face 'mode-line-filename-face)
;; (make-face 'mode-line-folder-face)
;; (make-face 'mode-line-minor-mode-face)
;; (make-face 'mode-line-mode-face)
;; (make-face 'mode-line-modified-face)
;; (make-face 'mode-line-position-face)
;; (make-face 'mode-line-process-face)
;; (make-face 'mode-line-read-only-face)
;; (set-face-attribute 'mode-line-inactive nil :foreground "gray80" :background "gray40" :inverse-video nil  :box '(:color "gray40" :style nil))
;; (set-face-attribute 'mode-line-read-only-face nil :inherit 'mode-line-face :weight 'bold :foreground "#990000")
;; (set-face-attribute 'mode-line-modified-face nil :inherit 'mode-line-read-only-face)
;; (set-face-attribute 'mode-line-folder-face nil :inherit 'mode-line-face :foreground "gray60")
;; (set-face-attribute 'mode-line-filename-face nil :inherit 'mode-line-face :foreground "#eab700" :weight 'bold) 
;; (set-face-attribute 'mode-line-position-face nil :inherit 'mode-line-face)
;; (set-face-attribute 'mode-line-mode-face nil :inherit 'mode-line-face :foreground "gray80") 
;; (set-face-attribute 'mode-line-minor-mode-face nil :inherit 'mode-line-mode-face :foreground "gray40"   ) 
;; (set-face-attribute 'mode-line-process-face nil :inherit 'mode-line-face :foreground "#718c00") 
;; (set-face-attribute 'mode-line-80col-face nil :inherit 'mode-line-position-face :foreground "black" :background "#eab700")

;; (setq-default mode-line-format
;;               '(; Position, including warning for 80 columns
;;                 (:propertize " %b" face mode-line-buffer-id)
;;                 ;; read-only or modified status
;;                 (:eval
;;                  (cond (buffer-read-only
;;                         (propertize " ⚑" 'face 'mode-line-inactive-face))
;;                        ((buffer-modified-p)
;;                         (propertize " ☡" 'face 'mode-line-modified-face))
;;                        (t "  ")))
;;                 (:eval
;;                  (propertize "%4l:%c"
;;                              'help-echo
;;                              (format "%d lines %d characters. Point=%d"
;;                                      (count-lines (point-min) (point-max))
;;                                      (point-max)
;;                                      (point))))

;;                 ;; narrow [default]
;;                 " %n "
;;                 (vc-mode vc-mode)
;;                 " %["
;;                 mode-line-mule-info
;;                 " "
;;                 (:propertize mode-name face mode-line-buffer-id)
;;                 "%] "
;;                 (:propertize mode-line-process face mode-line-process-face)
;;                 (global-mode-string global-mode-string)
;;                 mode-line-modes
;;                 (:eval (propertize "%n"
;;                                    'help-echo "mouse-2: Remove narrowing from the current buffer"
;;                                    'mouse-face 'mode-line-highlight
;;                                    'local-map (make-mode-line-mouse-map 'mouse-2 #'mode-line-widen)))
;;                 mode-line-misc-info
;;                 " %- "
;;                 ))
