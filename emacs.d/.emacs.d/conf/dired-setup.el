(require 'dired-x)
(require 'dired-details)
(require 'dired-details+)
;;
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies  'always)
;; try to guess dwim
(setq dired-dwim-target t)
;; move to trash
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/")
;; details
(setq dired-details-hide-link-targets nil)
;; omit files
(setq-default dired-omit-mode t)
(setq-default dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")
;(add-to-list 'dired-omit-extension ".example")
;(delete 'dired-omit-extension ".example")

;;  brew install coreutils findutils
(require 'ls-lisp)
;(setq ls-lisp-use-insert-directory-program t)
;(setq insert-directory-program "/usr/local/bin/gls")
;;(setq dired-listing-switches "-alh")
;(require 'dired-sort-map)
(setq dired-listing-switches "-Al --si --time-style long-iso")
;(setq dired-listing-switches "--group-directories-first -alh")




;;;;

(defun tmtxt/dired-do-shell-mac-open ()
	(interactive)
	(save-window-excursion
	  (let ((files (dired-get-marked-files nil current-prefix-arg))
			command)
		;; the open command
		(setq command "open ")
		(dolist (file files)
		  (setq command (concat command (shell-quote-argument file) " ")))
		(message command)
		;; execute the command
		(async-shell-command command))))

(defun tmtxt/dired-open-current-directory-in-finder ()
	"Open the current directory in Finder"
	(interactive)
	(save-window-excursion
	  (dired-do-async-shell-command
	   "open .")))

(defun dired-do-shell-unmount-device ()
	(interactive)
	(save-window-excursion
	  (dired-do-async-shell-command
	   "diskutil unmount" current-prefix-arg
	   (dired-get-marked-files t current-prefix-arg))))

;; default terminal application path
(defvar tmtxt/macos-default-terminal-app-path
	"/Applications/Terminal.app" "The default path to terminal application in MacOS")
;;; function to open new terminal window at current directory
  (defun tmtxt/open-current-dir-in-terminal ()
	"Open current directory in dired mode in terminal application.
For MacOS only"
	(interactive)
	(shell-command (concat "open -a "
						   (shell-quote-argument tmtxt/macos-default-terminal-app-path)
						   " "
						   (shell-quote-argument (file-truename default-directory)))))

(setq-default tmtxt/macos-default-terminal-app-path "/Volumes/tmtxt/Applications/iTerm.app")


(defun dired-get-size ()
(interactive)
(let ((files (dired-get-marked-files)))
	(with-temp-buffer
		(apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
		(message "Size of all marked files: %s"
						 (progn
							 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
							 (match-string 1))))))

;;;_. File-size info
(defun my-dired-size-of-file ()
	"Print size of file under point, or a list of results for marked files."
	(interactive)
	(let* ((marked-files (dired-get-marked-files))
				 (parent (f-common-parent marked-files))
				 (marked-files (--map (s-chop-prefix parent it) marked-files)))
		(if (cdr marked-files)
				(dired-do-shell-command "du --apparent-size -s -h -c * &" nil marked-files)
			(dired-do-shell-command "du --apparent-size -s -h" nil marked-files))))


;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------

;; redefine this function, to fix the formatting of file sizes in dired mode
(defun ls-lisp-format-file-size (file-size &optional human-readable level)
	(setq level (or level 1000))
	(if (or (not human-readable)
					(< file-size 1024))
			(format (if (floatp file-size) " %11.0f" " %11d") file-size)
		(do ((file-size (/ file-size 1024.0) (/ file-size 1024.0))
				 ;; kilo, mega, giga, tera, peta, exa
				 (post-fixes (list "k" "M" "G" "T" "P" "E") (cdr post-fixes))
				 (l level (1- l)))
				((or (= 0 l)
						 (< file-size 1024)) (format " %10.0f%s" file-size (car post-fixes))))))
(defvar dired-sort-modes-list
	'(("size" "S" "")
		("ext" "X" "S")
		("cdate" "ct" "X")
		("date" "t" "ct")
		("name" "" "t"))
	"List of dired buffer sort modes.")
(defvar dired-sort-current-mode ""
	"Current mode for sorting dired buffer.")
;; redefining from dired.el. Just cycle the options
(defun dired-sort-toggle ()
	(cond
	 ((equal dired-sort-current-mode "") (setq dired-sort-current-mode "S") (dired-sort-size))
	 ((equal dired-sort-current-mode "S") (setq dired-sort-current-mode "X") (dired-sort-extension))
	 ((equal dired-sort-current-mode "X") (setq dired-sort-current-mode "ct") (dired-sort-ctime))
	 ((equal dired-sort-current-mode "ct") (setq dired-sort-current-mode "t") (dired-sort-time))
	 ((equal dired-sort-current-mode "t") (setq dired-sort-current-mode "") (dired-sort-name))))
;; redefining from dired.el. With double-prefix show a menu to chose the sorting from
(defun dired-sort-toggle-or-edit (&optional arg)
	"Toggle sorting by date, and refresh the Dired buffer.
With a prefix argument \\[universal-argument], edit the current listing switches instead.
With a prefix argument \\[universal-argument] \\[universal-argument] prompt user with list of choices
to chose from."
	(interactive "P")
	(when dired-sort-inhibit
		(error "Cannot sort this dired buffer"))
	(cond
	 ((equal arg '(4))
		(dired-sort-other
		 (read-string "ls switches (must contain -l): " dired-actual-switches)))
	 ((equal arg '(16))
		(let* ((sort-mode (completing-read "Sort by: "
																			 (mapcar 'car dired-sort-modes-list)
																			 nil
																			 t))
					 (sort-switch (caddr (assoc sort-mode dired-sort-modes-list))))
			(setq dired-sort-current-mode sort-switch)
			(dired-sort-toggle)))
	 (t (dired-sort-toggle))))
(defun dired-sort-size ()
	"Dired sort by size."
	(interactive)
	(dired-sort-other (concat dired-listing-switches "S")))
(defun dired-sort-extension ()
	"Dired sort by extension."
	(interactive)
	(dired-sort-other (concat dired-listing-switches "X")))
(defun dired-sort-ctime ()
	"Dired sort by create time."
	(interactive)
	(dired-sort-other (concat dired-listing-switches "ct")))
(defun dired-sort-time ()
	"Dired sort by time."
	(interactive)
	(dired-sort-other (concat dired-listing-switches "t")))
(defun dired-sort-name ()
	"Dired sort by name."
	(interactive)
	(dired-sort-other (concat dired-listing-switches "")))
;; redefined from dired.el to support new types of sorting
(defun dired-sort-set-modeline ()
	(when (eq major-mode 'dired-mode)
		(setq mode-name
					(let (case-fold-search)
						(cond
						 ((string-match "ct" dired-actual-switches)
							"Dired by ctime")
						 ((string-match "^-[^t]*t[^t]*$" dired-actual-switches)
							"Dired by date")
						 ((string-match "^-[^X]*X[^X]*$" dired-actual-switches)
							"Dired by ext")
						 ((string-match "^-[^S]*S[^S]*$" dired-actual-switches)
							"Dired by size")
						 ((string-match "^-[^SXUt]*$" dired-actual-switches)
							"Dired by name")
						 (t
							(concat "Dired " dired-actual-switches)))))
		(force-mode-line-update)))
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------



;; Wdired : writeable dired
;; C-x C-q dired-toggle-read-only
;; C-c C-c wdired-finish-edit
;; C-c C-k wdired-abort-changes 


;;
(require 'dired-rainbow)
(defconst dired-audio-files-extensions
  '("mp3" "MP3" "m4a" "M4A" "ogg" "OGG" "flac" "FLAC" "wav" "WAV")
  "Dired Audio files extensions")
(defconst dired-video-files-extensions
    '("vob" "VOB" "mkv" "MKV" "mpe" "mpg" "MPG" "mp4" "MP4" "ts" "TS" "m2ts"
      "M2TS" "avi" "AVI" "mov" "MOV" "wmv" "asf" "m2v" "m4v" "mpeg" "MPEG" "tp")
    "Dired Video files extensions")
(dired-rainbow-define audio "#329EE8" dired-audio-files-extensions)
(dired-rainbow-define video "#B3CCFF" dired-video-files-extensions)
(dired-rainbow-define html  "#4E9A06" ("htm" "html" "xhtml"))
; boring regexp due to lack of imagination
(dired-rainbow-define log (:inherit default :italic t) ".*\\.log")
; highlight executable files, but not directories
(dired-rainbow-define-chmod executable-unix "Green" "-.*x.*")
(dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
(dired-rainbow-define xml "DarkGreen" ("xml" "xsd" "xsl" "xslt" "wsdl"))
(dired-rainbow-define document "#fce94f" ("doc" "docx" "odt" "pdb" "pdf" "ps" "rtf" "djvu"))
(dired-rainbow-define image "#ff4b4b" ("jpg" "png" "jpeg" "gif"))
(dired-rainbow-define log "#c17d11" ("log"))
(dired-rainbow-define sourcefile "#fcaf3e" ("py" "c" "cc" "clj" "el" "h" "java" "pl" "rb"))
(dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
(dired-rainbow-define compressed "#ad7fa8" ("zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
(dired-rainbow-define packaged "#e6a8df" ("deb" "rpm"))
(dired-rainbow-define encrypted "DarkRed" ("gpg" "pgp"))
;;
(put 'dired-find-alternate-file 'disabled nil)
;; C-c C-f follow-mode
;(image-dired-display-image-mode)

;; keybindings
(define-key dired-mode-map (kbd "RET")   'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "^")     (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory
(define-key dired-mode-map (kbd "C-c o") 'dired-omit-mode)
(define-key dired-mode-map (kbd "s-O")   'tmtxt/dired-open-current-directory-in-finder)
(define-key dired-mode-map (kbd "s-o")   'tmtxt/dired-do-shell-mac-open)
(define-key dired-mode-map (kbd "s-u")   'dired-do-shell-unmount-device)
(define-key dired-mode-map (kbd "C-c C-o") 'tmtxt/open-current-dir-in-terminal)
(define-key dired-mode-map (kbd "?") 'dired-get-size)
(define-key dired-mode-map (kbd "?") 'my-dired-size-of-file)

(provide 'dired-setup)
