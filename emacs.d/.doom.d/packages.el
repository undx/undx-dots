;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.
;;
;; WARNING: Disabling core packages listed in ~/.emacs.d/core/packages.el may
;; have nasty side-effects and is not recommended.


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
;(unpin! t)

;; ...but to unpin a single package:
;(unpin! pinned-package)
;; Use it to unpin multiple packages
;(unpin! pinned-package another-pinned-package)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;;; Code:


;;; UI
(package! material-theme)
(package! modus-operandi-theme)
(package! modus-vivendi-theme)
;;;; window
(package! dimmer)
(package! beacon)
;;;; helpers
(package! symbol-overlay)
(package! idle-highlight-mode)          ; needed? check if symbol-overlay is ok to remove
(package! rainbow-delimiters)
(package! rainbow-mode)
;;; buffers
(package! persistent-scratch)
;;; bookmarks
(package! bookmark+)
;;; dired and others
(package! ranger)
;;; editing
(package! expand-region)
(package! drag-stuff)
(package! duplicate-thing)
(package! copy-as-format)
;;;; snippets
(package! auto-yasnippet)
;;; Elisp
;; if any error with it, remember to call M-x bug-hunter-init-file e
(setq debug-on-error t)
(setq debug-on-quit t)
;; using bug-hunter for troublesshooting init file:
;; M-x bug-hunter-init-file RET e
;; M-x bug-hunter-init-file RET i ;; interactive mode
;; M-x bug-hunter-init-file RET a (featurep 'cl) RET ;; assertion hunt
(package! bug-hunter)
(package! outshine)
(package! aggressive-indent)

;;; org
;;;; https://github.com/the-humanities/org-mind-map

;;; web tools
;;;; REST client
(package! restclient)
(package! company-restclient)
(package! ob-restclient)
;;; system tools
(package! docker)
;;; misc
(package! gif-screencast
  :recipe (:host gitlab :repo "ambrevar/emacs-gif-screencast"))
;;
(provide 'packages)
;;; packages.el ends here
