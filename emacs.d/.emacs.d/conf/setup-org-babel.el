;;; setup-org-babel.el ---  -*- lexical-binding: t; -*-

;;; Commentary: Org Babel configuration
;;; Code:

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ruby . t)
   (clojure . t)
   (shell . t)
   (python . t)
   (dot . t)
   (emacs-lisp . t)
   (latex . t)
   (C . t)
   (java . t)
   (scheme . t)
   (lisp . t)
   (sql . t)
   (calc . t)))
;;(require 'htmlfontify)
;; fontify code in code blocks
(setq org-src-fontify-natively t)
(setq org-src-preserve-indentation nil)
(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)
(setq org-confirm-babel-evaluate nil)

(setq org-babel-default-header-args '((:session . "none")
                                      (:results . "replace")
                                      (:exports . "code")
                                      (:cache . "no")
                                      (:noweb . "no")
                                      (:hlines . "no")
                                      (:tangle . "no"))
      org-babel-default-header-args:python '((:results . "output verbatim raw replace")
                                             (:exports . "both")
                                             (:wrap . "EXAMPLE"))
      org-babel-default-header-args:ruby '((:results . "output verbatim raw replace")
                                           (:exports . "both")
                                           (:wrap . "EXAMPLE"))
      org-babel-default-header-args:sh '((:results . "output verbatim raw replace")
                                         (:exports . "both")
                                         (:wrap . "EXAMPLE"))
      org-babel-default-header-args:emacs-lisp '((:results . "output verbatim raw replace")
                                                 (:exports . "both")
                                                 (:wrap . "EXAMPLE"))
      org-babel-default-header-args:clojure '((:results . "output verbatim raw replace")
                                              (:exports . "both")
                                              (:wrap . "EXAMPLE"))
      )

(setq org-ditaa-jar-path "torax-ditaa-jar"
      org-plantuml-jar-path "storax-plantuml-jar"
      )

(provide 'setup-org-babel)
;;; setup-org-babel.el ends here
