(setq
 undx-personal-dic   (concat user-emacs-directory "dics/personal.dic")
 undx-dic-american   (concat user-emacs-directory "dics/en_US")
 undx-dic-francais   (concat user-emacs-directory "dics/fr_FR")
 undx-dic-portuguese (concat user-emacs-directory "dics/pt_PT"))

(require 'ispell)
(require 'flyspell)
(diminish 'flyspell-mode " ✈");;(string 32 #x2708)))
(setq
 ispell-program-name (locate-file "hunspell" exec-path exec-suffixes 'file-executable-p)
 ispell-dictionary "american";; default dic for all buffers
 ispell-local-dictionary-alist
 '(
   ("francais"
    "[[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]"
    "[^[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]"
    "[-']"
    t
    ("-d" "c:/Users/undx/.emacs.d/dics/fr_FR" "-p" "~/.emacs.d/dics/personal.dic")
    nil
    utf-8)
   ("american"
    "[[:alpha:]]"
    "[^[:alpha:]]"
    "[']"
    t
    ("-d" "~/.emacs.d/dics/en_US" "-p" "~/.emacs.d/dics/personal.dic")
    nil
    utf-8)
   ("portugese"
    "[a-zàáâãçéêíóôõúüA-ZÀÁÂÃÇÉÊÍÓÔÕÚÜ]"
    "[^a-zàáâãçéêíóôõúüA-ZÀÁÂÃÇÉÊÍÓÔÕÚÜ]"
    ""
    nil
    ( "-d" "~/.emacs.d/dics/pt_PT.aff" "-p" "~/.emacs.d/dics/personal.dic")
    nil
    utf-8)
   ))
(defun spell-FR ()
  "ispell change dictionary to French."
  (interactive)
  (ispell-change-dictionary "francais")
  (diminish 'flyspell-mode " ✈F"))
(defun spell-US ()
  "ispell change dictionary to American."
  (interactive)
  (ispell-change-dictionary "american")
  (diminish 'flyspell-mode " ✈E"))
(defun spell-PT ()
  "ispell change dictionary to Portugese."
  (interactive)
  (ispell-change-dictionary "portugese")
  (diminish 'flyspell-mode " ✈P"))
