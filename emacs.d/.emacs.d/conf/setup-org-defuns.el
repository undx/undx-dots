;;; setup-org-defuns.el ---  -*- lexical-binding: t; -*-

;;; Commentary: specific org-mode defuns
;;; Code:


;;
;; org-capture defuns
(defun org-find-ytb ()
  (org-datetree-find-date-create (calendar-current-date))
  (goto-char (point-at-eol))
  (when (not (re-search-forward
              (format org-complex-heading-regexp-format
                      (regexp-quote "YTB")) nil t))
    (insert "\n**** YTB\nY:\n- \nT:\nB:\n"))
  )

(defun org-find-log ()
  (org-datetree-find-date-create (calendar-current-date))
  (goto-char (point-at-eol))
  (when (not (re-search-forward
              (format org-complex-heading-regexp-format
                      (regexp-quote "LOG")) nil t))
    (insert "\n**** LOG\n"))
                                        ;(goto-char (point-max))
  )

(defun undx-org-find-or-create-headline ()
  ""
  (interactive)
  
  )

(defun undx-heading-candidates (level)
  (let (candidates)
    (org-map-entries
     (lambda ()
       (let ((comp (org-heading-components)))
         (if (= level (car comp))
             (push
              (nth 4 comp)
              candidates))
         )))
    (message "candidates: %s" candidates)
    (nreverse candidates)))

(defun undx-select-heading ()
  "Jump to a heading with completion."
  (interactive)
  (let ((cands (undx-heading-candidates 1)))
    (ivy-read "Heading: " cands)))


;; (defun bzg-find-location ()
;;   "Example: find my bzg.org file and the abcde string in the current buffer"
;;   (find-file "~/org/bzg.org")
;;   (goto-char (point-min))
;;   (re-search-forward "abcde" nil t)
;;   (newline 2))

;; (defun org-find-heading-in-datetree ()
;;   (org-datetree-find-date-create (calendar-current-date))
;;   (goto-char (point-at-eol))
;;   (when (not (re-search-forward
;;               (format org-complex-heading-regexp-format
;;                       (regexp-quote "Todo today")) nil t))
;;     (insert "\n**** Todo today\n"))
;;   (goto-char (point-max)))


;;----------------------------------------------------------------------
;;----------------------------------------------------------------------
;;----------------------------------------------------------------------
(defvar oc-capture-prmt-history nil
  "History of prompt answers for org capture.")
(defun oc/prmt (prompt variable)
  "PROMPT for string, save it to VARIABLE and insert it."
  (make-local-variable variable)
  (set variable (read-string (concat prompt ": ") nil oc-capture-prmt-history)))
(defun oc/inc (what text &rest fmtvars)
  "Ask user to include WHAT.  If user agrees return TEXT."
  (when (y-or-n-p (concat "Include " what "?"))
    (apply 'format text fmtvars)))
;; ----------------------------------------------------------------------
(defun oc/jira ()
  "Include Jira details in an org-capture template.

  This function will prompt for the Jira project and ticket number and return
  the string <project>-<number>.  The project and number are also available
  via the following custom capture expansions:

%@p - Jira Project
%@n - Jira Number
"
  (let ((jira-project (read-string "JIRA Project: " nil
                                   oc-capture-prmt-history))
        (jira-number (read-string "JIRA Ticket No: " nil
                                  oc-capture-prmt-history)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "%@\\([pn]\\)" nil t)
        (unless (org-capture-escaped-%)
          (let ((char (match-string-no-properties 1)))
            (replace-match (cond
                            ((equal char "p") jira-project)
                            ((equal char "n") jira-number)))))))
    (concat jira-project "-" jira-number)))
;;----------------------------------------------------------------------










(provide 'setup-org-defuns)
;;; setup-org-defuns.el ends here
