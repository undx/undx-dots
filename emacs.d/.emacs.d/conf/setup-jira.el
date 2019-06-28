;;; setup-jira.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:


;; JIRA related
(require 'org-jira)
(require 'ox-jira)
;;
(defconst talend-jira-url "https://jira.talendforge.org" "Talend JIRA URL")
(defvar talend-user-login-name "emmanuel_g")
(setq jiralib-user-login-name  talend-user-login-name)
(setq jiralib2-user-login-name talend-user-login-name)
(setq jiralib-url talend-jira-url)
(setq jiralib2-url talend-jira-url)
(setq org-jira-working-dir org-jira-dir)
;;
;; to get customfield : https://jira.talendforge.org/rest/api/2/field
;;
;; (use-package ejira :load-path "/home/undx/.spacemacs.d/lib/ejira" :ensure    nil)

;;
;; to get customfield : https://jira.talendforge.org/rest/api/2/field
;;

;; (use-package ejira :load-path "/home/undx/.spacemacs.d/lib/ejira" :ensure    nil)

(setq ejira-projects           '("TDI" )
      ejira-main-project       "TDI"
      ejira-my-org-directory   org-jira-working-dir
      ejira-done-states        '("Done")
      ejira-in-progress-states '("In Progress" "In Review" "Testing")
      ejira-high-priorities    '("High" "Highest")
      ejira-low-priorities     '("Low" "Lowest")
      ;; Customize these based on your JIRA server configuration
      ejira-sprint-field                     'customfield_11070
      ejira-epic-field                       'customfield_11071
      ejira-epic-summary-field               'customfield_11074
      )
(use-package ejira
  :load-path  "~/.emacs.d/lib/ejira"
  :ensure    nil)
(require 'ejira)
(require 'org-agenda)
(org-add-agenda-custom-command ejira-sprint-agenda)

(major-mode-hydra-bind org-jira-mode 
    (pretty-hydra-define hydra-jira (:exit t :hint nil :title "JIRA")
      ("Get" (("p" org-jira-get-projects                "Get Projects")
              ("g" org-jira-get-issues                  "Get Issues")
              ("G" org-jira-get-subtasks                "Get Subtasks")
              ("r" org-jira-refresh-issue               "Refresh Issue")
              ("R" org-jira-refresh-issues-in-buffer    "Refresh Issues in Buffer"))

       "Manage" (("b" org-jira-browse-issue             "Browse Issue")
                 ("c" org-jira-create-issue             "Create Issue")
                 ("s" org-jira-create-subtask           "Create Subtask")
                 ("P" org-jira-progress-issue           "Update Issue Progress")
                 ("a" org-jira-assign-issue             "Assign Issue"))

       "Push" (("u" org-jira-update-issue                "Update Issue")
               ("y" org-jira-copy-current-issue-key      "Copy Current Issue Key")
               ("U" org-jira-update-comment              "Update Comment")
               ("t" org-jira-todo-to-jira                "Todo to Jira"))))
  )

(provide 'setup-jira)
;;; setup-jira.el ends here
