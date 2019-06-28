;;; setup-org.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;
;;
;;
(require 'org)
(require 'org-bullets)
;; "①" "②" "③ " "④" "⑤" "⑥" "⑦" "⑧" "⑨" "⑩" "⑪" "⑫" "⑬" "⑭" "⑮"
;;
;;
;; my specific org defuns
(require 'setup-org-defuns)
;;
;;
;; vars
(setq org-default-notes-file      (concat org-directory "notes.org"))
(defvar org-setup-dir             (concat org-directory "_setup/"))
(defvar org-capture-templates-dir (concat org-setup-dir "org-capture-templates/"))
(defvar org-jira-dir              (concat org-directory "work/talend/jira/"))
(defvar org-work-journal-file     (concat org-directory "work/talend/journal.org"))
(defvar org-work-howto-file       (concat org-directory "work/talend/howto.org"))
(defvar org-diary-file            (concat org-directory "personal/life.org"))
(defvar org-personal-buy-log-file (concat org-directory "personal/journal_achats.org"))
(defvar org-personal-stratif-file (concat org-directory "personal/stratif.org"))
(defvar org-personal-account-file (concat org-directory "personal/accounts/ledger.org"))
;;
(defvar org-export-output-directory-prefix "export_" "prefix of directory used for org-mode export")
;;
;;
;; Record time and note when a task is completed
(setq org-log-done 'time)
;; Record time and note when the scheduled date of a task is modified
(setq org-log-reschedule 'time)
;; Record time and note when the deadline of a task is modified
(setq org-log-redeadline 'time)
;; Record time and note when clocking out of a task
(setq org-log-clock-out 'time)
;; Record time and note when a task is refiled
(setq org-log-refile 'time)
;; Record note when clocking out
(setq org-log-note-clock-out t)
;; Log everything into the LOGBOOK drawer
(setq org-log-into-drawer t)
;; allow to use shift for selection
(setq org-support-shift-select 'always)
;; no annoying _ 
(setq org-use-sub-superscripts nil)
;;;; Show headings up to level 2 by default when opening an org files
(setq org-startup-folded 'content)
;; Show inline images by default
(setq org-startup-with-inline-images t)
;; Add more levels to headlines that get displayed with imenu
(setq org-imenu-depth 5)
;;
;;
;; visual stuff
(add-hook 'org-mode-hook (lambda ()
                           (org-bullets-mode 1)
                           (org-toggle-pretty-entities)))

(defun my/org-mode-hook ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(
                  org-document-info-keyword
                  org-document-info
                  org-document-title
                  org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5
                  ))
    (set-face-attribute face nil :height 1.0)))
(add-hook 'org-mode-hook 'my/org-mode-hook)
;;
;;
;;
;;
;; Define custom link abbreviations
(setq org-link-abbrev-alist
      '(;; files
        ("dev"            . "file:journal.org::*")
        ;; website
        ("JIRA" . "https://jira.talendforge.org/browse/%s") ;; Thus [[JIRA:TDI-38322]]
        ("WP"  .  "https://en.wikipedia.org/wiki/%s") ;; Thus [[WP:Toronto, Ontario]]
        ))

;;
;;
;; add properties to templates (<p<tab>)
(add-to-list 'org-structure-template-alist
             (list "p" (concat ":PROPERTIES:\n?\n:END:")))
;;
;; export configuration
;;
;;(require 'ox-md)
(setq org-export-backends '(html odt md))
(setq org-export-with-sub-superscripts nil)
(setq org-export-copy-to-kill-ring 'if-interactive);; Copy to kill ring when export is made
(setq org-export-with-section-numbers nil)

(defadvice org-export-output-file-name (before org-add-export-dir activate)
  "Modifies org-export to place exported files in a different directory"
  (when (not pub-dir)
    (setq pub-dir (concat org-export-output-directory-prefix (substring extension 1)))
    (when (not (file-directory-p pub-dir))
      (make-directory pub-dir))))
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;;; Agenda
;; week starts on monday
(setq org-agenda-start-on-weekday 1)
(setq calendar-week-start-day 1)
(setq calendar-latitude 48.2
      calendar-longitude 3.2833
      calendar-location-name "Sens, France")
;;
(setq org-agenda-files `(,org-work-journal-file
                         ,org-jira-dir
                         ,org-default-notes-file
                         ,org-diary-file))
;;
;;
;; workflow
;; #+TODO: TODO IN-PROGRESS WAITING | DONE CANCELED
;; "WAIT(w@/!):
;; "w" fast item selection
;; "@" means to add a note (with time),
;; "!" means to record only the time of the state change.
;; With X and Y being either "@" or "!",
;; "X/Y" means use X when entering the state, and use Y when leaving the state if and only if the *target* state does not define X.
;; You may omit any of the fast-selection key or X or /Y, so WAIT(w@), WAIT(w/@) and WAIT(@/@) are all valid.

(setq org-todo-keywords
      '(
        (sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")
        (sequence "TODO(t@/!)" "WAITING(w@/!)" "DELEGATED(e@/!)" "|" "ASSIGNED(.@/!)" "CANCELLED(x@/!)" "DONE(d@/!)")
        ))

;; capture
(require 'setup-org-capture)
;; documents managements
;;(require 'setup-deft)
(require 'setup-jira)
;; see - nil org-fu.el


;; Set default column view headings: Task Total-Time Time-Stamp
(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

;; (setq org-todo-keyword-faces
;;       (quote
;;        (("TODO" :foreground "medium blue" :weight bold)
;;         ("INPROGRESS" :foreground "dark orange" :weight bold)
;;         ("EPIC" :foreground "deep sky blue" :weight bold)
;;         ("STORY" :foreground "royal blue" :weight bold)
;;         ("RECUR" :foreground "cornflowerblue" :weight bold)
;;         ("APPT" :foreground "medium blue" :weight bold)
;;         ("NOTE" :foreground "brown" :weight bold)
;;         ("HOLD" :foreground "red" :weight bold)
;;         ("DELEGATED" :foreground "dark violet" :weight bold)
;;         ("VALIDATION" :foreground "dark blue" :weight bold)
;;         ("REJECTED" :foreground "dark blue" :weight bold)
;;         ("CODEREVIEW" :foreground "dark blue" :weight bold)
;;         ("PROJECT" :foreground "#088e8e" :weight bold))))
;;
;; (setq org-agenda-custom-commands 
;;       '(
;;         ("c" "Desk Work" tags-todo "computer" ;; (1) (2) (3) (4)
;;          ((org-agenda-files '("~/org/widgets.org" "~/org/clients.org")) ;; (5)
;;           (org-agenda-sorting-strategy '(priority-up effort-down))) ;; (5) cont.
;;          ("~/computer.html")) ;; (6)
;;         ;; ...other commands here
;;         ("P" "Projects" ((tags "PROJECT")))
;;         ("H" "Office and Home Lists"
;;          ((agenda)
;;           (tags-todo "OFFICE")
;;           (tags-todo "HOME")
;;           (tags-todo "COMPUTER")
;;           (tags-todo "DVD")
;;           (tags-todo "READING")))
;;         ("D" "Daily Action List"
;;          (
;;           (agenda "" ((org-agenda-ndays 1)
;;                       (org-agenda-sorting-strategy
;;                        (quote ((agenda time-up priority-down tag-up) )))
;;                       (org-deadline-warning-days 0)
;;                       ))))
;;         ))
;;
;; (use-package org-gcal
;;   :ensure t
;;   :config
;;   (setq org-gcal-up-days   90)
;;   (setq org-gcal-down-days 90)
;;   (setq org-gcal-file-alist `(("emmanuel.gallois@gmail.com" .  ,(concat org-directory "schedule.org"))
;;                               ("another-mail@gmail.com" .  "~/task.org"))))
;;(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
;;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
;;
;; (add-hook 'org-capture-mode-hook
;;           (lambda ()
;;             (save-restriction
;;               (widen)
;;               (setq-local org-tag-alist (org-get-buffer-tags)))))

;; (org-map-entries (lambda () (org-entry-get nil "WEIGHT")) "+WEIGHT>0")
;; (defun schedule-grocery-hook ()
;;   (if (string= (org-capture-get :description)
;;                "Grocery today")     ;Must match the description in the template
;;       (org-schedule 0 (format-time-string "%Y-%m-%d"))))
;; (add-hook 'org-capture-before-finalize-hook 'schedule-grocery-hook)
;; (defun add-property-with-date-captured ()
;;   "Add DATE_CAPTURED property to the current item."
;;   (interactive)
;;   (org-set-property "DATE_CAPTURED" (format-time-string "%F")))
;; (add-hook 'org-capture-before-finalize-hook 'add-property-with-date-captured)



;; (defun orfu-wash-link ()
;;   "Return a pretty-printed top of `org-stored-links'.
;; Try to remove superfluous information, like website title."
;;   (let ((link (caar org-stored-links))
;;         (title (cl-cadar org-stored-links)))
;;     (org-make-link-string
;;      link
;;      (replace-regexp-in-string " - Stack Overflow" "" title))))

;; (defvar orfu-link-hook nil)
;; (defun orfu-link-hooks ()
;;   (prog1
;;       (mapconcat #'funcall
;;                  orfu-link-hook
;;                  "\n")
;;     (setq orfu-link-hook nil)))

;; (defun orfu-raise-frame ()
;;   (if (eq system-type 'gnu/linux)
;;       (call-process
;;        "wmctrl" nil nil nil "-i" "-R"
;;        (frame-parameter (selected-frame) 'outer-window-id))
;;     (raise-frame)))

;; (defun orfu-handle-link ()
;;   (message "orfu-handle-link 000")
;;   (orfu-raise-frame)
;;   (message "orfu-handle-link 001")
;;   (let ((link (caar org-stored-links))
;;         (title (cadr (car org-stored-links)))
;;         file)
;;     (message "orfu-handle-link 002")
;;     (message link)
;;     (cond ((string-match "^https://www.youtube.com/" link)
;;            (orfu-handle-link-youtube link title))
;;           ((string-match "^https://scholar.google.com/scholar.bib" link)
;;            (url-retrieve
;;             link
;;             (lambda (status)
;;               (let ((err (plist-get status :error)))
;;                 (if err (error
;;                          "\"%s\" %s" link
;;                          (downcase (nth 2 (assq (nth 2 err) url-http-codes)))))
;;                 (message (buffer-substring-no-properties
;;                           (point-min)
;;                           (point-max)))))
;;             nil nil t))
;;           ((string-match (regexp-quote "http://stackoverflow.com/") link)
;;            (find-file (concat org-directory "stack.org"))
;;            (goto-char (point-min))
;;            (re-search-forward "^\\*+ +Questions" nil t))
;;           ;; ((string-match orfu-github-project-name link)
;;           ;;  (let ((project-name (match-string 1 link))
;;           ;;        (parts (split-string title "·")))
;;           ;;    (setf (cl-cadar org-stored-links)
;;           ;;          (concat (car parts)
;;           ;;                  (substring (cadr parts) 7)))
;;           ;;    (find-file (concat org-directory "github.org"))
;;           ;;    (goto-char (point-min))
;;           ;;    (re-search-forward (concat "^\\*+ +" project-name) nil t)))
;;           (t
;;            (message "orfu-handle-link 003")
;;            (find-file (concat org-directory "ent.org"))
;;            (goto-char (point-min))
;;            (re-search-forward "^\\*+ +Articles" nil t)))))

;; (require 'async)
;; (defun orfu-handle-link-youtube (link title)
;;   (let* ((file-name (concat title ".mp4"))
;;          (dir undx-downloads-di)r
;;          (full-name
;;           (expand-file-name file-name dir)))
;;     (add-hook 'orfu-link-hook
;;               `(lambda ()
;;                  ,(concat
;;                    (org-make-link-string dir dir)
;;                    "\n"
;;                    (org-make-link-string full-name file-name))))
;;     (let* ((max-id 0)
;;            id
;;            (max-id
;;             (progn
;;               (dolist (b (buffer-list))
;;                 (when (string-match "\\`\\*youtube-dl \\([0-9]+\\)\\*\\'" (buffer-name b))
;;                   (setq id (string-to-number (match-string 1 (buffer-name b))))
;;                   (setq max-id (max id max-id))))
;;               max-id))
;;            (output-buffer (get-buffer-create
;;                            (format "*youtube-dl %d*" (1+ max-id)))))
;;       (async-shell-command
;;        (format "cd %s && youtube-dl -f mp4 \"%s\" -o %s" dir link
;;                (shell-quote-argument file-name))
;;        output-buffer))
;;     (find-file (concat org-directory "ent.org"))
;;     (goto-char (point-min))
;;     (re-search-forward "^\\*+ +Videos" nil t)))


(require 'setup-org-babel)


(defun org-agenda-cts ()
  (and (eq major-mode 'org-agenda-mode)
       (let ((args (get-text-property
                    (min (1- (point-max)) (point))
                    'org-last-args)))
         (nth 2 args))))

(defhydra hydra-org-agenda-view (:hint none)
  "
_d_: ?d? day        _g_: time grid=?g?  _a_: arch-trees
_w_: ?w? week       _[_: inactive       _A_: arch-files
_t_: ?t? fortnight  _f_: follow=?f?     _r_: clock report=?r?
_m_: ?m? month      _e_: entry text=?e? _D_: include diary=?D?
_y_: ?y? year       _q_: quit           _L__l__c_: log = ?l?"
  ("SPC" org-agenda-reset-view)
  ("d" org-agenda-day-view (if (eq 'day (org-agenda-cts)) "[x]" "[ ]"))
  ("w" org-agenda-week-view (if (eq 'week (org-agenda-cts)) "[x]" "[ ]"))
  ("t" org-agenda-fortnight-view (if (eq 'fortnight (org-agenda-cts)) "[x]" "[ ]"))
  ("m" org-agenda-month-view (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
  ("y" org-agenda-year-view (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
  ("l" org-agenda-log-mode (format "% -3S" org-agenda-show-log))
  ("L" (org-agenda-log-mode '(4)))
  ("c" (org-agenda-log-mode 'clockcheck))
  ("f" org-agenda-follow-mode (format "% -3S" org-agenda-follow-mode))
  ("a" org-agenda-archives-mode)
  ("A" (org-agenda-archives-mode 'files))
  ("r" org-agenda-clockreport-mode (format "% -3S" org-agenda-clockreport-mode))
  ("e" org-agenda-entry-text-mode (format "% -3S" org-agenda-entry-text-mode))
  ("g" org-agenda-toggle-time-grid (format "% -3S" org-agenda-use-time-grid))
  ("D" org-agenda-toggle-diary (format "% -3S" org-agenda-include-diary))
  ("!" org-agenda-toggle-deadlines)
  ("[" (let ((org-agenda-include-inactive-timestamps t))
         (org-agenda-check-type t 'timeline 'agenda)
         (org-agenda-redo)
         (message "Display now includes inactive timestamps as well")))
  ("q" (message "Abort") :exit t)
  ("v" nil))


;; Recommended binding:
;;(define-key org-agenda-mode-map "v" 'hydra-org-agenda-view/body)

(provide 'setup-org)
;;; setup-org.el ends here
