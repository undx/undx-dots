;;
;;
;;
(require 'org)
;;(use-package org-plus-contrib :ensure t :defer t)
(use-package htmlize :ensure t)
(use-package org-bullets :ensure t)
(setq org-log-into-drawer t)
(setq org-log-done (quote time)
      org-log-redeadline (quote time)
      org-log-reschedule (quote time)
      org-log-refile (quote time))
(setq org-support-shift-select 'always)
(setq htmlize-html-charset "utf-8")
(setq org-use-sub-superscripts nil)
(setq org-agenda-default-appointment-duration 60)

;;
;; visual stuff
;;
(add-hook 'org-mode-hook (lambda ()
                           (org-bullets-mode 1)
                           (org-toggle-pretty-entities)))
(eval-after-load 'org-bullets
  '(setq org-bullets-bullet-list '("✺" "✹" "✸" "✷" "✶" "✭" "✦" "■" "▲" "●" )))

(setq org-fontify-done-headline t)
;; checkbox done too
(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-headline-done prepend))
 'append)


(setq org-todo-keyword-faces
      (quote
       (("TODO" :foreground "medium blue" :weight bold)
        ("INPROGRESS" :foreground "dark orange" :weight bold)
        ("EPIC" :foreground "deep sky blue" :weight bold)
        ("STORY" :foreground "royal blue" :weight bold)
        ("RECUR" :foreground "cornflowerblue" :weight bold)
        ("APPT" :foreground "medium blue" :weight bold)
        ("NOTE" :foreground "brown" :weight bold)
        ("HOLD" :foreground "red" :weight bold)
        ("DELEGATED" :foreground "dark violet" :weight bold)
        ("VALIDATION" :foreground "dark blue" :weight bold)
        ("REJECTED" :foreground "dark blue" :weight bold)
        ("CODEREVIEW" :foreground "dark blue" :weight bold)
        ("PROJECT" :foreground "#088e8e" :weight bold))))
;;
;;
;;
(defvar org-setup-dir             (concat org-directory "_setup/"))
(defvar org-capture-templates-dir (concat org-setup-dir "org-capture-templates/"))
(setq   org-default-notes-file    (concat org-directory "notes.org"))
(defvar org-jira-dir              (concat org-directory "work/talend/jira/"))
(defvar org-work-journal-file     (concat org-directory "work/talend/journal.org"))
(defvar org-diary-file            (concat org-directory "personal/life.org"))
;;
(defvar org-export-output-directory-prefix "export_" "prefix of directory used for org-mode export")
;;
;;
;;
;; Define custom link abbreviations
(setq org-link-abbrev-alist
      '(
        ("JIRA" . "https://jira.talendforge.org/browse/%s") ;; Thus [[JIRA:TDI-38322]]
        ("WP"  .  "https://en.wikipedia.org/wiki/%s") ;; Thus [[WP:Toronto, Ontario]]
        ))
(org-add-link-type "grep" 'endless/follow-grep-link)
(defun endless/follow-grep-link (regexp)
  "Run `rgrep' with REGEXP as argument."
  (grep-compute-defaults)
  (rgrep regexp "*" (expand-file-name "./")))
;;
;; export configuration
;;
(require 'ox-md)
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
;;
;; agenda
;;
(setq org-agenda-files `(,org-work-journal-file
                         ,org-default-notes-file
                         ,org-diary-file)
      )


(setq org-agenda-custom-commands 
      '(
        ("c" "Desk Work" tags-todo "computer" ;; (1) (2) (3) (4)
         ((org-agenda-files '("~/org/widgets.org" "~/org/clients.org")) ;; (5)
          (org-agenda-sorting-strategy '(priority-up effort-down))) ;; (5) cont.
         ("~/computer.html")) ;; (6)
        ;; ...other commands here
        
        ("P" "Projects" ((tags "PROJECT")))

        ("H" "Office and Home Lists"
         ((agenda)
          (tags-todo "OFFICE")
          (tags-todo "HOME")
          (tags-todo "COMPUTER")
          (tags-todo "DVD")
          (tags-todo "READING")))

        ("D" "Daily Action List"
         (
          (agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up) )))
                      (org-deadline-warning-days 0)
                      ))))
        ))

;;
;;* Babel
;;
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
(require 'htmlfontify)
(setq org-src-fontify-natively t)
(setq org-src-preserve-indentation nil)
(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)
(setq org-confirm-babel-evaluate nil)

;;
;; workflow
;; #+TODO: TODO IN-PROGRESS WAITING | DONE CANCELED
;;
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))


(use-package org-gcal :ensure t
  :config
  (setq org-gcal-up-days   90)
  (setq org-gcal-down-days 90)
  (setq org-gcal-file-alist `(("emmanuel.gallois@gmail.com" .  ,(concat org-directory "schedule.org"))
                              ("another-mail@gmail.com" .  "~/task.org"))))

;;(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
;;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
;;

;; Capture bookmarklet or function: 
;; location.href='org-protocol:///capture?template=c&url='+ encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)+'&body='+ encodeURIComponent(window.getSelection());
;; %:link	URL of the web-page
;; %:description	The title of the web-page
;; %:initial	Selected text.
;;
(require 'org-capture)
(require 'org-protocol)
(require 'org-protocol-capture-html)

;;
;; Template expansions
;;
;; %U Inactive timestamp
;; %ˆ{Name} Prompt for something
;; %a Annotation (org-store-link)
;; %i Active region
;; %? Cursor ends up here
;; %:link URL of the web-page
;; %:description The title of the web-page
;; %:initial Selected text.

;; %[file]     Insert the contents of the file given by file.
;; %(sexp)     Evaluate Elisp sexp and replace with the result.
;; For convenience, %:keyword (see below) placeholders
;; within the expression will be expanded prior to this.
;; The sexp must return a string.
;; %<...>      The result of format-time-string on the ... format specification.
;; %t          Timestamp, date only.
;; %T          Timestamp, with date and time.
;; %u, %U      Like the above, but inactive timestamps.
;; %i          Initial content, the region when capture is called while the
;; region is active.
;; The entire text will be indented like %i itself.
;; %a          Annotation, normally the link created with org-store-link.
;; %A          Like %a, but prompt for the description part.
;; %l          Like %a, but only insert the literal link.
;; %c          Current kill ring head.
;; %x          Content of the X clipboard.
;; %k          Title of the currently clocked task.
;; %K          Link to the currently clocked task.
;; %n          User name (taken from user-full-name).
;; %f          File visited by current buffer when org-capture was called.
;; %F          Full path of the file or directory visited by current buffer.
;; %:keyword   Specific information for certain link types, see below.
;; %^g         Prompt for tags, with completion on tags in target file.
;; %^G         Prompt for tags, with completion all tags in all agenda files.
;; %^t         Like %t, but prompt for date.  Similarly %^T, %^u, %^U.
;; You may define a prompt like %^{Birthday}t.
;; %^C         Interactive selection of which kill or clip to use.
;; %^L         Like %^C, but insert as link.
;; %^{prop}p   Prompt the user for a value for property prop.
;; %^{prompt}  prompt the user for a string and replace this sequence with it.
;; You may specify a default value and a completion table with
;; %^{prompt|default|completion2|completion3...}.
;; The arrow keys access a prompt-specific history.
;; %\1 ... %\N Insert the text entered at the Nth %^{prompt}, where N is
;; a number, starting from 1.2
;; %?          After completing the template, position cursor here.


;; Select-from-list prompts
;;    %^{Tidbit type|quote|zinger|one-liner|textlet}

(setq org-capture-templates
      `(("c"         "Capture"
         entry (file+headline ,(concat org-directory "notes.org") "Inbox")
         "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n LINK: %:link\nDESC: %:description\nINITIAL: %:initial\n\n%?")
        ("a" "Appointment" entry (file org-agenda-file-gcal) (file ,(concat org-capture-templates-dir "appointment.org")))
        ("x" "Activity")
        ("xm" "Activity (math)"     entry (file+headline "~/activity.org" "Log") "Act: math\nTopic: %^{Topic}\nDate: %u\nStart time: %^{Time}\nDuration: %^{Duration}\nUnit: mins" :immediate-finish)
        ("xp" "Activity (physical)" entry (file+headline "~/activity.org" "Log") "Act: physical \nType: %^{Type}\nClass: %^{Class}\nDate: %u\nStart time: %^{Time}\nDuration: %^{Duration}\nUnit: mins" :immediate-finish)
        ("xx" "Activity (other)" entry (file+headline "/path/to/file" "Log") "Activity: %^{Activity}\n%?\nDate: %u\nStart time: %^{Time}\nDuration: %^{Duration}\nUnit: mins")
        ("w" "Link" entry (function orfu-handle-link) "* TODO %(orfu-wash-link)\nAdded: %T\n%(orfu-link-hooks)\n%?")
        ("z" "Diary"   entry (file+olp+datetree (file ,org-diary-file))                "**** %U%?%a \n")
        ("j" "Journal YTB"   item (file+function ,org-work-journal-file org-find-ytb) "- %?" :jump-to-captured t) ; :tree-type week
        ("l" "Journal Log"   entry (file+function ,org-work-journal-file org-find-log) "**** %T - %^{Activity}  %^g%^{Type}p")

        ("t" "Add Task" entry (file+headline "~/todo.txt" "Inbox") "* TODO %?\n:PROPERTIES:\n:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U\n:END:" :prepend t)
        ("p" "Protocol" entry (file+headline "~/todo.txt" "Inbox") "* NOTE %?\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n:PROPERTIES:\n:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U\n:URL:      %c\n:END:")
        ("L" "Pro Link" entry (file+headline "~/todo.txt" "Inbox") "* NOTE %?\n[[%:link][%:description]]\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n:PROPERTIES:\n:ID:  %(shell-command-to-string \"uuidgen\"):CREATED:  %U\n:URL:      %c\n:END:")

        ("s" "Test" plain (file+function "~/test.org" org-find-heading-in-datetree) "***** TODO %?
  * CAL-IN Diet for day %t
  %^{Weight}p
  | Food / Exercise | Calories | Quantity | Total |
  |-----------------+----------+----------+-------|
  | %?              |          |          |       |
  |-----------------+----------+----------+-------|
  | Total           |          |          |       |
  |-----------------+----------+----------+-------|
  #+TBLFM: $4=$2*$3;%.0f::$LR4=vsum(@2$4..@-I$4)
  ")
        ("W" "WP TEST" entry (function bzg-find-location)   "* TODO Put this after abcde\n\n" :prepend t)

        ))
(setq org-protocol-default-template-key "l")
(add-hook 'org-capture-mode-hook
          (lambda ()
            (save-restriction
              (widen)
              (setq-local org-tag-alist (org-get-buffer-tags)))))

(org-map-entries (lambda () (org-entry-get nil "WEIGHT")) "+WEIGHT>0")
(defun schedule-grocery-hook ()
  (if (string= (org-capture-get :description)
               "Grocery today")     ;Must match the description in the template
      (org-schedule 0 (format-time-string "%Y-%m-%d"))))
(add-hook 'org-capture-before-finalize-hook 'schedule-grocery-hook)
(defun add-property-with-date-captured ()
  "Add DATE_CAPTURED property to the current item."
  (interactive)
  (org-set-property "DATE_CAPTURED" (format-time-string "%F")))
(add-hook 'org-capture-before-finalize-hook 'add-property-with-date-captured)

(defun bzg-find-location ()
  "Example: find my bzg.org file and the abcde string in the current buffer"
  (find-file "~/org/bzg.org")
  (goto-char (point-min))
  (re-search-forward "abcde" nil t)
  (newline 2))

(defun org-find-heading-in-datetree ()
  (org-datetree-find-date-create (calendar-current-date))
  (goto-char (point-at-eol))
  (when (not (re-search-forward
              (format org-complex-heading-regexp-format
                      (regexp-quote "Todo today")) nil t))
    (insert "\n**** Todo today\n"))
  (goto-char (point-max)))

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



(defun orfu-wash-link ()
  "Return a pretty-printed top of `org-stored-links'.
Try to remove superfluous information, like website title."
  (let ((link (caar org-stored-links))
        (title (cl-cadar org-stored-links)))
    (org-make-link-string
     link
     (replace-regexp-in-string " - Stack Overflow" "" title))))

(defvar orfu-link-hook nil)
(defun orfu-link-hooks ()
  (prog1
      (mapconcat #'funcall
                 orfu-link-hook
                 "\n")
    (setq orfu-link-hook nil)))

(defun orfu-raise-frame ()
  (if (eq system-type 'gnu/linux)
      (call-process
       "wmctrl" nil nil nil "-i" "-R"
       (frame-parameter (selected-frame) 'outer-window-id))
    (raise-frame)))

(defun orfu-handle-link ()
  (message "orfu-handle-link 000")
  (orfu-raise-frame)
  (message "orfu-handle-link 001")
  (let ((link (caar org-stored-links))
        (title (cadr (car org-stored-links)))
        file)
    (message "orfu-handle-link 002")
    (message link)
    (cond ((string-match "^https://www.youtube.com/" link)
           (orfu-handle-link-youtube link title))
          ((string-match "^https://scholar.google.com/scholar.bib" link)
           (url-retrieve
            link
            (lambda (status)
              (let ((err (plist-get status :error)))
                (if err (error
                         "\"%s\" %s" link
                         (downcase (nth 2 (assq (nth 2 err) url-http-codes)))))
                (message (buffer-substring-no-properties
                          (point-min)
                          (point-max)))))
            nil nil t))
          ((string-match (regexp-quote "http://stackoverflow.com/") link)
           (find-file (concat org-directory "stack.org"))
           (goto-char (point-min))
           (re-search-forward "^\\*+ +Questions" nil t))
          ;; ((string-match orfu-github-project-name link)
          ;;  (let ((project-name (match-string 1 link))
          ;;        (parts (split-string title "·")))
          ;;    (setf (cl-cadar org-stored-links)
          ;;          (concat (car parts)
          ;;                  (substring (cadr parts) 7)))
          ;;    (find-file (concat org-directory "github.org"))
          ;;    (goto-char (point-min))
          ;;    (re-search-forward (concat "^\\*+ +" project-name) nil t)))
          (t
           (message "orfu-handle-link 003")
           (find-file (concat org-directory "ent.org"))
           (goto-char (point-min))
           (re-search-forward "^\\*+ +Articles" nil t)))))

(require 'async)
(defun orfu-handle-link-youtube (link title)
  (let* ((file-name (concat title ".mp4"))
         (dir undx-downloads-di)r
         (full-name
          (expand-file-name file-name dir)))
    (add-hook 'orfu-link-hook
              `(lambda ()
                 ,(concat
                   (org-make-link-string dir dir)
                   "\n"
                   (org-make-link-string full-name file-name))))
    (let* ((max-id 0)
           id
           (max-id
            (progn
              (dolist (b (buffer-list))
                (when (string-match "\\`\\*youtube-dl \\([0-9]+\\)\\*\\'" (buffer-name b))
                  (setq id (string-to-number (match-string 1 (buffer-name b))))
                  (setq max-id (max id max-id))))
              max-id))
           (output-buffer (get-buffer-create
                           (format "*youtube-dl %d*" (1+ max-id)))))
      (async-shell-command
       (format "cd %s && youtube-dl -f mp4 \"%s\" -o %s" dir link
               (shell-quote-argument file-name))
       output-buffer))
    (find-file (concat org-directory "ent.org"))
    (goto-char (point-min))
    (re-search-forward "^\\*+ +Videos" nil t)))


;; documents managements
(require 'setup-deft)
;;(require 'setup-jira)
;; see - nil org-fu.el

