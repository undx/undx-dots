;;; setup-org-capture.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(require 'org-capture)
(require 'org-protocol)
;;(require 'org-protocol-capture-html)


;;
;; orca : Orca is an ORg CApture list of recipes; mainly for capturing from a browser. The more this package is
;; configured, the less refiling you’ll do on your captures: they will go directly to where they belong.
(require 'orca)
(setq orca-handler-list
      '((orca-handler-match-url "https://www.reddit.com/emacs/" "~/Dropbox/org/wiki/emacs.org" "Reddit")
        (orca-handler-match-url "https://emacs.stackexchange.com/" "~/Dropbox/org/wiki/emacs.org" "\\* Questions")
        (orca-handler-current-buffer "\\* Tasks")
        (orca-handler-file "~/Dropbox/org/ent.org" "\\* Articles")))


;; Capture bookmarklet or function: 
;; location.href='org-protocol:///capture?template=c&url='+ encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)+'&body='+ encodeURIComponent(window.getSelection());
;; %:link	URL of the web-page
;; %:description	The title of the web-page
;; %:initial	Selected text.
;;

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
;;
;; Select-from-list prompts:
;;    %^{Tidbit type|quote|zinger|one-liner|textlet}


;; Capture Templates for TODO tasks
(setq org-capture-templates
      `(
        ("t" "Talend work" )
        ("ty" "YTB"   item  (file+function ,org-work-journal-file org-find-ytb) "- %?" :jump-to-captured t) ; :tree-type week
        ("tl" "Log"   entry (file+function ,org-work-journal-file org-find-log) " %? %^g")
        ("tt" "Task"  entry (file+function ,org-work-journal-file org-find-log) "**** %T - %^{Activity}  %^g%^{Type}p")
        ("th" "HOWTO" entry (file+function ,org-work-howto-file undx-select-heading) "** %T - %^{Activity}  %^g%^{Type}p")
        
        ("T" "TODO"  entry (file+headline ,org-default-notes-file "Tasks") "** TODO %? \n SCHEDULED: %^T \n")
        ("e" "Event" entry (file+headline ,org-default-notes-file "Event") "** %? \n %^T \n")
        ("i" "INC" entry (file+headline ,org-default-notes-file "Incidents")
         "** TODO %? \n
  %^{Ticket}p %^{PIN}p %^{Computer}p %^{Location}p \n
")

        ("b" "Book" entry (file+headline ,org-default-notes-file "Books")
         "* read /%^{Title}/ by %^{Author}
:PROPERTIES:
:CREATED: %T
:END:
%\\2, a very cool writer (I always say the same about all authors)%?" :kill-buffer t)
        
        ("w" "Protocol" entry (file+headline ,org-default-notes-file "Inbox") "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?" :empty-lines 1)
        ("L" "Link"     entry (file+headline ,org-default-notes-file "Inbox") "* %? [[%:link][%:description]] \nCaptured On: %U" :empty-lines 1)
        ))






;; (setq org-capture-templates
;;       `(("c"         "Capture"
;;          entry (file+headline ,(concat org-directory "notes.org") "Inbox")
;;          "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n LINK: %:link\nDESC: %:description\nINITIAL: %:initial\n\n%?")
;;         ("a" "Appointment" entry (file org-agenda-file-gcal) (file ,(concat org-capture-templates-dir "appointment.org")))
;;         ("x" "Activity")
;;         ("xm" "Activity (math)"     entry (file+headline "~/activity.org" "Log") "Act: math\nTopic: %^{Topic}\nDate: %u\nStart time: %^{Time}\nDuration: %^{Duration}\nUnit: mins" :immediate-finish)
;;         ("xp" "Activity (physical)" entry (file+headline "~/activity.org" "Log") "Act: physical \nType: %^{Type}\nClass: %^{Class}\nDate: %u\nStart time: %^{Time}\nDuration: %^{Duration}\nUnit: mins" :immediate-finish)
;;         ("xx" "Activity (other)" entry (file+headline "/path/to/file" "Log") "Activity: %^{Activity}\n%?\nDate: %u\nStart time: %^{Time}\nDuration: %^{Duration}\nUnit: mins")
;;         ("w" "Link" entry (function orfu-handle-link) "* TODO %(orfu-wash-link)\nAdded: %T\n%(orfu-link-hooks)\n%?")
;;         ("z" "Diary"   entry (file+olp+datetree (file ,org-diary-file))                "**** %U%?%a \n")
;;         ("j" "Journal YTB"   item (file+function ,org-work-journal-file org-find-ytb) "- %?" :jump-to-captured t) ; :tree-type week
;;         ("l" "Journal Log"   entry (file+function ,org-work-journal-file org-find-log) "**** %T - %^{Activity}  %^g%^{Type}p")

;;         ("t" "Add Task" entry (file+headline "~/todo.txt" "Inbox") "* TODO %?\n:PROPERTIES:\n:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U\n:END:" :prepend t)
;;         ("p" "Protocol" entry (file+headline "~/todo.txt" "Inbox") "* NOTE %?\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n:PROPERTIES:\n:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U\n:URL:      %c\n:END:")
;;         ("L" "Pro Link" entry (file+headline "~/todo.txt" "Inbox") "* NOTE %?\n[[%:link][%:description]]\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n:PROPERTIES:\n:ID:  %(shell-command-to-string \"uuidgen\"):CREATED:  %U\n:URL:      %c\n:END:")

;;         ("s" "Test" plain (file+function "~/test.org" org-find-heading-in-datetree) "***** TODO %?
;;   * CAL-IN Diet for day %t
;;   %^{Weight}p
;;   | Food / Exercise | Calories | Quantity | Total |
;;   |-----------------+----------+----------+-------|
;;   | %?              |          |          |       |
;;   |-----------------+----------+----------+-------|
;;   | Total           |          |          |       |
;;   |-----------------+----------+----------+-------|
;;   #+TBLFM: $4=$2*$3;%.0f::$LR4=vsum(@2$4..@-I$4)
;;   ")
;;         ("W" "WP TEST" entry (function bzg-find-location)   "* TODO Put this after abcde\n\n" :prepend t)

;;         ))






;; Add ID automatically on capture
(add-hook 'org-capture-prepare-finalize-hook 'org-id-store-link)



(defun my/org-capture-during-meeting (task)
  "Capture todo task with or without deadline, populate task :Via: field with meeting task,
and then insert a link in line of the new todo task."
  (interactive "sTask: ")
  (call-interactively 'org-store-link)
  (save-excursion
    (org-insert-heading-respect-content)
    (org-return)
    (org-capture 0)
    (org-previous-visible-heading 1)
    (org-cut-subtree)
    (org-do-demote)
    (org-end-of-line)
    (insert task)
    (let ((parent-task
           ;; ;; This implementation prompts due to the use of 'org-insert-last-stored-link.
           ;; (replace-regexp-in-string "\n" ""
           ;;                           (with-temp-buffer
           ;;                             (org-mode)
           ;;                             (org-insert-last-stored-link 1)
           ;;                             (buffer-string)))))
           ;; ;; This implementation requires 'set-window-buffer due to 'execute-kbd-macro.
           ;; ;; To prevent 'y-or-no-p dialog box, set use-dialog-box to nil.
           (with-temp-buffer
             (save-window-excursion
               (set-window-buffer nil (current-buffer))
               (org-mode)
               (execute-kbd-macro [?\C-c ?\C-l return return]))
             (buffer-string))))
      (org-set-property "Via" parent-task))
    (call-interactively 'org-store-link)
    (if (y-or-n-p "Set deadline?")
        (call-interactively 'org-deadline))
    (if (y-or-n-p "Set scheduled?")
        (call-interactively 'org-schedule))
    (org-cycle))
  ;; (org-insert-last-stored-link 1)
  (execute-kbd-macro [?\C-c ?\C-l return return]))
;;(org-delete-backward-char 1))

(define-key org-mode-map "\C-cm" 'my/org-capture-during-meeting)

;; Redefine org-cut-special to also exit org-capture

(defun my/org-cut-special-and-exit-org-capture ()
  (interactive)
  (org-cut-special)
  ;; ;; This doesn't work as intended
  ;; (org-capture-kill)
  (kill-buffer)
  (delete-window))




(define-key org-capture-mode-map (kbd "C-c C-x C-w") 'my/org-cut-special-and-exit-org-capture)

(defun my/generate-openssl-password ()
  "Automatically generate a 15 character password using OpenSSL for the Account capture template."
  (replace-regexp-in-string "\n\\'" ""
                            (shell-command-to-string "openssl rand -base64 15")))

(defun my/insert-openssl-password ()
  "Insert an OpenSSL password from `my/generate-openssl-password' as a string."
  (interactive)
  (insert (my/generate-openssl-password)))




(provide 'setup-org-capture)


;;; setup-org-capture.el ends here
