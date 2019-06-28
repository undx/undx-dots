;;; Package -- my functions 
;;; Code:
;;; Commentary: 
(defun delete-to-end-of-buffer ()
  "Deletes everything from point to 'point-max."
  (interactive)
  (kill-region (point) (point-max)))

(defun delete-to-beginning-of-buffer()
  (interactive)
  (kill-region (point) (point-min)))

(defun copy-current-buffer-name ()
  "Copy the current 'buffer-file-name in the kill ring."
  (interactive)
  (let ((name (buffer-file-name)))
    (kill-new name)
    (message name)))

(defun copy-line (arg)
  "Copy lines (as many as prefix ARG) in the kill ring.
Ease of use features:
 - Move to start of next line.
 - Appends the copy on sequential calls.
 - Use newline as last char even on the last line of the buffer.
 - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun simple-copy-line (arg)
  "Copy lines (as many as prefix ARG) in the kill ring."
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(autoload 'copy-from-above-command "misc"
    "Copy characters from previous nonblank line, starting just above point.
  
  \(fn &optional arg)"
    'interactive)

(defun copy-above-to-char (arg char)
  "Copy all characters from the previous line
beginning with the character currently above the cursor up to the ARG th occurrence
of CHAR."
  (interactive "p\ncCopy to char: ")
  (let* ((col (current-column))
         (n (save-excursion
              (forward-line -1)
              (move-to-column col)
              (search-forward (char-to-string char)
                              (line-end-position) nil arg)
              (- (current-column) col))))
    (copy-from-above-command n)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/move-beginning-of-line ()
  "Move to indentation, or beginning of the line."
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-show-in-mode-line (text &optional buffer delay)
  "Display TEXT in BUFFER's mode line.
The text is shown for DELAY seconds (default 2), or until a user event.
So call this last in a sequence of user-visible actions."
  (message nil) ; Remove any current msg
  (with-current-buffer (or buffer  (current-buffer))
    (make-local-variable 'mode-line-format) ; Needed for Emacs 21+.
    (let ((mode-line-format  text))
      (force-mode-line-update) (sit-for (or delay  2)))
    (force-mode-line-update)))

(my-show-in-mode-line "my-functions parsed...")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;http://blog.binchen.org/posts/diff-regions-in-emacs.html I know M-x
;; ediff-regions-linewise. But it's kind of too generic. I only want to view
;; the different lines of two regions as quickly as possible.

;; Diff two regions
;; Step 1: Select a region and `M-x diff-region-tag-selected-as-a'
;; Step 2: Select another region and `M-x diff-region-compare-with-b'
(defun diff-region-format-region-boundary (b e)
  "Make sure lines are selected and B is less than E."
  (let (tmp rlt)
    ;; swap b e, make sure b < e
    (when (> b e)
      (setq tmp b)
      (setq b e)
      (set e tmp))
    ;; select lines
    (save-excursion
      ;; Another workaround for evil-visual-line bug:
      ;; In evil-mode, if we use hotkey V or `M-x evil-visual-line` to select line,
      ;; the (line-beginning-position) of the line which is after the last selected
      ;; line is always (region-end)! Don't know why.
      (if (and (> e b)
               (save-excursion (goto-char e) (= e (line-beginning-position)))
               (boundp 'evil-state) (eq evil-state 'visual))
          (setq e (1- e)))
      (goto-char b)
      (setq b (line-beginning-position))
      (goto-char e)
      (setq e (line-end-position)))
    (setq rlt (list b e))
    rlt))

(defun diff-region-tag-selected-as-a ()
  "Select a region to compare."
  (interactive)
  (when (region-active-p)
    (let (tmp buf)
      ;; select lines
      (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
      (setq buf (get-buffer-create "*Diff-regionA*"))
      (save-current-buffer
        (set-buffer buf)
        (erase-buffer))
      (append-to-buffer buf (car tmp) (cadr tmp))))
  (message "Now select other region to compare and run `diff-region-compare-with-b`"))

(defun diff-region-compare-with-b ()
  "Compare current region with region selected by `diff-region-tag-selected-as-a'."
  (interactive)
  (if (region-active-p)
      (let (rlt-buf
            diff-output
            (fa (make-temp-file (expand-file-name "scor"
                                                  (or small-temporary-file-directory
                                                      temporary-file-directory))))
            (fb (make-temp-file (expand-file-name "scor"
                                                  (or small-temporary-file-directory
                                                      temporary-file-directory)))))
        (when fb
          (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
          (write-region (car tmp) (cadr tmp) fb))

        (setq rlt-buf (get-buffer-create "*Diff-region-output*"))
        (when (and fa (file-exists-p fa) fb (file-exists-p fb))
          (save-current-buffer
            (set-buffer (get-buffer-create "*Diff-regionA*"))
            (write-region (point-min) (point-max) fa))
          (setq diff-output (shell-command-to-string (format "diff -Nabur %s %s" fa fb)))
          ;; show the diff output
          (if (string= diff-output "")
              (message "Two regions are SAME!")
            (save-current-buffer
              (switch-to-buffer-other-window rlt-buf)
              (set-buffer rlt-buf)
              (erase-buffer)
              (insert diff-output)
              (diff-mode))))

        (if (and fa (file-exists-p fa))
            (delete-file fa))
        (if (and fb (file-exists-p fb))
            (delete-file fb)))
    (message "Please select region at first!")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ispell and Abbrev, the Perfect Auto-Correct
;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.  
With prefix P, create local abbrev. Otherwise it will
be global.  
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (thing-at-point 'word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word))
      (setq aft (thing-at-point 'word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://emacs.stackexchange.com/questions/13970/fixing-double-capitals-as-i-type/13975#13975
(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))
;;(add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
;;And the minor mode definition:
(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc functions 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun write-packages-list ()
  "Writes the month's current packages list."
  (let* ((pl package-activated-list)
         (fpl (concat temporary-file-directory (system-name) ".packageslist-" (format-time-string "%Y%m"))))
    (unless (file-exists-p fpl)
      (message "writing packages in %s" fpl)
      (append-to-file (format "%s" pl) nil fpl))
    ))
(write-packages-list)

(defun insert-ts-sec ()
  "Insert TS unix epoq format."
  (interactive)
  (insert (format-time-string "%s")))
(defun insert-ts-iso ()
  "Insert TS ISO format."
  (interactive)
  (insert (format-time-string "%F")))


(defmacro sacha/def-wordsearch (name docstring skip-syntax-function re-search-function)
  `(defun ,name ()
     ,docstring
     (interactive)
     (let ((cur (point)))
       (,skip-syntax-function "w_")
       (let ((current-word (current-word)))
         (message "Searching for word: %s" current-word)
         (goto-char
          (if (,re-search-function
               (concat "\\_<" (regexp-quote current-word) "\\_>") nil t)
              (match-beginning 0)
            cur))))))

(sacha/def-wordsearch sacha/search-word-forward
                      "Find the next occurrence of the current word."
                      skip-syntax-forward re-search-forward)

(sacha/def-wordsearch sacha/search-word-backward
                      "Find the previous occurrence of the current word."
                      skip-syntax-backward re-search-backward)



(defun try-expand-by-dict (old)
  ;; old is true if we have already attempted an expansion
  (unless (bound-and-true-p ispell-minor-mode)
    (ispell-minor-mode 1))
  ;; english-words.txt is the fallback dictionary
  (if (not ispell-alternate-dictionary)
      (setq ispell-alternate-dictionary
            (file-truename (concat user-emacs-directory "/dics/english-words.txt"))))
  (let ((lookup-func (if (fboundp 'ispell-lookup-words)
                         'ispell-lookup-words
                       'lookup-words)))
    (unless old
      (he-init-string (he-lisp-symbol-beg) (point))
      (if (not (he-string-member he-search-string he-tried-table))
          (setq he-tried-table (cons he-search-string he-tried-table)))
      (setq he-expand-list
            (and (not (equal he-search-string ""))
                 (funcall lookup-func (concat (buffer-substring-no-properties (he-lisp-symbol-beg) (point)) "*")))))
    (if (null he-expand-list)
        (if old (he-reset-string))
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t)
    ))


(defun cleanup-buffer-safe ()
  "perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun trailing-whitespace ()
  (setq-local show-trailing-whitespace t))

(defun no-trailing-whitespace ()
  (setq-local show-trailing-whitespace nil))

(provide 'setup-defuns)
;;; setup-defuns.el ends here


