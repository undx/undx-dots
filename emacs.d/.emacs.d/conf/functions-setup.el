;;----------------------------------------------------------------------------
;; Functions
;;----------------------------------------------------------------------------
(setq ditaa-cmd "java -jar /usr/local/Cellar/ditaa/0.9/libexec/ditaa0_9.jar")
(defun undx-ditaa-generate ()
  (interactive)
  (shell-command
    (concat ditaa-cmd " " buffer-file-name)))
;;----------------------------------------------------------------------------
(defalias 'qqr 'query-replace-regexp)
(defalias 'sr 'replace-string)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vim like dt
(autoload 'zap-up-to-char "misc"
          "Kill up to, but not including ARGth occurrence of CHAR.

          \(fn arg char)"
'interactive)

(defmacro dotfile (filename)
  "Define the function `filename' to edit the dotfile in question"
  (let ((filestr (symbol-name filename)))
    `(progn
       (defun ,(intern filestr) ()
         ,(format "Open %s for editing" filestr)
         (interactive)
         (find-file ,(concat "~/" filestr))))))
(dotfile .emacs)
(dotfile .vimrc)

;;
(defun wikipedia-search (search-term)
  "Search for SEARCH-TERM on wikipedia"
  (interactive
    (let ((term (if mark-active
                  (buffer-substring (region-beginning) (region-end))
                  (word-at-point))))
      (list
        (read-string
          (format "Wikipedia (%s):" term) nil nil term)))
    )
  (browse-url
    (concat
      "http://en.m.wikipedia.org/w/index.php?search="
      search-term
      ))
  )

;; insert an empty line after the current line and position the cursor on its beginning
(defun insert-empty-line ()
  (interactive)
  (move-end-of-line nil)
  (open-line 1)
  (next-line 1))
;;
(defun google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
    (concat
      "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
      (if mark-active
        (buffer-substring (region-beginning) (region-end))
        (read-string "Google: ")))))
;;----------------------------------------------------------------------
(defun get-above-char()
  "Get character from a line above"
  (let ( (o) )
    (setq o (- (point) (line-beginning-position)))
    (save-excursion
      (if (> (line-number-at-pos) 1)
        (progn
          (forward-line -1)
          (forward-char o)
          (char-after))))))
;;
(defun get-below-char ()
  "Get character below cursor"
  (let (o m)
    (setq o (- (point) (line-beginning-position)))
    (setq m (count-lines (point-min) (point-max)))
    (save-excursion
      (if (< (line-number-at-pos) m)
        (progn
          (forward-line 1)
          (forward-char o)
          (char-after))))))
;;
(defun copy-above-char()
  "Copy character from a line above"
  (interactive)
  (insert (get-above-char)))
;;
(defun copy-below-char()
  "Copy character below cursor"
  (interactive)
  (insert (get-below-char)))
;;efun move-line-up (
(defun undx/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
;;
(defun undx/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
;; indent whole buffer
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    ))
;;
(defun switch-to-list-buffers ()
  (interactive)
  (save-excursion
    (list-buffers)
    (switch-to-buffer "*Buffer List*")
    ))
;;
(defun my-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))
;;
(defun my-decrement-number-decimal (&optional arg)
  (interactive "p*")
  (my-increment-number-decimal (if arg (- arg) -1)))
;;
(defun my-increment-number-hexadecimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer hex-format)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789abcdefABCDEF")
        (when (re-search-forward "[0-9a-fA-F]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 16) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 16 field-width) answer)))
          (if (equal (match-string 0) (upcase (match-string 0)))
            (setq hex-format "X")
            (setq hex-format "x"))
          (replace-match (format (concat "%0" (int-to-string field-width)
                                         hex-format)
                                 answer)))))))
;;
(defun my-format-bin (val width)
  "Convert a number to a binary string."
  (let (result)
    (while (> width 0)
           (if (equal (mod val 2) 1)
             (setq result (concat "1" result))
             (setq result (concat "0" result)))
           (setq val (/ val 2))
           (setq width (1- width)))
    result))

(defun my-increment-number-binary (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "01")
        (when (re-search-forward "[0-1]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 2) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 2 field-width) answer)))
          (replace-match (my-format-bin answer field-width)))))))

;;
(defun copy-buffer-filename-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                    default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
;;
(defun org-clipshot ()
  "Take a screenshot into a time stamped
  unique-named file in the same directory as

  the org-buffer and insert a link to this file."
  (interactive)
  (setq filename

        (concat
          (make-temp-name
            (concat (buffer-file-name)
                    "_"
                    (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (message filename)
  (call-process "boxcutter" nil nil nil filename)
  (insert (concat "[[file:" filename "]]"))
  (org-display-inline-images))
;;
(defun undx/buffer-cleanup ()
  "Clean up the buffer"
  (interactive)
  (delete-blank-lines)
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))

;;----------------------------------------------------------------------
(setq browse-url-browser-function '( ("php\.net" . eww-open-file)
                                    ("weitz"	   . eww-open-file)
                                    ("."	       . browse-url-default-macosx-browser)))


(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
  Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                               ((and (listp symbol) (imenu--subalist-p symbol))
                                (addsymbols symbol))

                               ((listp symbol)
                                (setq name (car symbol))
                                (setq position (cdr symbol)))

                               ((stringp symbol)
                                (setq name symbol)
                                (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))


(defun endless/comment-line (n)
  "Comment or uncomment current line and leave point after it.
  With positive prefix, apply to N lines including current one.
  With negative prefix, apply to -N lines above."
  (interactive "p")
  (comment-or-uncomment-region
    (line-beginning-position)
    (goto-char (line-end-position n)))
  (forward-line 1)
  (back-to-indentation))



(defun jcs-get-link (link)
  "Retrieve URL from current Safari page and prompt for description.
  Insert an Org link at point."
  (interactive "sLink Description: ")
  (let ((result (shell-command-to-string
                  "osascript -e 'tell application \"Google Chrome\" to return URL of document 1'")))
    (insert (format "[[%s][%s]]" (org-trim result) link))))



(defcustom smart-to-ascii '(("\x201C" . "\"")
                            ("\x201D" . "\"")
                            ("\x2018" . "'")
                            ("\x2019" . "'")
                            ;; en-dash
                            ("\x2013" . "-")
                            ;; em-dash
                            ("\x2014" . "-"))
           "Map of smart quotes to their replacements"
           :type '(repeat (cons (string :tag "Smart Character  ")
                                (string :tag "Ascii Replacement"))))

(defun my/smart-to-ascii (beg end)
  "Replace smart quotes and dashes with their ASCII equivalents"
  (interactive "r")
  (format-replace-strings smart-to-ascii
                          nil beg end))
;;transpose buffers

(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
           (let ((this-win (window-buffer))
                 (next-win (window-buffer (funcall selector))))
             (set-window-buffer (selected-window) next-win)
             (set-window-buffer (funcall selector) this-win)
             (select-window (funcall selector)))
           (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(global-set-key (kbd "C-x 4 t") 'transpose-buffers)

(provide 'functions-setup)
