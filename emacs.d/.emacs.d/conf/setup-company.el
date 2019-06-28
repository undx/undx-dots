;;; setup-company.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

;; Completion will start automatically after you type a few letters.
;; Use M-n and M-p to select, <return> to complete or <tab> to complete the common part.
;; Search through the completions with C-s, C-r and C-o.
;; Press M-(digit) to quickly complete with one of the first 10 candidates.
;; Type M-x company-complete to initiate completion manually. Bind this command to a key combination of your choice.
;; When the completion candidates are shown,
;;  press <f1> to display the documentation for the selected candidate, or
;;         C-w to see its source. Not all back-ends support this.




(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-backends '((
                            company-yasnippet
                            company-capf
                            company-files
                            )))
  :diminish " ς")
;; show tooltip even if one match
(setq company-frontends '(company-pseudo-tooltip-frontend
                          company-echo-metadata-frontend))
;; enable flx matching
(with-eval-after-load 'company
  (company-flx-mode +1))
;;
;; https://oremacs.com/2017/12/27/company-numbers/
(let ((map company-active-map))
  (mapc
   (lambda (x)
     (define-key map (format "%d" x) 'ora-company-number))
   (number-sequence 0 9))
  (define-key map " " (lambda ()
                        (interactive)
                        (company-abort)
                        (self-insert-command 1)))
  (define-key map (kbd "<return>") nil))

(defun ora-company-number ()
  "Forward to `company-complete-number'.

Unless the number is potentially part of the candidate.
In that case, insert the number."
  (interactive)
  (let* ((k (this-command-keys))
         (re (concat "^" company-prefix k)))
    (if (cl-find-if (lambda (s) (string-match re s))
                    company-candidates)
        (self-insert-command 1)
      (company-complete-number (string-to-number k)))))
;;
;;
;;
;;
;;
(company-quickhelp-mode)
;;
;;
;;
(provide 'setup-company)
;;; setup-company.el ends here
