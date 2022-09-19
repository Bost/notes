;; To activate changes switch to any scribble buffer and reload scribble-mode:
;;   M-x scribble-mode
(
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Rebinding.html
 ;; see local-function-key-map
 ;; `M-x local-set-key RET key cmd RET' seems not to be permanent
 (nil
  .
  ((eval
    .
    (progn

      ;; from https://www.emacswiki.org/emacs/AddKeywords
      ;; (defun scribble-add-keywords (face-name keyword-rules)
      ;;   (let* ((keyword-list (mapcar #'(lambda (x)
      ;;                                    (symbol-name (cdr x)))
      ;;                                keyword-rules))
      ;;          (keyword-regexp (concat "(\\("
      ;;                                  (regexp-opt keyword-list)
      ;;                                  "\\)[ \n]")))
      ;;     (font-lock-add-keywords 'scheme-mode
      ;;                             `((,keyword-regexp 1 ',face-name))))
      ;;   (mapc #'(lambda (x)
      ;;             (put (cdr x)
      ;;                  'scheme-indent-function
      ;;                  (car x)))
      ;;         keyword-rules))

      ;; (scribble-add-keywords
      ;;  'font-lock-keyword-face
      ;;  '(
      ;;    (1 . https)
      ;;    ;; (1 . when)
      ;;    ;; (1 . unless)
      ;;    ;; (2 . let1)
      ;;    ;; (1 . error)
      ;;    ))

    (font-lock-add-keywords 'scribble-mode
       `(
         (,(rx (group ";; " (+ not-newline)))
          . 'font-lock-comment-face)
         (,(rx (group "# " (+ not-newline)))
          . 'font-lock-comment-face)
         (,(rx (group "http" (+ (not (any space "\"" "'" "[" "{" "(")))))
          . 'font-lock-constant-face)
         ))

      (setq-local home-dir (format "%s/dev/notes/notes" (getenv "HOME")))

      (defun notes=find-file--guix.scrbl ()
        (interactive)
        (find-file
         (format "%s/guix.scrbl" home-dir)))

      (dolist (state-map `(,scribble-mode-map))
        ;; See also `set-local-keymap'
        (bind-keys
         :map state-map
         ;; The binding description doesn't appear in the `M-x helm-descbinds'
         ;; if the binding is defined using lambda:
         ;;    ("<some-key>" . (lambda () (interactive) ...))
         ("<s-f4>"  . notes=find-file--guix.scrbl)
         )))))))
