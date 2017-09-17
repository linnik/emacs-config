(prelude-require-packages
 '(multiple-cursors window-number circe ag
                    transpose-frame pyenv-mode virtualenvwrapper
                    markdown-mode sphinx-doc material-theme
                    ibuffer-projectile ibuffer-projectile
                    magit magit-gitflow
                    ))

(disable-theme 'zenburn)
(global-hl-line-mode -1)
(scroll-bar-mode -1)
(set-default-font "Monaco 12")
(setq ibuffer-default-sorting-mode 'filename/process)
(setq prelude-theme nil)
(setq prelude-flyspell nil)
(setq flx-ido-threshold 5000)
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")
;; increase Emacs GC threshold for flx-ido
(setq gc-cons-threshold 20000000)

(require 'window-number)
(window-number-mode)
(window-number-meta-mode)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; prelude modules
(require 'prelude-css)
(require 'prelude-emacs-lisp)
(require 'prelude-erc)
;; web-mode is used for js editing instead of js2-mode
;; (require 'prelude-js)
(require 'prelude-lisp)
(require 'prelude-org)
(require 'prelude-python)
;; (require 'prelude-scss)
(require 'prelude-web)
(require 'prelude-xml)
(require 'prelude-key-chord)

(setq projectile-switch-project-action 'projectile-dired)

;; reuse dired buffers
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))
                                        ; was dired-up-directory
            ))

;; delete buffer on quit (usually 'q' keybinding)
(defadvice quit-window (before quit-window-always-kill)
  "When running `quit-window', always kill the buffer."
  (ad-set-arg 0 t))
(ad-activate 'quit-window)

;; disable graphical dialogs
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent `yes-or-no-p' from activating a dialog."
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent `y-or-n-p' from activating a dialog."
  (let ((use-dialog-box nil))
    ad-do-it))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-todo-keywords
      '((sequence "TODO(t)" "PROGRESS(p)" "|" "DONE(d)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "|" "CANCELED(c)")))

(add-hook 'python-mode-hook (lambda ()
                              (require 'sphinx-doc)
                              (sphinx-doc-mode t)))

;; uncomment this when using js2
;; (setq-default js-indent-level 2)
;; (setq-default js2-basic-offset 2)

;; web-mode configuration

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-indent-style 2)
    (setq web-mode-attr-indent-offset 1)
    (turn-off-show-smartparens-mode)
    (if (equal web-mode-content-type "html")
        (whitespace-mode -1)))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Projects in dired buffer
(require 'ibuffer-projectile)
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'filename/process)
              (ibuffer-do-sort-by-filename/process))))

;; Title format
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))
;; Codings
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Session management and recovering
(require 'desktop)
(setq desktop-path (list "~/.emacs-desktop/"))
(desktop-save-mode 1)
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)
(add-hook 'focus-out-hook 'my-desktop-save)

(provide 'prelude-custom)
;;; prelude-custom.el ends here
