(prelude-require-packages
 '(multiple-cursors window-number circe jabber jabber-otr
                    transpose-frame pyenv-mode virtualenvwrapper
                    markdown-mode sphinx-doc material-theme))

(require 'jabber)
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
(require 'prelude-js)
(require 'prelude-lisp)
(require 'prelude-org)
(require 'prelude-python)
(require 'prelude-ruby)
(require 'prelude-scala)
;; (require 'prelude-scss)
(require 'prelude-web)
(require 'prelude-xml)
(require 'prelude-key-chord)

(setq projectile-switch-project-action 'projectile-dired)

;; reuse dired buffers
(put 'dired-find-alternate-file 'disabled nil)

;; disable graphical dialogs
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
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

(setq-default js-indent-level 2)
(setq-default js2-basic-offset 2)

(global-hl-line-mode -1)
(scroll-bar-mode -1)
(setq visible-bell 1)
(setq neo-window-position 'right)
(setq neo-smart-open t)
(setq prelude-flyspell nil)

(setq ibuffer-default-sorting-mode 'filename/process)

;; web-mode configuration

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-indent-style 4)
    (setq web-mode-attr-indent-offset 1)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)
