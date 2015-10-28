(prelude-require-packages
 '(multiple-cursors window-number circe jabber jabber-otr
                    transpose-frame pyenv-mode virtualenvwrapper))

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
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))

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