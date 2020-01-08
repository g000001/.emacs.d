(add-hook 'before-init-hook (lambda () (message "before-init-hook")))

(require 'cl-lib)

(progn
  (add-to-list 'load-path "/l/src/rw/slime")
  (require 'slime)
  (slime-setup '(slime-fancy
                 slime-quicklisp)))

(progn
  ;; git clone http://mumble.net/~campbell/git/paredit.git
  (add-to-list 'load-path "/l/src/rw/paredit")
  (require 'paredit))


(require 'skk)


(add-to-list 'load-path "~/.emacs.d/lisp/")

(add-to-list 'load-path "/l/src/rw/powerline")
(require 'powerline)

(load "pc-ppr")
(load "my-switch")

(add-to-list 'load-path "/l/src/rw/powerline")

(add-to-list 'load-path "/l/src/rw/cl-font-lock/")
(require 'cl-font-lock)


;;(add-to-list 'load-path "/l/src/rw/magit/lisp/")
;;(require 'magit)
;;(load "/l/src/rw/magit/magit-autoloads.el")

(add-to-list 'load-path "/l/src/rw/dash.el")
(require 'dash)

(add-to-list 'load-path "/l/src/rw/transient/lisp")
(require 'transient)

(add-to-list 'load-path "/l/src/rw/with-editor")
(require 'with-editor)

;LOAD_PATH  = -L /l/src/rw/magit/
;DASH_DIR=/l/src/rw/dash.el TRANSIENT_DIR=/l/src/rw/transient/lisp WITH_EDITOR_DIR=/l/src/rw/with-editor LIBGIT_DIR=/l/src/rw/libegit2 

;; M-x package-refresh-contents RET


(require 'private)
