;;; init.el --- Amadeus emacs(duli's emacs config) -*- lexical-binding: t; -*-

;; The overall load order of my emacs config.
;;
;;  > Stage 0: .emacs.d/early-init.el
;;  > Stage 1:.emacs.d/init.el
;;    > Stage 1.5: .emacs.d/packages.el
;;      > before Stage 2: install setup and begin benchmark
;;    > Stage 2: package setting
;;    > Stage 3: major mode setting
;;    > Stage 4: user config
;;  > .emacs.d/lisp/*
;;  > .emacs.d/volatile/*
;;

;;; Stage 1:
;;;; emacs version check
(when (version< emacs-version "29")
  (warn "This configuration is only tested on Emacs 30 and higher, 29 maybe fine"))

;;;; constant setup
(defvar amadeus-emacs-dir user-emacs-directory
  "The user-emacs-directory.")
(defvar amadeus-local-dir (expand-file-name ".local/" amadeus-emacs-dir)
  "The root of directory for local storage.")

(defvar amadeus-cache-dir (expand-file-name "cache/" amadeus-local-dir)
  "This directory will replaced with user-emacs-directory
   and place some unimportant stuff.")
(defvar amadeus-etc-dir (expand-file-name "etc/" amadeus-emacs-dir)
  "This directory will store global data files")

(defvar amadeus-lisp-dir (expand-file-name "lisp/" amadeus-emacs-dir)
  "This directory contains third-party Lisp files.")
(defvar amadeus-volatile-dir (expand-file-name "volatile/" amadeus-emacs-dir)
  "This directory contains volatile configuration.  All Lisp
  files are loaded automatically.  You shouldn't byte-compile
  these Lisp files.  To disable a file, just change the extension
  from .el to whatever else.")

(defvar amadeus-packages-file (expand-file-name "packages.el" amadeus-emacs-dir)
  "This file is contains all straight-use-package declaration not include modules")

;; From Henrik: https://github.com/doomemacs/doomemacs/blob/90b1b221fe7c20f2edef341a780e194cd22c7daa/lisp/doom.el#L544
;; HACK: I change `user-emacs-directory' because many packages (even built-in
;; ones) abuse it to build paths for storage/cache files (instead of correctly
;; using `locate-user-emacs-file'). This change ensures that said data files
;; are never saved to the root of your emacs directory *and* saves us the
;; trouble of setting a million directory/file variables. But it may throw off
;; anyone (or any package) that uses it to search for your Emacs initfiles.
;;
;; Although there are such package
;; (https://github.com/emacscollective/no-littering)
;; to keep the emacs directory clean, I think it is better to do it in one step.
(setq user-emacs-directory amadeus-cache-dir)
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln/" amadeus-cache-dir))

;;;; install straight and load setup
(setq straight-base-dir (file-truename amadeus-local-dir)
      straight-repository-branch "develop"
      straight-build-dir (format "build-%s" emacs-version)
      straight-vc-git-default-clone-depth '(1 single-branch)
      straight-check-for-modifications '(check-on-save find-when-checking))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" amadeus-local-dir))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(load amadeus-packages-file)
(straight-use-package 'setup)
(require 'setup)

;;;; benchmark-init
;; Benchmark-init after setup load, but require setup won't take much time. It's fine.
(setup benchmark-init
  (:require benchmark-init)
  (:with-function benchmark-init/deactivate
    (:hook-into after-init-hook)))

;;;; define setup macro
;; Most of them from emacswiki. And it may not be used, providing an option.
(setup-define :advice-into
  (lambda (symbol how)
    `(advice-add ',symbol ,how #',(setup-get 'func)))
  :documentation "Add the current function as a advice to the SYMBOL in the manner specified by HOW.
 See `advice-add' for more details."
  :after-loaded t
  :debug '(sexp sexp)
  :ensure '(nil nil)
  :repeatable t)

(setup-define :override-bind
  (lambda (key command)
    `(bind-key ,key ,command override-global-map))
  :documentation "Bind KEY to COMMAND in `override-global-map'"
  :debug '(form sexp)
  :ensure '(kbd func)
  :repeatable t)

(setup-define :face
  (lambda (face spec) `(custom-set-faces (quote (,face ,spec))))
  :documentation "Customize FACE to SPEC."
  :signature '(face spec ...)
  :debug '(setup)
  :repeatable t
  :after-loaded t)

(setup-define :if-host
  (lambda (hostname)
    `(unless (string= (system-name) ,hostname)
       ,(setup-quit)))
  :documentation "If HOSTNAME is not the current hostname, stop evaluating form.")

(setup-define :needs
  (lambda (executable)
    `(unless (executable-find ,executable)
       ,(setup-quit)))
  :documentation "If EXECUTABLE is not in the path, stop here."
  :repeatable 1)

(setup-define :option*
  (lambda (name val)
    `(customize-set-variable
      ',(intern (format "%s-%s" (setup-get 'feature) name))
      ,val
      ,(format "Set for %s's setup block" (setup-get 'feature))))
  :documentation "Set the option NAME to VAL.
NAME is not the name of the option itself, but of the option with
the feature prefix."
  :debug '(sexp form)
  :repeatable t)

(setup-define :unhook
  (lambda (func)
    `(remove-hook (quote ,(setup-get 'hook)) ,func))
  :documentation "Remove FUNC from the current hook."
  :repeatable t
  :ensure '(func)
  :signature '(FUNC ...))

(setup-define :local-unhook
  (lambda (hook &rest functions)
    `(add-hook
      (quote ,(setup-get 'hook))
      (lambda ()
        ,@(mapcar
           (lambda (arg)
             (let ((fn (cond ((eq (car-safe arg) 'function) arg)
                             ((eq (car-safe arg) 'quote)    `(function ,(cadr arg)))
                             ((symbolp arg)                 `(function ,arg))
                             (t                             arg))))
               `(remove-hook (quote ,hook) ,fn t)))
           functions))))
  :documentation "Remove FUNCTION from HOOK only in the current hook."
  :debug '(&rest sexp)
  :repeatable nil)

(setup-define :silence
  (lambda (&rest body)
    `(cl-letf (((symbol-function 'message) (lambda (&rest _args) nil)))
       ,(macroexp-progn body)))
  :documentation "Evaluate BODY but keep the echo era clean."
  :debug '(setup))

(setup-define :delay
  (lambda (time &rest body)
    `(run-with-idle-timer ,time nil
      (lambda () ,@body)))
  :documentation "Delay loading BODY until a certain amount of idle time
has passed."
  :indent 1)


;;; Stage 2: package
;;;; Outli: Org-like code outliner
;; Using for organize this file.
(setup outli
  (:hook-into prog-mode text-mode)
  (:bind-into outli-mode-map
    "C-c C-p" #'(lambda () (interactive) (outline-back-to-heading))))

;;;; Meow modal-editing
;; Another modal-editing package.
(setup meow
  (:require meow)
  (meow-global-mode 1)
  (:option
   meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-setup-indicator)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . keyboard-quit))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k") ;;因为j，k覆盖了按键，使原生按键可用得加SPC
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   ;; '("SPC" . keyboard-escape-quit)
   )
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-save)
   '("d" . meow-C-d)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("F" . meow-find-expand)
   '("g" . meow-cancel)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("I" . meow-open-above)
   '("i" . meow-insert)
   '("m" . meow-join)
   '("n" . meow-search)
   '("N" . meow-pop-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("T" . meow-till-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("V" . meow-kmacro-matches)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-kmacro-lines)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)
   '("'" . repeat)
   '("\\" . quoted-insert)
   '("<escape>" . ignore)))

;;;; vertico
(defun +vertico-crm-indicator-a (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))

(setup vertico
  (:with-function +vertico-crm-indicator-a
    (:advice-into completing-read-multiple :filter-args))
  (:hook-into after-init)
  (:option vertico-resize nil
           vertico-count 17
           vertico-cycle t)

  (:option marginalia-mode t)
  (:with-feature marginalia
    (:option nerd-icons-completion-mode t)
    (:hook nerd-icons-completion-marginalia-setup)
    (:bind-into minibuffer-local-map
      "M-A" marginalia-cycle)
    )

  (:with-feature consult-dir
    (:global [remap list-directory] consult-dir))

  (:with-feature consult
    (:global [remap switch-to-buffer] #'consult-buffer
             [remap goto-line] #'consult-goto-line
             [remap imenu] #'consult-imenu))

  (:with-feature embark
    (:global "C-'" #'embark-act
             "C-=" #'embark-dwim))

  (:with-feature orderless
    (:option completion-styles '(orderless))))

(setup doom-modeline
  (:hook-into after-init)
  (:option doom-modeline-major-mode-icon nil))

;;;; rime
(setup rime
  (:override-bind "C-," toggle-input-method)
  (:option default-input-method "rime"
           rime-user-data-dir "~/.local/share/emacs-rime"
           rime-disable-predicates '(meow-normal-mode-p
                                     meow-motion-mode-p
                                     meow-keypad-mode-p
                                     meow-beacon-mode-p)))

;;;; cns
;; a word segmentation based on jieba, need compile first.
(setup cns
  (:with-function cns-auto-enable
    (:autoload-this)
    (:hook-into find-file))
  (let ((cns-dir (file-name-directory (locate-library "cns.el"))))
    (:option* prog (expand-file-name "cnws" cns-dir)
              dict-directory (concat cns-dir "dict"))))

;;;; wakatime
(setup wakatime
  (:with-mode global-wakatime-mode
    (:hook-into after-init)))

;;;; which-key
(setup which-key
  (:hook-into after-init)
  (:option which-key-sort-order #'which-key-key-order-alpha
           which-key-sort-uppercase-first nil
           which-key-add-column-padding 1
           which-key-max-display-columns nil
           which-key-min-display-lines 6
           ;; windows setting?
           which-key-side-window-slot -10)
  (:when-loaded
    (which-key-setup-side-window-bottom)))


;;; Stage 3: major-mode settings
;; Let the configuration of major-mode be concentrated in one place for easy modification.
;;;; emacs-lisp-mode
(setup major:elisp-mode)
(defun +setup-enable-imenu-support ()
  (setf (map-elt imenu-generic-expression "Setup")
        (list (rx line-start (0+ blank)
                  "(setup" (1+ blank)
                  (or (group-n 1 (1+ (or (syntax word)
                                         (syntax symbol))))
                      ;; Add here items that can define a feature:
                      (seq "(:" (or "straight" "require" "package")
                           (1+ blank)
                           (group-n 1 (1+ (or (syntax word)
                                              (syntax symbol)))))))
              1)))

(setup imenu
  (:with-hook emacs-lisp-mode-hook
    (:hook +setup-enable-imenu-support)))
;;; Stage 4: user config
(setup config:emacs-theme
  (load-theme 'modus-operandi-tritanopia)
  (custom-theme-set-faces 'modus-operandi-tritanopia
                          '(font-lock-comment-face ((t (:inherit modus-themes-slant :foreground "#595959")))))
  )

(setup config:user
  (:option user-full-name "duli kiles"
           user-mail-address "duil4868@gmail.com"))
