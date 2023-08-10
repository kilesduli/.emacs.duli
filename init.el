;;; init.el --- Description -*- lexical-binding: t; -*-

(when (version< emacs-version "29")
  (warn "This configuration is only tested on Emacs 29"))

;;; constant setup
(defvar amadeus-emacs-dir user-emacs-directory
  "The user-emacs-directory")
(defvar amadeus-cache-dir
  (expand-file-name ".local/cache" amadeus-emacs-dir))
(defvar amadeus-modules-dir (expand-file-name "modules" amadeus-emacs-dir)
  "The directory contains all modules.")
(defvar amadeus-lisp-dir (expand-file-name "lisp" amadeus-emacs-dir)
  "This directory contains third-party Lisp files.")
(defvar amadeus-volatile-dir (expand-file-name "volatile" amadeus-emacs-dir)
  "This directory contains volatile configuration.  All Lisp
  files are loaded automatically.  You shouldn't byte-compile
  these Lisp files.  To disable a file, just change the extension
  from .el to whatever else.")
(defvar amadeus-packages-file (expand-file-name "packages.el" amadeus-emacs-dir)
  "This file is contains all straight-use-package declaration not include modules")

(add-to-list 'load-path amadeus-modules-dir)

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

(require 'prelude-preinstall)
(require 'prelude-amadeus)

;;aria-meow
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
   '("k" . "H-k");;因为j，k覆盖了按键，使原生按键可用得加SPC
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


;;aria-vertico
(defun +vertico-crm-indicator-a (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))

(setup vertico
  (:advice completing-read-multiple :filter-args +vertico-crm-indicator-a)
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

(setup custom
  (load-theme 'doom-one-light t))

(straight-use-package 'rime)
(setup rime
  (:option default-input-method "rime"
           rime-user-data-dir "~/.local/emacs-rime"
           rime-disable-predicates '(meow-normal-mode-p
                                     meow-motion-mode-p
                                     meow-keypad-mode-p
                                     meow-beacon-mode-p
                                     )))

