;;; init.el --- Description -*- lexical-binding: t; -*-

;;; Stage 1:
;;;; emacs version check
(when (version< emacs-version "29")
  (warn "This configuration is only tested on Emacs 29"))

;;;; constant setup
(defvar amadeus-emacs-dir user-emacs-directory
  "The user-emacs-directory")
(defvar amadeus-cache-dir (expand-file-name ".local/cache" amadeus-emacs-dir)
  "This directory will replaced with user-emacs-directory
   and place some unimportant stuff")
(defvar amadeus-etc-dir (expand-file-name ".local/etc" amadeus-emacs-dir)
  "This directory will store global data files")

(defvar amadeus-lisp-dir (expand-file-name "lisp" amadeus-emacs-dir)
  "This directory contains third-party Lisp files.")
(defvar amadeus-volatile-dir (expand-file-name "volatile" amadeus-emacs-dir)
  "This directory contains volatile configuration.  All Lisp
  files are loaded automatically.  You shouldn't byte-compile
  these Lisp files.  To disable a file, just change the extension
  from .el to whatever else.")

(defvar amadeus-packages-file (expand-file-name "packages.el" amadeus-emacs-dir)
  "This file is contains all straight-use-package declaration not include modules")

;;;; change user-emacs-directory
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

;;;; install straight.el, load package.el and require setup
(setq straight-repository-branch "develop")
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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
(setup benchmark-init
  (:require benchmark-init)
  (:with-function benchmark-init/deactivate
    (:hook-into after-init-hook)))

;;;; define setup macro
(setup-define :silence
  (lambda (&rest body)
    `(cl-letf (((symbol-function 'message) (lambda (&rest _args) nil)))
       ,(macroexp-progn body)))
  :documentation "Evaluate BODY but keep the echo era clean."
  :debug '(setup))

(setup-define :option*
  (setup-make-setter
   (lambda (name)
     `(funcall (or (get ',name 'custom-get)
                   #'symbol-value)
       ',name))
   (lambda (name val)
     `(progn
        (custom-load-symbol ',name)
        (funcall (or (get ',name 'custom-set) #'set-default)
                 ',name ,val))))

  :documentation "Like default `:option', but set variables after the feature is
loaded."
  :debug '(sexp form)
  :repeatable t
  :after-loaded t)

(setup-define :after
  (lambda (feature &rest body)
    `(:with-feature ,feature
      (:when-loaded ,@body)))
  :documentation "Eval BODY after FEATURE."
  :indent 1)

(setup-define :delay
  (lambda (time &rest body)
    `(run-with-idle-timer ,time nil
      (lambda () ,@body)))
  :documentation "Delay loading BODY until a certain amount of idle time
has passed."
  :indent 1)

;;  src: https://emacs.nasy.moe/#Setup-EL
(setup-define :autoload
  (lambda (func)
    (let ((fn (if (memq (car-safe func) '(quote function))
                  (cadr func)
                func)))
      `(unless (fboundp (quote ,fn))
         (autoload (function ,fn) ,(symbol-name (setup-get 'feature)) nil t))))
  :documentation "Autoload COMMAND if not already bound."
  :repeatable t
  :signature '(FUNC ...))

(setup-define :init
  (lambda (&rest body) (macroexp-progn body))
  :documentation "Init keywords like use-package and leaf.")

(setup-define :advice
  (lambda (symbol where function)
    `(advice-add ',symbol ,where ,function))
  :documentation "Add a piece of advice on a function.
 See `advice-add' for more details."
  :after-loaded t
  :debug '(sexp sexp function-form)
  :ensure '(nil nil func)
  :repeatable t)

;;; Stage 2: package
;;;; Outli: Org-like code outliner
(setup outli
  (:hook-into prog-mode text-mode)
  (:bind-into outli-mode-map
    "C-c C-p" #'(lambda () (interactive) (outline-back-to-heading))))

;;;; Meow modal-editing
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

;;;; rime
(setup rime
  (:option default-input-method "rime"
           rime-user-data-dir "~/.local/emacs-rime"
           rime-disable-predicates '(meow-normal-mode-p
                                     meow-motion-mode-p
                                     meow-keypad-mode-p
                                     meow-beacon-mode-p)))

;;;; cns
(defun d/cns--compile-cnws-binary ()
  "compile cnws to make it available"
  (unless (fboundp 'cns) (require 'cns))
  (let* ((cns-dir
          (shell-quote-argument
           (file-name-directory (locate-library "cns.el" t))))
         (make-commands
          (concat
           "cd " cns-dir "; \
             make;"))
         (buffer (get-buffer-create "*cnws-compile-buffer*")))
    (with-current-buffer buffer
      (compilation-mode)
      (if (zerop (let ((inhibit-read-only t))
                   (call-process "sh" nil buffer t "-c" make-commands)))
          (progn
            (setq cns-dict-directory (concat cns-dir "cpp-jieba/dict"))
            (setq cns-prog (concat cns-dir "cnws"))
            (message "Compilation of `cnws' binary succeeded")
            )
        (error "Compilation of `cnws' binary failed!")))))

(defun d/cns--maybe-prompt-for-compile ()
  (let ((cns-dir (file-name-directory (locate-library "cns.el" t))))
    (if (file-exists-p (concat cns-dir "cnws"))
        t
      (yes-or-no-p
       (format "emacs-chinese-word-segmentation will use `cnws' binary, it did\'t compile yet,
compiling will take a moment, continue?")))))

(defun d/cns--before-cns-mode-enable-check ()
  (when (d/cns--maybe-prompt-for-compile)
    (d/cns--compile-cnws-binary)))

(setup cns
  (:with-function cns-auto-enable
    (:autoload-this)
    (:hook-into find-file))
  (:advice cns-mode-enable :before d/cns--before-cns-mode-enable-check))

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
