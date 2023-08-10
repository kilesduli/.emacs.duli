;;; prelude-preinstall.el --- Description -*- lexical-binding: t; -*-

;;;;; Install straight.el
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

(straight-use-package 'setup)

(load amadeus-packages-file)

(require 'setup)

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

(setup-define :hooks
  (lambda (hook func)
    `(add-hook ',hook #',func))
  :documentation "Add pairs of hooks."
  :repeatable t)

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

;; TODO not work
(with-eval-after-load 'imenu
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setf (map-elt imenu-generic-expression "Setup")
                    (list (rx line-start (0+ blank)
                              "(setup" (1+ blank)
                              (or (group-n 1 (1+ (or (syntax word)
                                                     (syntax symbol))))
                                  ;; Add here items that can define a feature:
                                  (seq "(:" (or "straight" "require" "package")

                                       (group-n 1 (1+ (or (syntax word)
                                                          (syntax symbol)))))))
                          1)))))

(provide 'prelude-preinstall)
;;; prelude-preinstall.el ends here
