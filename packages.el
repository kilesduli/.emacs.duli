;; -*- no-byte-compile: t; -*-

;;; Stage 1.5: install package
(straight-use-package 'vertico)
(straight-use-package 'orderless)
(straight-use-package 'consult)
(straight-use-package 'embark)
(straight-use-package 'marginalia)
(straight-use-package 'which-key)

(straight-use-package 'consult-dir)
(straight-use-package 'consult-flycheck)
(straight-use-package 'embark-consult)
(straight-use-package '(vertico-posframe :host github :repo "tumashu/vertico-posframe"))
;; (straight-use-package 'flycheck)


(straight-use-package '(meow :type git :host github :repo "meow-edit/meow"))
(straight-use-package 'doom-modeline)
(straight-use-package 'doom-themes)
(straight-use-package 'nerd-icons-completion)
(straight-use-package '(org :type git :host github :repo "emacs-straight/org-mode" :pre-build (straight-recipes-org-elpa--build) :build (:not autoloads) :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))))

(straight-use-package '(outli :type git :host github :repo "jdtsmith/outli"))
(straight-use-package 'rime)
(straight-use-package '(benchmark-init :type git :repo "dholm/benchmark-init-el"))
(straight-use-package '(cns :type git :host github :repo "kanglmf/emacs-chinese-word-segmentation"
                        :files ("cppjieba/dict" "cnws" "cns.el")
                        :pre-build ("make")))

(straight-use-package '(wakatime :type git :host github :repo "Borwe/wakatime-mode"))
