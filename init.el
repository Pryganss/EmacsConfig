;;;;;;;;;;;;;
;; Init.el ;;
;;;;;;;;;;;;;

;; Simplify UI
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)


;; Font
(set-face-attribute 'default nil :height 100)

;; All the icons
(use-package all-the-icons
  :if (display-graphic-p))

;; Try
(use-package try)

;; Themes
(use-package uwu-theme)
(use-package doom-themes)
(use-package kaolin-themes)
(use-package tangotango-theme)
(use-package mood-one-theme)
(load-theme 'mood-one t t)
(enable-theme 'mood-one)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Package Manager
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa")
 			 ("melpa-stable" . "https://stable.melpa.org/packages/")
 			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Comand Log
(use-package command-log-mode)

;; Ivy
(use-package ivy
  :bind (("C-s" . swiper)
  :map ivy-minibuffer-map
  ("TAB" . ivy-alt-done))
  :config
  (ivy-mode 1))

;; Keybindings
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer pry/leader-keys
    :keymaps '(normal insert visual)
    :global-prefix "C-<tab>")

  (general-auto-unbind-keys)
  
  (pry/leader-keys
    "t" '(:ignore t :which-key "toggles")
    "tt" '(load-theme :which-key "choose theme")
    "o" '(:ignore t :which-key "org commands")
    "oa" '(org-agenda :which-key "agenda menu")
    "m" '(:ignore t :which-key "magit commands")
    "ms" '(magit-status :which-key "magit status")))


(defun pry/evil-hook ()
  (dolist (mode '(custom-mode
 		  eshell-mode
 		  git-rebase-mode
 		  erc-mode
 		  circe-server-mode
 		  circe-chat-mode
 		  sauron-mode
 		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; Evil
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :hook (evil-mode . pry/evil-hook)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-insert-state-map (kbd "C-p") 'evil-previous-visual-line)
  (define-key evil-insert-state-map (kbd "C-n") 'evil-next-visual-line)
  
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(evil-mode 1)

;; Evil collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Disable mouse
(use-package disable-mouse)
(global-disable-mouse-mode)
(mapc #'disable-mouse-in-keymap
      (list evil-motion-state-map
	    evil-normal-state-map
	    evil-visual-state-map
	    evil-insert-state-map))

(general-define-key
 "C-M-j" 'counsel-switch-buffer)

;; Ace Jump Mode
(use-package ace-jump-mode)
(define-key global-map (kbd "M-s") 'ace-jump-mode)

;; Doom Mode Line
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Which Key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key mode
  :config
  (setq which-key-idle-delay 0.3))


;; Smart parens
(use-package smartparens)
(require 'smartparens-config)
(smartparens-global-mode 1)

;; Auto-complete
(use-package auto-complete)
(global-auto-complete-mode)

;; Helpfull
(use-package helpful
  :custom
  (describe-function-function #'helpful-callable)
  (describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Hydra
(use-package hydra)

(defhydra hydra-text-scale ()
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(pry/leader-keys "ts" '(hydra-text-scale/body :which-key "scale text"))

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))


;; Magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Forge
(use-package forge
  :after magit)

;; Ace Window
(use-package ace-window)
(global-set-key (kbd "C-x o") 'ace-window)

;; Smart Semicolon
(use-package smart-semicolon)
(add-hook 'prog-mode #'smart-semicolon-mode)

;; Lorem Ipsum
(use-package lorem-ipsum)
(lorem-ipsum-use-default-bindings)

;; Org Mode
(defun pry/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))


(use-package org
  :hook (org-mode . pry/org-mode-setup)
  :config
  (setq org-ellipsis " ⌄"
	org-hide-emphasis-markers t)
  
  (setq org-agenda-files
	'("~/.emacs.d/OrgFiles/Tasks.org"
	  "~/.emacs.d/OrgFiles/Habits.org"))

  (setq org-agenda-skip-scheduled-if-done t)

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  
  (setq org-refile-targets
	'(("Archive.org" :maxlevel . 1)
	  ("Tasks.org" :maxlevel . 1)))

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-todo-keywords
	'((sequence "TODO(t)"  "WAIT(w@/!)" "NEXT(n)" "|" "DONE(d)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-agenda-custom-commands
	'(("d" "Dashboard"
	   ((agenda "" ((org-deadline-warning-days 7)))
	    (todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))
	    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))
	  
	  ("n" "Next Tasks"
	   ((todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))))
	  
	  ("W" "School Tasks" tags-todo "+school")
	  
	  ;; Low-effort next actions
	  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	   ((org-agenda-overriding-header "Low Effort Tasks")
	    (org-agenda-max-todos 20)
	    (org-agenda-files org-agenda-files)))
	  
	  ("w" "Workflow Status"
	   ((todo "WAIT"
		  ((org-agenda-overriding-header "Waiting")
		   (org-agenda-files org-agenda-files)))
	    (todo "REVIEW"
		  ((org-agenda-overriding-header "In Review")
		   (org-agenda-files org-agenda-files)))
	    (todo "PLAN"
		  ((org-agenda-overriding-header "In Planning")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "BACKLOG"
		  ((org-agenda-overriding-header "Project Backlog")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
	    (todo "READY"
		  ((org-agenda-overriding-header "Ready for Work")
		   (org-agenda-files org-agenda-files)))
	    (todo "ACTIVE"
		  ((org-agenda-overriding-header "Active Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "COMPLETED"
		  ((org-agenda-overriding-header "Completed Projects")
		   (org-agenda-files org-agenda-files)))
	    (todo "CANC"
		  ((org-agenda-overriding-header "Cancelled Projects")
		   (org-agenda-files org-agenda-files)))))))
  
  (setq org-capture-templates
	`(("t" "Tasks / Projects")
	  ("tt" "Task" entry (file+olp "~/.emacs.d/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
	  
	  ("j" "Journal Entries")
	  ("jj" "Journal" entry
           (file+olp+datetree "~/.emacs.d/OrgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
	   
           :clock-in :clock-resume
           :empty-lines 1)
	  ("jm" "Notes" entry
           (file+olp+datetree "~/.emacs.d/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :notes:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1))))
	  
	  



  
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "○" "●" "○" "●" "○" "●"))) 


(defun pry/org-mode-visual-fill ()
  (setq visual-fill-column-width 200
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . pry/org-mode-visual-fill))

(add-hook 'org-mode-hook
          (lambda () (face-remap-add-relative 'default :family "DejaVu Sans Mono")))









;; AUTO

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-sources '("~/.authinfo"))
 '(custom-safe-themes
   '("370109bfd8c7784bbf62e3f8c4f332aed0b915116e2d718bc7e2b8e0ebe35e10" "6ca5f925de5c119694dbe47e2bc95f8bad16b46d154b3e2e0ae246fec4100ec5" "c2aeb1bd4aa80f1e4f95746bda040aafb78b1808de07d340007ba898efa484f5" "5e2cdea6453f8963037723ab91c779b203fb201bf5c377094440f0c465d688ec" "2ac867f8748fdc50977c868eeeb44358e5a88efbf1e38fe310352431e4ed1be8" "47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" default))
 '(package-selected-packages
   '(visual-fill-column lorem-ipsum org-bullets smart-semicolon auto-complete forge evil-magit magit counsel-projectile projectile hydra evil-collection evil mood-one-theme try tangotango-theme general disable-mouse uwu-theme all-the-icons kaolin-themes doom-themes helpful smartparens smart-parens which-key ace-jump-mode use-package swiper command-log-mode))
 '(warning-suppress-log-types '((comp) (comp) (comp) (comp)))
 '(warning-suppress-types '((comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-date ((t (:inherit fixed-pitch :foreground "Cyan"))))
 '(org-ellipsis ((t nil)))
 '(org-habit-alert-face ((t (:background "light green" :foreground "black"))))
 '(org-habit-clear-face ((t (:background "dim gray"))))
 '(org-habit-clear-future-face ((t (:background "LightSteelBlue4"))))
 '(org-level-1 ((t (:font "DejaVu Sans" :inherit outline-1 :extend nil :height 1.2))))
 '(org-level-2 ((t (:font "DejaVu Sans" :inherit outline-2 :extend nil :height 1.1))))
 '(org-level-3 ((t (:font "DejaVu Sans" :inherit outline-3 :extend nil :height 1.05))))
 '(org-level-4 ((t (:font "DejaVu Sans" :inherit outline-4 :extend nil :height 1.0))))
 '(org-level-5 ((t (:font "DejaVu Sans" :inherit outline-5 :extend nil :height 1.1))))
 '(org-level-6 ((t (:font "DejaVu Sans" :inherit outline-6 :extend nil :height 1.1))))
 '(org-level-7 ((t (:font "DejaVu Sans" :inherit outline-7 :extend nil :height 1.1))))
 '(org-level-8 ((t (:font "DejaVu Sans" :inherit outline-8 :extend nil :height 1.1))))
 '(rainbow-delimiters-base-face ((t (:inherit rainbow-delimiters-base-face :foreground "rosy brown"))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "#55cdfc"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "#f7a8b8"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "#ffffff"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "#95c4a4"))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "#c2deae"))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "#faf9ce"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "#fca3c4"))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "#db8ae3"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "#ab7edd")))))
