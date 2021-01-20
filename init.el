(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)
(defvar bootstrap-version)
(let* ((straight-repo-dir
	(expand-file-name "straight/repos" user-emacs-directory))
       (bootstrap-file
	(concat straight-repo-dir "/straight.el/bootstrap.el"))
       (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (shell-command
     (concat
      "mkdir -p " straight-repo-dir " && "
      "git -C " straight-repo-dir " clone "
      "https://github.com/raxod502/straight.el.git && "
      "git -C " straight-repo-dir " checkout 2d407bc")))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(eval-and-compile
  (setq gc-cons-threshold 402653184
	gc-cons-percentage 0.6))

(setq user-full-name "Joel Brewster"
      user-mail-address "hi@joelbrewster.com")

(use-package aggressive-indent
  :straight
  (aggressive-indent
   :type git
   :host github
   :repo "Malabarba/aggressive-indent-mode")
  :config
  (global-aggressive-indent-mode 1))

(use-package anzu
  :config
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-anzu-mode))

(use-package avy
  :straight
  (avy
   :type git
   :host github
   :repo "abo-abo/avy")
  :bind
  (("C-c j" . avy-goto-char-timer)
   ("C-c k" . avy-kill-region)
   ("C-c l" . avy-goto-line)))

(use-package consult
  :straight
  (consult
   :type git
   :host github
   :repo "minad/consult")
  :init
  (fset 'multi-occur #'consult-multi-occur)  
  :bind
  (( "C-c o" . consult-outline)
   ( "C-x b" . consult-buffer)
   ( "C-x 4 b" . consult-buffer-other-window)
   ( "C-x 5 b" . consult-buffer-other-frame)
   ( "C-x r x" . consult-register)
   ( "C-x r b" . consult-bookmark)
   ( "M-s r" . consult-ripgrep)
   ( "M-s o" . consult-outline)
   ( "M-s l" . consult-line)
   ( "M-s m" . consult-multi-occur)
   ( "M-y" . consult-yank-pop)))

(use-package scss-mode
  :straight
  (scss-mode
   :type git
   :host github
   :repo "antonj/scss-mode"))

(use-package web-mode
  :straight
  (web-mode
   :type git
   :host github
   :repo "fxbois/web-mode"))

;; overwrite selected text
(delete-selection-mode t)

;; y and n instead of yes and no everywhere
(fset 'yes-or-no-p 'y-or-n-p)

;; Make the backspace properly erase the tab instead of removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

;; Kill the whole line
(setq kill-whole-line t)

;; Normal delete setup
(normal-erase-is-backspace-mode 0)

;; Set history-length longer
(savehist-mode 1)
(setq-default history-length 500)

;; Save place mode
(save-place-mode +1)

;; Show Keystrokes quicker
(setq echo-keystrokes 0.1)

;; Visual wrap mode
(global-visual-line-mode 1)

;; Move Custom-Set-Variables to different file
(setq custom-file (concat user-emacs-directory "custom.el"))

;; So long for minified files
(when (require 'so-long nil :noerror)
  (global-so-long-mode 1))

;; Stop autosave and backups
(setq make-backup-files nil) ;; stop creating backup~ files
(setq auto-save-default nil) ;; stop creating #autosave# files
(setq create-lockfiles nil)  ;; stop creating lockfiles

;; Electric pair mode
(electric-pair-mode t)

;; Replace selection on insert
(delete-selection-mode 1)

;; Turn Off Cursor Alarms
(setq ring-bell-function 'ignore)

;; Enable global auto-revert
(global-auto-revert-mode t)

;; Change cursor to be a bar
(setq-default cursor-type 'bar)

(add-hook 'dired-mode-hook
	    (lambda ()
	      (dired-hide-details-mode)))

(use-package doom-themes
  :straight
  (doom-themes
   :type git
   :host github
   :repo "hlissner/emacs-doom-themes")
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (load-theme 'doom-monokai-spectrum t)
  (doom-themes-org-config))

(use-package emmet-mode
  :straight
  (emmet-mode
   :type git
   :host github
   :repo "smihica/emmet-mode")
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'js-mode-hook 'emmet-mode))

(if (display-graphic-p)
    (use-package git-gutter-fringe
      :straight
      (git-gutter-fringe
       :type git
       :host github
       :repo "emacsorphanage/git-gutter-fringe")
      :config
      (global-git-gutter-mode 1)
      (setq-default fringes-outside-margins t)
      (setq-default left-fringe-width 6)
      (setq-default right-fringe-width 8)
      (define-fringe-bitmap 'git-gutter-fr:added
	[224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
	nil nil 'center)
      (define-fringe-bitmap 'git-gutter-fr:modified
	[224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
	nil nil 'center)
      (define-fringe-bitmap 'git-gutter-fr:deleted
	[0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
	nil nil 'center)
      (setq git-gutter:modified-sign " ")
      (setq git-gutter:added-sign " ")
      (setq git-gutter:deleted-sign " ")))

(use-package goto-last-change
  :straight
  (goto-last-change
   :type git
   :host github
   :repo "camdez/goto-last-change.el")
  :bind ("C-c :" . goto-last-change))

(use-package vterm
  :straight
  (vterm
   :type git
   :host github
   :repo "akermu/emacs-libvterm")
  :bind ("C-c t" . vterm))

(use-package lorem-ipsum
  :straight
  (lorem-ipsum
   :type git
   :host github
   :repo "jschaf/emacs-lorem-ipsum")
  :config
  (setq lorem-ipsum-sentence-separator " "))

(use-package marginalia
  :straight
  (marginalia
   :type git
   :host github
   :repo "minad/marginalia")
  :config
  (marginalia-mode +1))

(use-package magit
  :straight
  (magit
   :type git
   :host github
   :repo "magit/magit")
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :commands (magit-status magit-blame magit-log-buffer-file magit-log-all))
(add-hook 'magit-mode-hook
	  (lambda ()
	    (setq left-fringe-width 8)))

(use-package minions
  :straight
  (minions
   :type git
   :host github
   :repo "tarsius/minions")
  :config (minions-mode 1))

(use-package move-text
  :straight
  (move-text
   :type git
   :host github
   :repo "emacsfodder/move-text")
  :config
  (move-text-default-bindings))

(use-package multiple-cursors
  :config
  (setq mc/always-run-for-all 1)
  :bind
  (("M-<mouse-1>" . mc/add-cursor-on-click)
  ("C-:" . mc/mark-all-dwim)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this)))

(use-package olivetti
  :straight
  (olivetti
   :type git
   :host github
   :repo "rnkn/olivetti")
  :hook
  (window-configuration-change . my/olivetti-mode-maybe)
  :config
  (setq-default olivetti-body-width 120)
  (setq olivetti-body-width 120)
  (olivetti-mode))

(defvar my/olivetti-whitelist-buffers '("*scratch*")
  "List of buffers for which `olivetti-mode' should be enabled automatically.")

(defvar my/olivetti-whitelist-modes '(Custom-mode
				      Info-mode
				      dired-mode
				      erc-mode
				      org-mode
				      magit-mode
				      elfeed-mode
				      eww-mode
				      lisp-interaction-mode
				      special-mode
				      vterm-mode)
  "List of modes for which `olivetti-mode' should be enabled automatically.")

(defun my/olivetti-mode-maybe (&optional frame)
  "Turn on `olivetti-mode' for lone buffers.

  Doesn't count volatile windows unless the major-mode of their associated buffer
  is found in `my/olivetti-whitelist-modes' or is derived from one of them.
  Windows from buffers whose names are found in `my/olivetti-whitelist-buffers'
  are also considered.

  If FRAME shows exactly one window, turn on `olivetti-mode' for that window.
  Otherwise, disable it everywhere."
  (let* ((whitelist-buffers my/olivetti-whitelist-buffers)
	 (whitelist-modes my/olivetti-whitelist-modes)
	 (predicate (lambda (window)
		      (with-selected-window window
			(or (buffer-file-name)
			    (member (buffer-name) whitelist-buffers)
			    (apply 'derived-mode-p whitelist-modes)))))
	 (windows (seq-filter predicate (window-list frame))))
    (if (= 1 (length windows))
	(with-selected-window (car windows)
	  (olivetti-mode 1))
      (dolist (window windows)
	(with-selected-window window
	  (olivetti-mode -1))))))

(setq org-use-speed-commands t)
(setq org-confirm-babel-evaluate 'nil)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d!)")))
(setq org-agenda-window-setup 'this-window)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-agenda-start-on-weekday nil)
(setq org-agenda-inhibit-startup t)
(setq org-agenda-skip-unavailable-files t)
(setq org-agenda-skip-scheduled-if-done t)

;; Use current window for agenda
(setq org-agenda-window-setup 'current-window)

;; More space around today and the fortnight
(setq org-agenda-span 14)
(setq org-agenda-start-day "-3d")

;; make images smaller
(setq org-image-actual-width (/ (display-pixel-width) 3))

;; Don't adapt content
(setq org-adapt-indentation nil)

;; Hide marketup elements - emphasis markers
(setq org-hide-emphasis-markers t)

;; Be able to select whole lines with shift
(setq org-support-shift-select t)

(setq org-ellipsis " [+]")
;; (setq org-habit-graph-column 60)
(setq org-refile-targets
      '(("archive.org" :maxlevel . 1)))
;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

;; Set org html output
(setq org-html-html5-fancy t
      org-html-doctype "html5")

;; Configure custom agenda views
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
	 ((agenda "" ((org-agenda-span 5)(org-deadline-warning-days 7)))
	  (todo "NEXT"((org-agenda-overriding-header "Next Tasks")))))
	("h" tags "+@home")
	("m" tags "+@mac")
	("p" tags "+@phone")
	("e" tags "+@errand")
	("n" "Next Tasks" ((todo "NEXT" ((org-agenda-overriding-header "Next Tasks")(org-agenda-files '("~/org/life.org"))))))
	("wn" "Next Work Tasks" ((todo "NEXT" ((org-agenda-overriding-header "Next Tasks")(org-agenda-files '("~/org/work.org"))))))
	))

(setq org-agenda-files (list "~/org/inbox.org"
			     "~/org/habit.org"
			     "~/org/dates.org"
			     "~/org/finances.org"
			     "~/org/calendar.org"
			     "~/org/weather.org"
			     "~/org/life.org"))
(setq org-capture-templates
      '(
	("i" "Inbox" entry (file "~/org/inbox.org")
	 "* TODO %?\n  %i\n")

	("l" "Life" entry (file "~/org/life.org")
	 "* TODO %?\n  %i\n")

	("w" "Work" entry (file "~/org/work.org")
	 "* TODO %?\n  %i\n")

	("b" "Bookmarks" entry (file "~/org/bookmarks.org")
	 "* %?\n  %i\n")

	("c" "Contacts" entry (file "~/org/contacts.org")
	 "* %?\n  %i\n")
	))

(use-package prettier-js
  :straight
  (prettier-js
   :type git
   :host github
   :repo "prettier/prettier-emacs"))
(add-hook 'js-mode-hook 'prettier-js-mode)

(use-package rainbow-delimiters
  :straight
  (rainbow-delimiters
   :type git
   :host github
   :repo "Fanael/rainbow-delimiters")
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :straight
  (rainbow-mode
   :type git
   :host github
   :repo "emacsmirror/rainbow-mode")
  :hook (prog-mode . rainbow-mode))

(use-package reveal-in-osx-finder
  :straight
  (reveal-in-osx-finder
   :type git
   :host github
   :repo "kaz-yos/reveal-in-osx-finder"))

(use-package selectrum
  :straight
  (selectrum
   :type git
   :host github
   :repo "raxod502/selectrum")
  :config
  (setq selectrum-refine-candidates-function #'orderless-filter)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (selectrum-mode +1)
  (global-set-key (kbd "C-x C-z") #'selectrum-repeat))

(use-package selectrum-prescient
  :straight
  (selectrum-prescient
   :type git
   :host github
   :repo "raxod502/prescient.el")
  :after selectrum
  :config
  (prescient-persist-mode +1)
  (selectrum-prescient-mode +1))

(use-package which-key
:straight
(which-key
 :type git
 :host github
 :repo "justbur/emacs-which-key")
    :config
    (which-key-mode))

(defun 32-random-letter-string ()
  (interactive)
  (dotimes (_ 32)
    (insert
     (let ((x (random 36)))
       (if (< x 10) (+ x ?0) (+ x (- ?a 10)))))))

(progn
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-unset-key (kbd "M-<down-mouse-2>")))

(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "C-c p") 'project-switch-project)
(global-set-key (kbd "C-c f") 'project-find-file)
(global-set-key (kbd "C-c g") 'project-find-regexp)

(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)
