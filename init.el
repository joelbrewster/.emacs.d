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

(use-package async
  :straight
  (async
   :type git
   :host github
   :repo "jwiegley/emacs-async")
  :config
  (dired-async-mode 1))

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
  :config
  (require 'consult-selectrum)
  :bind
  (( "C-c o" . consult-outline)
   ( "C-x b" . consult-buffer)
   ( "C-x 4 b" . consult-buffer-other-window)
   ( "C-x 5 b" . consult-buffer-other-frame)
   ( "C-x r x" . consult-register)
   ( "C-x r b" . consult-bookmark)
   ( "M-s g" . consult-grep)
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
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

(defun eshell-here ()
  (interactive)
  (let* ((parent (if (buffer-file-name)
		     (file-name-directory (buffer-file-name))
		   default-directory))
	 (name (car (last (split-string parent "/" t)))))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    ))
(global-set-key (kbd "C-c t") 'eshell-here)

(add-hook 'dired-mode-hook
	    (lambda ()
	      (dired-hide-details-mode)))

(use-package elfeed
  :straight
  (elfeed
   :type git
   :host github
   :repo "skeeto/elfeed")
  :bind (
	 ("C-c e" . elfeed)
	 (:map elfeed-show-mode-map
	       ("n" . next-line)
	       ("p" . previous-line)
	       ("f" . forward-char)
	       ("b" . backward-char)
	       ("v" . elfeed-show-next)
	       ("V" . elfeed-show-prev)
	       ))
  :config
  (setf url-queue-timeout 30)
  ;; (setq elfeed-search-title-max-width 50)
  ;; (setq elfeed-search-trailing-width 25)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-feeds
	'(("https://www.reddit.com/r/emacs.rss")
	  ("https://www.reddit.com/r/orgmode.rss")
	  ("https://hnrss.org/frontpage?link=comments")
	  ("https://planet.emacslife.com/atom.xml")
	  ("http://xenodium.com/rss.xml")
	  ("https://joshwcomeau.com/rss.xml")
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g")
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCduKuJToxWPizJ7I2E6n1kA")
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCshObcm-nLhbu8MY50EZ5Ng")
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ")
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ")
	  ("https://twitrss.me/twitter_search_to_rss/?term=emacs+org+mode")
	  )))
;; Store elfeed entry store link for capture template
(defun private/org-elfeed-entry-store-link ()
  (when elfeed-show-entry
    (let* ((link (elfeed-entry-link elfeed-show-entry))
	   (title (elfeed-entry-title elfeed-show-entry)))
      (org-store-link-props
       :link link
       :description title)
      )))
(add-hook 'org-store-link-functions
	  'private/org-elfeed-entry-store-link)

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
  :init
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
  :init
  (move-text-default-bindings))

(use-package multiple-cursors
  :straight
  (multiple-cursors
   :type git
   :host github
   :repo "magnars/multiple-cursors.el")
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
  :config
  (setq-default olivetti-body-width 120)
  (setq olivetti-body-width 120))
(add-hook 'dired-mode-hook 'olivetti-mode)
(add-hook 'erc-mode-hook 'olivetti-mode)
(add-hook 'eshell-mode-hook 'olivetti-mode)
(add-hook 'info-mode-hook 'olivetti-mode)
(add-hook 'special-mode-hook 'olivetti-mode)
(add-hook 'text-mode-hook 'olivetti-mode)
(add-hook 'prog-mode-hook 'olivetti-mode)

(setq org-use-speed-commands t)
(setq org-confirm-babel-evaluate 'nil)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d!)")))
(setq org-todo-keyword-faces
      '(("TODO" . "#7bd88f") ("NEXT" . "#fd9353") ("WAIT" . "#948ae3") ("DONE" . "#c6c6c6"))
      )
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

	("e" "Email" entry (file "~/org/life.org")
	 "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
	))

(use-package prettier-js
  :straight
  (prettier-js
   :type git
   :host github
   :repo "prettier/prettier-emacs"))
(add-hook 'js-mode-hook 'prettier-js-mode)

(use-package project
  :bind
  (("C-c p" . project-switch-project)
   ("C-c f" . project-find-file)
   ("C-c g" . project-find-regexp)))

(use-package rainbow-delimiters
    :straight
    (rainbow-delimiters
     :type git
     :host github
     :repo "Fanael/rainbow-delimiters"))
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

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
  :init
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

(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)
