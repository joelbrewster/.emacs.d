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

(use-package bongo
  :straight
  (bongo
   :type git
   :host github
   :repo "dbrock/bongo")
  :bind ("C-c b" . bongo)
  :config
  (setq bongo-default-directory "~/Music")
  (setq bongo-prefer-library-buffers nil)
  (setq bongo-insert-whole-directory-trees t)
  (setq bongo-logo nil)
  (setq bongo-display-track-icons nil)
  (setq bongo-display-track-lengths nil)
  (setq bongo-display-header-icons nil)
  (setq bongo-display-playback-mode-indicator t)
  (setq bongo-display-inline-playback-progress t)
  (setq bongo-join-inserted-tracks nil)
  (setq bongo-field-separator (propertize " · " 'face 'shadow))
  (setq bongo-mark-played-tracks t)
  (setq bongo-header-line-mode nil)
  (setq bongo-mode-line-indicator-mode nil)
  (setq bongo-enabled-backends '(vlc))
  ;; Hide the playlist's banner
  (define-advice bongo-default-playlist-buffer
      (:override () contrib/bongo-playlist-no-banner)
    (with-current-buffer (get-buffer-create bongo-default-playlist-buffer-name)
      (unless (derived-mode-p 'bongo-playlist-mode)
	(bongo-playlist-mode))
      (current-buffer))))

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

(with-eval-after-load 'eww
  (setq eww-restore-desktop nil)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format "%u")
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  (setq eww-download-directory "~/Downloads/")
  (setq eww-suggest-uris
	'(eww-links-at-point
	  thing-at-point-url-at-point))
  (setq eww-bookmarks-directory (concat user-emacs-directory "eww-bookmarks/"))
  (setq eww-history-limit 150)
  (setq eww-use-external-browser-for-content-type
	"\\`\\(video/\\|audio/\\|application/pdf\\)")
  (setq eww-browse-url-new-window-is-tab nil)
  (setq eww-form-checkbox-selected-symbol "[X]")
  (setq eww-form-checkbox-symbol "[ ]")

  (let ((map eww-mode-map))
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "f") #'forward-char)
    (define-key map (kbd "b") #'backward-char)
    (define-key map (kbd "B") #'eww-back-url)
    (define-key map (kbd "N") #'eww-next-url)
    (define-key map (kbd "P") #'eww-previous-url)))
(add-hook 'eww-after-render-hook 'eww-readable)

(use-package exec-path-from-shell
    :straight
    (exec-path-from-shell
     :type git
     :host github
     :repo "purcell/exec-path-from-shell"))
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package esup)
(setq esup-depth 0)

(if (display-graphic-p)
    (use-package git-gutter-fringe
      :straight
      (git-gutter-fringe
       :type git
       :host github
       :repo "emacsorphanage/git-gutter-fringe")
      :config
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
      (setq git-gutter:modified-sign "")
      (setq git-gutter:added-sign "")
      (setq git-gutter:deleted-sign "")
      :init
      (global-git-gutter-mode 1)))

(use-package gnus
  :config
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-gcc-mark-as-read t)
  (setq gnus-select-method
	'(nnimap "main"
		 (nnimap-address "imap.fastmail.com")
		 (nnimap-server-port 993)
		 (nnimap-stream ssl)
		 (nnimap-authinfo-file "~/.authinfo")))
  ;; (setq gnus-secondary-select-methods
  ;; 	'((nntp "news.gwene.org")))
  (setq smtpmail-smtp-server "smtp.fastmail.com"
	smtpmail-smtp-service 587)
  (setq gnus-thread-sort-functions
	'(gnus-thread-sort-by-number
	  gnus-thread-sort-by-date))
  (setq send-mail-function 'smtpmail-send-it)
  (setq gnus-expert-user t)
  (setq inhibit-startup-screen t)
  (setq gnus-use-full-window nil)
  (setq gnus-read-active-file 'some)
  (setq gnus-use-dribble-file t)
  (setq gnus-always-read-dribble-file t)
  (setq gnus-asynchronous t)
  (setq gnus-agent-cache t)
  (setq gnus-agent-expire-all nil)
  (setq gnus-auto-select-first nil)
  (setq gnus-article-mode-line-format "%G %S %m")
  (setq gnus-group-mode-line-format "%%b")
  (setq gnus-summary-mode-line-format "[%U] %p")
  (setq gnus-group-sort-function
	'((gnus-group-sort-by-unread)
	  (gnus-group-sort-by-alphabet)
	  (gnus-group-sort-by-rank)))
  (setq gnus-agent-expire-days 30)
  :bind (("C-c m" . gnus)))

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
  :init (minions-mode 1))

(use-package modus-themes
  :straight
  (modus-themes
   :type git
   :host gitlab
   :repo "protesilaos/modus-themes")
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
	modus-themes-bold-constructs nil)

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  (setq modus-themes-vivendi-color-overrides
	'((bg-main . "#141414")
	  (bg-dim . "#262626")
	  (bg-alt . "#1e1e1e")
	  (bg-active . "#282828")
	  (bg-inactive . "#262626")))
  (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

(add-hook 'ns-system-appearance-change-functions
	  #'(lambda (appearance)
	      (mapc #'disable-theme custom-enabled-themes)
	      (pcase appearance
		('light (load-theme 'modus-operandi t))
		('dark (load-theme 'modus-vivendi t)))))

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

(use-package nyan-mode
  :straight
  (nyan-mode
   :type git
   :host github
   :repo "TeMPOraL/nyan-mode")
  :config
  (setq nyan-minimum-window-width 160)
  (setq nyan-animate-nyancat t)
  :init
  (nyan-mode 1))
(add-hook 'after-init-hook #'nyan-start-animation)

(use-package olivetti
  :straight
  (olivetti
   :type git
   :host github
   :repo "rnkn/olivetti")
  :config
  (setq-default olivetti-body-width 120)
  (setq olivetti-body-width 120))
;; (add-hook 'dired-mode-hook 'olivetti-mode)
;; (add-hook 'erc-mode-hook 'olivetti-mode)
;; (add-hook 'eshell-mode-hook 'olivetti-mode)
;; (add-hook 'info-mode-hook 'olivetti-mode)
;; (add-hook 'special-mode-hook 'olivetti-mode)
;; (add-hook 'text-mode-hook 'olivetti-mode)
;; (add-hook 'prog-mode-hook 'olivetti-mode)

(use-package openwith
  :straight
  (openwith
   :type git
   :host github
   :repo "garberw/openwith")
  :config
  (setq openwith-associations
	(list
	 ;; (list (openwith-make-extension-regexp
	 ;; 	'("dmg" "doc" "jpg" "jpeg" "png" "svg"))
	 ;;       "open"
	 ;;       '(file))
	 (list (openwith-make-extension-regexp
		'("mp4" "mp3" "webm" "avi" "flv" "mov"))
	       "open"
	       '("-a" "vlc" file))
	 ))
  (openwith-mode +1))

;; (require 'org-habit)
;; (setq org-habit-graph-column 80)

(setq org-use-speed-commands t)
(setq org-confirm-babel-evaluate 'nil)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d!)")))
;; (setq org-todo-keyword-faces
;;       '(("TODO" . "#2BC940") ("NEXT" . "#FF5F58") ("WAIT" . "##FEBC2E") ("DONE" . "#5E5E5E"))
;;       )

(setq org-agenda-window-setup 'this-window)
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
	("y" "Daily habits"
	 ((agenda ""))
	 ((org-agenda-show-log t)
	  (org-agenda-ndays 7)
	  (org-agenda-log-mode-items '(state))
	  (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
	("h" tags "+@home")
	("m" tags "+@mac")
	("p" tags "+@phone")
	("e" tags "+@errand")
	("n" "Next Tasks" ((todo "NEXT" ((org-agenda-overriding-header "Next Tasks")(org-agenda-files '("~/org/life.org"))))))
	("w" "Next Work Tasks" ((todo "NEXT" ((org-agenda-overriding-header "Next Tasks")(org-agenda-files '("~/org/work.org"))))))
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

	("r" "Resource" entry (file "~/org/life.org")
	 "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
	))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(with-eval-after-load 'org
  (bind-key "C-c C-f" #'browse-url-firefox org-mode-map))

(use-package org-download
  :straight
  (org-download
   :type git
   :host github
   :repo "abo-abo/org-download")
  :init
  (org-download-enable)
  :config
  (setq org-download-display-inline-images nil)
  (setq org-download-method 'attach))

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
  :init
  (which-key-mode))

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
;; (global-visual-line-mode 1)

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

;; Firefox path
(setq browse-url-firefox-program "/Applications/Firefox.app/Contents/MacOS/firefox-bin")

;; Window divider mode
(setq window-divider-default-right-width 1)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-places 'right-only)
(add-hook 'after-init-hook #'window-divider-mode)

;; startup maximised
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Don't ask about opening large files
(setq large-file-warning-threshold nil)

;; ;; scroll stuff
;; (pixel-scroll-mode)
;; ;; Never go back to the old scrolling behaviour.
;; (setq pixel-dead-time 0)
;; ;; Scroll by number of pixels instead of lines (t = frame-char-height pixels).
;; (setq pixel-resolution-fine-flag t)
;; ;; Distance in pixel-resolution to scroll each mouse wheel event.
;; (setq mouse-wheel-scroll-amount '(2)) 
;; (setq mouse-wheel-progressive-speed nil)

(defun 32-random-letter-string ()
  (interactive)
  (dotimes (_ 32)
    (insert
     (let ((x (random 36)))
       (if (< x 10) (+ x ?0) (+ x (- ?a 10)))))))

(defun youtube-dl ()
  (interactive)
  (let* ((str (current-kill 0))
	 (parent (if (buffer-file-name)
		     (file-name-directory (buffer-file-name))
		   default-directory))
	 (name (car (last (split-string parent "/" t)))))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " str "*"))
    (insert (concat "cd ~/Downloads && youtube-dl " str "\n"))
    (eshell-send-input)))

(defun streamvideos ()
  (interactive)
  (let* ((str (current-kill 0))
	 (parent (if (buffer-file-name)
		     (file-name-directory (buffer-file-name))
		   default-directory))
	 (name (car (last (split-string parent "/" t)))))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " str "*"))
    (insert (concat "streamlink " str " best \n vlc \n"))
    (eshell-send-input)))

(progn
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-unset-key (kbd "M-<down-mouse-2>")))

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(bind-keys
 ("s-n" . make-frame-command)
 ("s-m" . iconify-frame)
 ("s-s" . save-buffer)
 ("s-o" . find-file)
 ("s-w" . delete-frame)
 ("s-q" . save-buffers-kill-terminal)
 ("s-a" . mark-whole-buffer)
 ("s-z" . undo-only) ;; Why no redo? Read up on it.
 ("s-x" . kill-region)
 ("s-c" . kill-ring-save)
 ("s-v" . yank)
 ("s-<up>" . beginning-of-buffer)
 ("s-<down>" . end-of-buffer)
 ("s-<left>" . beginning-of-visual-line)
 ("s-<right>" . end-of-visual-line)
 ("s-b" . switch-to-buffer)
 ("s-B" . ibuffer)
 ("s-[" . previous-buffer)
 ("s-]" . next-buffer)
 ("s-k" . kill-this-buffer)
 ("s-P" . project-switch-project)
 ("M-u" . upcase-dwim)
 ("M-l" . downcase-dwim)
 ("M-c" . capitalize-dwim)
 ("C-c C-f" . browse-url-firefox)
 ("C-c w" . eww)
 )

(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)
