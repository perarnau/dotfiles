;; -*- mode: elisp -*-

;; Disable the splash screen

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq inhibit-splash-screen t)

;; Move the backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; Go back to the dark side of the force
(add-to-list 'load-path "~/.emacs.d/undo-tree")
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

;; Remove all that terrible clutter
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Enable transient mark mode
(transient-mark-mode 1)

;; Text filling that makes sense
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 80)

;; Color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(load-theme 'solarized t)
(setq-default solarized-termcolors 16)

;; c mode config
(require 'cc-mode)
(setq-default c-default-style "linux"
	      c-basic-offset 4)
(setq font-lock-maximum-decoration t)


(setq browse-url-browser-function 'browse-url-generic
	browse-url-generic-program "xdg-open")

;; nxhtml
;;(load "~/.emacs.d/nxhtml/autostart.el")

;; new plugins
;;(add-to-list 'load-path "~/.emacs.d/modes")
;;(require 'jinja)

;;;; org-mode configuration
;; Enable org-mode
(add-to-list 'load-path "~/.emacs.d/org-mode")
(require 'org)

; some evil bindings to make it easier
(define-key evil-normal-state-map (kbd "TAB") 'org-cycle)

; Some initial languages we want org-babel to support
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (shell . t)
   (python . t)
   (R . t)
   (ruby . t)
   (ditaa . t)
   (dot . t)
   (octave . t)
   (sqlite . t)
   (perl . t)
   (screen . t)
   (plantuml . t)
   ))

(setq org-confirm-babel-evaluate nil)

;; A few nice options for org-mode
(setq org-startup-indented t)
(setq org-indent-mode t)
(setq org-agenda-include-all-todo t)
(setq org-agenda-include-diary t)
(setq org-log-into-drawer t)
(setq org-tags-match-list-sublevels 'indented)

(setq org-directory "~/org/")
(setq org-default-notes-file "~/org/refile.org")
(setq org-agenda-files (list org-directory))
(setq org-capture-templates
      '(("c" "Conference" entry (file+datetree+prompt "~/org/trips.org") (file "~/org/templates/workflow-conference.org"))
	))

;;;;;; Refile targets
;; any headline with level <= 2 is a target
(setq org-refile-targets '((nil :maxlevel . 3)
                                ; all top-level headlines in the
                                ; current buffer are used (first) as a
                                ; refile target
                           (org-agenda-files :maxlevel . 3)))

;; provide refile targets as paths, including the file name
;; (without directory) as level 1 of the path
(setq org-refile-use-outline-path 'file)

;; allow to create new nodes (must be confirmed by the user) as
;; refile targets
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; refile only within the current buffer
(defun my/org-refile-within-current-buffer ()
  "Move the entry at point to another heading in the current buffer."
  (interactive)
  (let ((org-refile-targets '((nil :maxlevel . 5))))
    (org-refile)))

;; inline images from org-babel execution
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-display-inline-images)

;; Bind key features of org-mode
(global-set-key "\C-c'" 'org-edit-src-code)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
