;; -*- mode: elisp -*-

;; Disable the splash screen
(setq inhibit-splash-screen t)

;; Disable the backup files
(setq make-backup-files nil)

;; Go back to the dark side of the force
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
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized-master")
(require 'color-theme)
(color-theme-initialize)
(require 'color-theme-solarized)
(color-theme-solarized-dark)

;; nxhtml
(load "~/.emacs.d/nxhtml/autostart.el")

;; new plugins
(add-to-list 'load-path "~/.emacs.d/modes")
(require 'jinja)

;;;; org-mode configuration
;; Enable org-mode
(require 'org)
;; Activate org-mode for files ending in .org
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; should be default on recent emacs

; Some initial languages we want org-babel to support
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
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

(setq org-directory "~/org/")
(setq org-default-notes-file "~/org/refile.org")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/liste.org" "Tasks")
             "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
             "* %?\n%u\n%i\n%a")))

;;;;;; Refile targets
;; any headline with level <= 2 is a target
(setq org-refile-targets '((nil :maxlevel . 2)
                                ; all top-level headlines in the
                                ; current buffer are used (first) as a
                                ; refile target
                           (org-agenda-files :maxlevel . 2)))

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
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/org/"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
