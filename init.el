(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/my-custom-files"))

;;(when window-system (set-frame-size (selected-frame) 120 60))

(setq package-selected-packages '(meow which-key vertico consult kaolin-themes dictionary denote orderless corfu rainbow-delimiters org-download org-noter tangonov-theme pdf-tools mpv hydra tree-sitter tree-sitter-langs lsp-mode avy ef-themes lsp-pyright cheat-sh helpful elisp-demos cape expand-region))
(package-install-selected-packages)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(add-hook 'python-mode-hook (lambda () (require 'lsp-pyright) (lsp)))
(add-hook 'c-mode-hook #'lsp)

(setq-default indent-tabs-mode nil)
(setq js-indent-level 4)
(add-hook 'js-mode-hook #'lsp)

(setq dabbrev-ignored-buffer-regexps '("\\.\\(pdf\\|jpe?g\\|png\\)"))
(setq org-startup-folded 'nofold)
(setq org-hide-block-startup t)
(add-hook 'text-mode-hook #'flyspell-mode)

(setq sentence-end-double-space nil)
(setq-default require-final-newline t)
(setq-default indicate-empty-lines t)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(save-place-mode 1)
(electric-pair-mode 1)
(global-visual-line-mode 1)
(blink-cursor-mode 0)
(add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode 1)
                            ;;(display-line-numbers-mode 1)
                            (hl-line-mode 1)))
(defalias 'yes-or-no-p 'y-or-n-p)
(savehist-mode 1)
(recentf-mode 1)
(which-key-mode)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(vertico-mode)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(setq corfu-auto t corfu-auto-delay 0.0 corfu-auto-prefix 1)
(global-corfu-mode)
(pdf-loader-install)
(delete-selection-mode 1)
(define-key global-map (kbd "C-c r") #'consult-recent-file)
(define-key global-map (kbd "C-x b") #'consult-buffer)
(define-key corfu-map (kbd "C-SPC") #'u-insert-separator)
(define-key global-map (kbd "M-/") #'dabbrev-completion)
(define-key global-map (kbd "C-M-/") #'dabbrev-expand)
(load-theme 'ef-trio-dark t)
(define-key global-map (kbd "<f5>") #'modus-themes-toggle)
(setq dictionary-server "localhost")
(with-eval-after-load "pdf-view" (define-key pdf-view-mode-map (kbd "c") #'pdf-annot-add-highlight-markup-annotation))
(setq org-noter-auto-save-last-location t)
(setq org-noter-notes-search-path '("~/Org"))

(setq make-backup-files nil
      auto-save-mode -1
      auto-save-default nil
      create-lockfiles nil
      auto-revert-mode 1)

;;main typeface
(set-face-attribute 'default nil :family "JetBrains Mono" :height 110)
;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "Noto Sans" :height 1.0)
;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 1.0)

(setq denote-directory (expand-file-name "~/Documents/Org/"))
(setq denote-known-keywords '("emacs" "elisp" "linux" "german" "noter" "techtip" "video" "vim"))

(defun denote-journal ()
  (interactive)
  (denote
   nil
   '("journal")))

(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(require 'org-download)

;; Change annotation color
(setq pdf-annot-default-annotation-properties `((t (label . ,user-full-name))
    (text (icon . "Note")
          (color . "#ff0000"))
    (highlight (color . "SeaGreen1"))
    (squiggly (color . "orange"))
    (strike-out(color . "red"))
    (underline (color . "blue"))))

(defun my/next_video_note ()
  (interactive)
  (org-next-visible-heading 1)
  (end-of-line)
  (mpv-seek-to-position-at-point))

(defun my/previous_video_note ()
  (interactive)
  (org-previous-visible-heading 1)
  (end-of-line)
  (mpv-seek-to-position-at-point))

;; Keyboard macro for formatting subtitles
(fset 'format-subtitles
   (kmacro-lambda-form [?\C-\M-% ?^ ?\[ ?0 ?- ?9 ?\] ?+ ?$ return return ?! ?\M-< ?\C-\M-% ?^ ?\[ ?0 ?- ?9 ?\] ?\[ ?0 ?- ?9 ?\] ?: ?. ?+ ?\\ ?b return return ?! ?\M-< ?\C-\M-% ?^ ?\C-q ?\C-j return return ?!] 0 "%d"))

(defun goto-random-line ()
  "Go to a random line in this buffer."
  (interactive)
  (goto-line (1+ (random (buffer-line-count)))))

(defun buffer-line-count () 
  "Return the number of lines in this buffer."
  (count-lines (point-min) (point-max)))

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)
(put 'narrow-to-region 'disabled nil)
