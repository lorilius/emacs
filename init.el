;;packages
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(setq package-selected-packages '(which-key vertico consult denote orderless corfu rainbow-delimiters org-download org-noter pdf-tools mpv lsp-mode avy ef-themes lsp-pyright cheat-sh helpful elisp-demos cape expand-region vundo lsp-ui marginalia pug-mode magit ample-theme ztree olivetti))
(package-install-selected-packages)

;;keybindings
(define-key global-map (kbd "M-o") #'other-window)
(define-key global-map (kbd "C-c o") #'delete-other-windows)
(define-key global-map (kbd "<f7>") #'previous-buffer)
(define-key global-map (kbd "<f8>") #'next-buffer)
(define-key global-map (kbd "C-j") #'newline)
(define-key global-map (kbd "M-n") #'my/scroll-up-in-place)
(define-key global-map (kbd "M-p") #'my/scroll-down-in-place)
(define-key global-map (kbd "M-v") #'my/scroll-down-half-page)
(define-key global-map (kbd "C-v") #'my/scroll-up-half-page)

;;custom file
(add-to-list 'load-path (expand-file-name "~/.emacs.d/my-custom-files"))
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

;;performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;;editing
(global-set-key (kbd "C-=") 'er/expand-region)
(delete-selection-mode 1)

;;helpful (package)
(setq helpful-max-buffers 2)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

;;indentation
(setq-default indent-tabs-mode nil)
(setq js-indent-level 4)

;;completion
(define-key global-map (kbd "M-/") #'dabbrev-completion)
(define-key global-map (kbd "C-M-/") #'dabbrev-expand)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      orderless-matching-styles '(orderless-flex)
      completion-category-overrides '((file (styles basic partial-completion))))
(setq tab-always-indent 'complete)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(setq corfu-auto t corfu-auto-delay 0.0 corfu-auto-prefix 2 corfu-cycle t)
(global-corfu-mode)
(define-key corfu-map (kbd "C-SPC") #'corfu-insert-separator)
(setq dabbrev-ignored-buffer-regexps '("\\.\\(pdf\\|jpe?g\\|png\\)"))
(setq read-file-name-completion-ignore-case t)
(setq completion-cycle-threshold 3)

;;misc.
(add-hook 'text-mode-hook #'flyspell-mode)
(global-auto-revert-mode 1)
(setq auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(save-place-mode 1)
(electric-pair-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(which-key-mode)
(vertico-mode)
(marginalia-mode)
(setq scroll-conservatively 101)

;;lsp
(setq lsp-completion-provider :none)
(defun my/lsp-mode-setup-completion ()
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless)))
(add-hook 'lsp-completion-mode-hook #'my/lsp-mode-setup-completion)
(add-hook 'python-ts-mode-hook (lambda () (require 'lsp-pyright) (lsp)))
(add-hook 'c-ts-mode-hook #'lsp)
(add-hook 'js-ts-mode-hook #'lsp)

;;treesitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")))
(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (js-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)))
(setq treesit-font-lock-level 4)

;;pdf
(pdf-loader-install)
(setq pdf-annot-default-annotation-properties `((t (label . ,user-full-name))
    (text (icon . "Note")
          (color . "#98fb98"))
    (highlight (color . "PaleTurquoise1"))
    (squiggly (color . "orange"))
    (strike-out(color . "red"))
    (underline (color . "blue"))))

;;org
(setq org-startup-folded 'nofold)
;;(setq org-hide-block-startup t)
(setq org-startup-with-inline-images t)
(setq org-noter-auto-save-last-location t)
(setq org-noter-notes-search-path '("~/Documents/Org"))
(require 'org-download)
(setq org-clock-sound "/usr/lib/libreoffice/share/gallery/sounds/apert.wav")
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
(add-hook 'org-mode-hook #'olivetti-mode)


;;whitespace
(setq sentence-end-double-space nil)
(setq-default require-final-newline t)

;;UI
;;(global-tab-line-mode 1)
(setq-default indicate-empty-lines t)
(global-visual-line-mode 1)
(blink-cursor-mode 0)
(add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode 1)
                            (setq display-line-numbers-type 'relative)
                            (display-line-numbers-mode t)
                            (hl-line-mode 1)))
(defalias 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(load-theme 'modus-vivendi-tritanopia t)
;;main typeface
(set-face-attribute 'default nil :family "JetBrains Mono" :height 100)
;; Proportionately spaced typeface
(set-face-attribute 'variable-pitch nil :family "Noto Sans" :height 1.0)
;; Monospaced typeface
(set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 1.0)
(setq bookmark-set-fringe-mark nil)
;;display the number of matches in a search
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format "   (%s/%s)")

;;consult
(define-key global-map (kbd "C-c r") #'consult-recent-file)
(define-key global-map (kbd "C-x b") #'consult-buffer)

;;pdf
(with-eval-after-load "pdf-view" (define-key pdf-view-mode-map (kbd "c") #'pdf-annot-add-highlight-markup-annotation))


;;autosave & backup
(setq make-backup-files nil
      auto-save-mode -1
      auto-save-default nil
      create-lockfiles nil)

;;denote
(setq denote-directory (expand-file-name "~/Documents/Org/"))
(setq denote-known-keywords '("emacs" "elisp" "linux" "german" "noter" "techtip" "video" "vim"))
(defun denote-journal ()
  (interactive)
  (denote
   nil
   '("journal")))

;;mpv chapter navigation
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

(defun my/scroll-down-in-place (n)
  (interactive "p")
  ;;(previous-line 2)
  (unless (eq (window-start) (point-min))
    (scroll-down n)))

(defun my/scroll-up-in-place (n)
  (interactive "p")
  ;;(next-line 2)
  (unless (eq (window-end) (point-max))
    (scroll-up n)))

(defun my/scroll-up-half-page ()
  "scroll down half a page while keeping the cursor centered"
  (interactive)
  (let ((ln (line-number-at-pos (point)))
    (lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) (move-to-window-line nil))
      ((= ln lmax) (recenter (window-end)))
      (t (progn
           (move-to-window-line -1)
           (recenter))))))

(defun my/scroll-down-half-page ()
  "scroll up half a page while keeping the cursor centered"
  (interactive)
  (let ((ln (line-number-at-pos (point)))
    (lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) nil)
      ((= ln lmax) (move-to-window-line nil))
      (t (progn
           (move-to-window-line 0)
           (recenter))))))

(put 'narrow-to-region 'disabled nil)
