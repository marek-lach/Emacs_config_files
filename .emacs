;; Add MELPA repository:
(when (>= emacs-major-version 27)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   '("org" . "http://orgmode.org/elpa/")))
 
;; Add Quelpa as well:
(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/bootstrap.el")
    (eval-buffer)))

(package-initialize)

;; Defines which modules need to be installed:
(require 'org)
(org-indent-mode)
(require 'olivetti)
  (unless (package-installed-p 'olivetti)
    (package-install 'olivetti))
(require 'org-bullets)
  (unless (package-installed-p 'org-bullets)
    (package-install 'org-bullets))
(require 'org-notebook)
  (unless (package-installed-p 'org-notebook)
    (package-install 'org-notebook))
(require 'persistent-scratch)
  (unless (package-installed-p 'persistent-scratch)
    (package-install 'persistent-scratch))
(require 'org-books)
  (unless (package-installed-p 'org-books)
    (package-install 'org-books))
    
(setq-default major-mode 'org-mode)
    
;; Shortcuts for Org Mode submodules:
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-log-done t)
(setq org-image-actual-width nil)
    
;; Enable the olivetti mode for margins:    
(define-globalized-minor-mode my-global-olivetti-mode olivetti-mode
  (lambda () (olivetti-mode 1)))
(my-global-olivetti-mode 1)

(fringe-mode 0)
(setq org-descriptive-links nil)
(setq company-minimum-prefix-length 2)

(define-key global-map "\C-cl" 'org-store-link)
(setq x-select-enable-clipboard t)
(global-visual-line-mode t)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(defun sa-outline-forward-same-level (arg)
  "If its the last outline sibling, move to the next visible outline
heading."
  (interactive "p")
  (if (save-excursion (outline-get-next-sibling))
      (outline-forward-same-level arg)
    (outline-next-visible-heading arg)))

(defun sa-outline-backward-same-level (arg)
  "If its the last outline sibling, move to the previous visible outline heading."
  (interactive "p")
  (if (save-excursion (outline-get-last-sibling))
      (outline-backward-same-level arg)
    (outline-previous-visible-heading arg)))

(defun sa-org-dwim-next()
  "Move to next item or headline. If at an item move to the next item
otherwise move to next headline."
  (interactive)
  (unless (eq major-mode 'org-mode) (error "Not an `org-mode' buffer"))
  (if (org-in-item-p)
      (if (eq t (condition-case nil (org-next-item)
                  (error t)))
	  (outline-next-visible-heading 1))
    (outline-next-visible-heading 1)))

(defun sa-org-dwim-previous()
  "Move to next item or headline. If at an item move to the next item
otherwise move to next headline."
  (interactive)
  (unless (eq major-mode 'org-mode) (error "Not an `org-mode' buffer"))
  (if (org-in-item-p)
      (if (eq t (condition-case nil (org-previous-item)
	          (error t)))
	  (outline-previous-visible-heading 1))
(outline-previous-visible-heading 1)))

(defun org-transpose-paragraphs (arg)
 (interactive)
 (when (and (not (or (org-at-table-p) (org-on-heading-p) (org-at-item-p)))
            (thing-at-point 'sentence))
   (re-search-forward "[[:graph:]]")
   (goto-char (match-beginning 0))
   t))

(setq org-todo-keywords
      (quote ((sequence "TODO(t!)"  "NEXT(n!)" "|" "DONE(d!)")
	      (sequence "REPEAT(r)"  "WAIT(w!)"  "|"  "PAUSED(p@/!)" "CANCELLED(c@/!)" )
	      (sequence "IDEA(i!)" "MAYBE(y!)" "STAGED(s!)" "WORKING(k!)" "|" "USED(u!/@)")
)))

(add-to-list 'org-metaup-hook
 (lambda () (interactive) (org-transpose-paragraphs -1)))
(add-to-list 'org-metadown-hook
 (lambda () (interactive) (org-transpose-paragraphs 1)))

(defun org-word-count (beg end
                           &optional count-latex-macro-args?
                           count-footnotes)
  "Report the number of words in the Org mode buffer or selected region.
COUNT-FOOTNOTES? is non-nil.

If the optional argument COUNT-LATEX-MACRO-ARGS? is non-nil, the word count
includes LaTeX macro arguments (the material between {curly braces}).
Otherwise, and by default, every LaTeX macro counts as 1 word regardless
of its arguments."
  (interactive "r")
  (unless mark-active
    (setf beg (point-min)
          end (point-max)))
  (let ((wc 0)
        (latex-macro-regexp "\\\\[A-Za-z]+\\(\\[[^]]*\\]\\|\\){\\([^}]*\\)}"))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (cond
         ;; Ignore source code blocks.
         ((org-in-regexps-block-p "^#\\+BEGIN_SRC\\W" "^#\\+END_SRC\\W")
          nil)
         ;; Ignore inline source blocks, counting them as 1 word.
         ((save-excursion
            (backward-char)
            (looking-at org-babel-inline-src-block-regexp))
          (goto-char (match-end 0))
          (setf wc (+ 2 wc)))
         ;; Count latex macros as 1 word, ignoring their arguments.
         ((save-excursion
            (backward-char)
            (looking-at latex-macro-regexp))
          (goto-char (if count-latex-macro-args?
                         (match-beginning 2)
                       (match-end 0)))
          (setf wc (+ 2 wc)))
         ;; Ignore footnotes.
         ((and (not count-footnotes?)
               (or (org-footnote-at-definition-p)
                   (org-footnote-at-reference-p)))
          nil)
         (t
          (let ((contexts (org-context)))
            (cond
             ;; Ignore tags and TODO keywords, etc.
             ((or (assoc :todo-keyword contexts)
                  (assoc :priority contexts)
                  (assoc :keyword contexts)
                  (assoc :checkbox contexts))
              nil)
             ;; Ignore sections marked with tags that are
             ;; excluded from export.
             ((assoc :tags contexts)
              (if (intersection (org-get-tags-at) org-export-exclude-tags
                                :test 'equal)
                  (org-forward-same-level 1)
                nil))
             (t
              (incf wc))))))
        (re-search-forward "\\w+\\W*")))
    (message (format "%d words in %s." wc
                     (if mark-active "region" "buffer")))))

(defun org-point-at-end-of-empty-headline ()
  "If point is at the end of an empty headline, return t, else nil."
  (and (looking-at "[ \t]*$")
       (save-excursion
         (beginning-of-line 1)
         (looking-at (concat "^\\(\\*+\\)[ \t]+\\(" org-todo-regexp "\\)?[
\t]*")))))

(defun org-level-increment ()
  "Return the number of stars that will be added or removed at a
time to headlines when structure editing, based on the value of
`org-odd-levels-only'."
  (if org-odd-levels-only 2 1))

(defvar org-previous-line-level-cached nil)

(defun org-recalculate-previous-line-level ()
  "Same as `org-get-previous-line-level', but does not use cached
value. It does *set* the cached value, though."
  (set 'org-previous-line-level-cached
       (let ((current-level (org-current-level))
             (prev-level (when (> (line-number-at-pos) 1)
                           (save-excursion
                             (previous-line)
                             (org-current-level)))))
         (cond ((null current-level) nil) ; Before first headline
               ((null prev-level) 0)      ; At first headline
               (prev-level)))))

(defun org-get-previous-line-level ()
  "Return the outline depth of the last headline before the
current line. Returns 0 for the first headline in the buffer, and
nil if before the first headline."
  ;; This calculation is quite expensive, with all the regex searching
  ;; and stuff. Since org-cycle-level won't change lines, we can reuse
  ;; the last value of this command.
  (or (and (eq last-command 'org-cycle-level)
           org-previous-line-level-cached)
      (org-recalculate-previous-line-level)))

(defun org-cycle-level ()
  (interactive)
  (let ((org-adapt-indentation nil))
    (when (org-point-at-end-of-empty-headline)
      (setq this-command 'org-cycle-level) ;Only needed for caching
      (let ((cur-level (org-current-level))
            (prev-level (org-get-previous-line-level)))
        (cond
         ;; If first headline in file, promote to top-level.
         ((= prev-level 0)
          (loop repeat (/ (- cur-level 1) (org-level-increment))
                do (org-do-promote)))
         ;; If same level as prev, demote one.
         ((= prev-level cur-level)
          (org-do-demote))
         ;; If parent is top-level, promote to top level if not already.
         ((= prev-level 1)
          (loop repeat (/ (- cur-level 1) (org-level-increment))
                do (org-do-promote)))
         ;; If top-level, return to prev-level.
         ((= cur-level 1)
          (loop repeat (/ (- prev-level 1) (org-level-increment))
                do (org-do-demote)))
         ;; If less than prev-level, promote one.
         ((< cur-level prev-level)
          (org-do-promote))
         ;; If deeper than prev-level, promote until higher than
         ;; prev-level.
         ((> cur-level prev-level)
          (loop repeat (+ 1 (/ (- cur-level prev-level)
(org-level-increment)))
                do (org-do-promote))))
        t))))

;; Identity
(setq user-full-name "Marek Lach")
(setq user-mail-address "mareklach@outlook.com")

;; Disable welcome message at startup
(setq inhibit-startup-message t)

;; Disable graphical controls of the editor
(menu-bar-mode -1)
(tool-bar-mode -1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(inkpot))
 '(custom-safe-themes
   '("392103647f87afe03ce2e4e1d99c8b840c69dfa267e7d68382d470c047b5c181" "318317bf0188781fc9852633a8fa55b3ade2d01694643f21c2ce75597f277dfc" "c0429683ff6ea4bed2f3d5a1c94717dff22d4f52ec1f5ebaf67dfab5d78787f1" "60d4556ebff0dc94849f177b85dcb6956fe9bd394c18a37e339c0fcd7c83e4a9" default))
 '(initial-frame-alist '((fullscreen . maximized)))
 '(package-selected-packages
   '(ocamlformat merlin inkpot-theme flyspell-correct-ivy flyspell-correct-helm flyspell-correct olivetti org-bullets org-books org-notebook persistent-scratch html2org highlight-context-line highlight-current-line autothemer rudel ace-jump-mode ace-jump-zap ace-link ac-ispell windresize rich-minority myers xclip scroll-restore nlinum on-screen pabbrev compact-docstrings jumpc auto-overlays loc-changes loccur ace-window context-coloring delight org outorg tablist fountain-mode caseformat beacon avy all org-wc org-tree-slide org-transform-tree-table org-seek org-ref org-pdfview org-linkany org-if org-grep org-gnome org-edit-latex org-cua-dwim org-context org-cliplink org-bullets org-beautify-theme org-autolist org-ac flyspell-popup flyspell-correct-popup define-word buffer-manage broadcast bracketed-paste bpr bookmark+ bibtex-utils bibretrieve avk-emacs-themes autopair autofit-frame autobookmarks auto-save-buffers-enhanced auto-package-update auto-org-md auto-dim-other-buffers auto-complete-chunk ac-html))
 '(show-paren-mode t))
 
 ;; scratch mode settings
(persistent-scratch-setup-default)

(require 'org-inlinetask)

(defun scimax/org-return (&optional ignore)
  "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
  (interactive "P")
  (if ignore
      (org-return)
    (cond

     ((eq 'line-break (car (org-element-context)))
      (org-return-indent))

     ;; Open links like usual, unless point is at the end of a line.
     ;; and if at beginning of line, just press enter.
     ((or (and (eq 'link (car (org-element-context))) (not (eolp)))
          (bolp))
      (org-return))

     ;; It doesn't make sense to add headings in inline tasks. Thanks Anders
     ;; Johansson!
     ((org-inlinetask-in-task-p)
      (org-return))

     ;; checkboxes too
     ((org-at-item-checkbox-p)
      (org-insert-todo-heading nil))

     ;; lists end with two blank lines, so we need to make sure we are also not
     ;; at the beginning of a line to avoid a loop where a new entry gets
     ;; created with only one blank line.
     ((org-in-item-p)
      (if (save-excursion (beginning-of-line) (org-element-property
:contents-begin (org-element-context)))
          (org-insert-heading)
        (beginning-of-line)
        (delete-region (line-beginning-position) (line-end-position))
        (org-return)))

     ;; org-heading
     ((org-at-heading-p)
      (if (not (string= "" (org-element-property :title
(org-element-context))))
          (progn (org-end-of-meta-data)
                 (org-insert-heading-respect-content)
                 (outline-show-entry))
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")))

     ;; tables
     ((org-at-table-p)
      (if (-any?
           (lambda (x) (not (string= "" x)))
           (nth
            (- (org-table-current-dline) 1)
            (org-table-to-lisp)))
          (org-return)
        ;; empty row
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")
        (org-return)))

     ;; fall-through case
     (t
      (org-return)))))

(define-key org-mode-map (kbd "RET")
  'scimax/org-return)

;; packages
;; mode specific

;; config files
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

;; spell checker
(setq flyspell-issue-welcome-flag nil)
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Coming Soon" :foundry "BROS" :slant normal :weight normal :height 132 :width normal)))))
