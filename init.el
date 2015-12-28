;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq gc-cons-threshold (* 16 1024 1024))
(load "~/.emacs.d/common.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu 0.1)
 '(ac-auto-start 4)
 '(ac-menu-height 12)
 '(ac-quick-help-delay 0.5)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "green3" "yellow3" "cyan" "magenta3" "cyan3" "gray90"])
 '(blink-matching-delay 0.1)
 '(bm-cycle-all-buffers nil)
 '(bm-repository-file "~/.bm-repository")
 '(browse-url-browser-function (quote browse-url-chromium))
 '(completion-category-overrides (quote ((buffer (styles basic substring)))))
 '(cua-normal-cursor-color "black")
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" default)))
 '(default-input-method "japanese")
 '(dired-after-readin-hook nil)
 '(dired-auto-revert-buffer t)
 '(dired-listing-switches "-ahl")
 '(dired-no-confirm (quote (byte-compile copy)))
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(enable-recursive-minibuffers nil)
 '(fci-rule-color "#2e2e2e")
 '(fstar-enabled-modules (quote (font-lock indentation comments interactive)))
 '(fstar-executable "fstar.exe")
 '(google-translate-default-source-language "en")
 '(google-translate-default-target-language "ja")
 '(grep-command "grep -nH -rI -e ")
 '(guide-key/popup-window-position (quote right))
 '(haskell-indent-offset 2)
 '(history-delete-duplicates t)
 '(history-length 3000)
 '(icomplete-prospects-height 3)
 '(icomplete-show-matches-on-no-input t)
 '(ido-completion-buffer-all-completions t)
 '(ido-max-file-prompt-width 1)
 '(ido-max-prospects 0)
 '(indent-tabs-mode nil)
 '(js2-basic-offset 2)
 '(magit-push-always-verify t)
 '(ocamlspot-command "~/Dropbox/prog/ocamlspot.opt")
 '(org-agenda-export-html-style nil)
 '(org-agenda-files nil)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (dot . t) (ocaml . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")
 '(org-export-backends (quote (ascii beamer html icalendar latex)))
 '(org-html-html5-fancy t)
 '(org-html-htmlize-output-type (quote css))
 '(org-html-indent nil)
 '(org-latex-pdf-process
   (quote
    ("pdflatex -interaction nonstopmode -output-directory %o %f" "pdflatex -interaction nonstopmode -output-directory %o %f" "pdflatex -interaction nonstopmode -output-directory %o %f" "org_dvi_name=%f; platex -interaction nonstopmode -shell-escape %f && dvipdfmx ${org_dvi_name%.tex}.dvi")))
 '(package-selected-packages
   (quote
    (zencoding-mode wgrep w3m viewer tuareg tern-auto-complete sml-mode runner redo+ recentf-ext real-auto-save point-undo org-present nodejs-repl minibuf-isearch markdown-mode magit jedi image+ htmlize helm guide-key google-translate git-gutter fm flymake-jslint expand-region esup disaster color-moccur cdlatex c-eldoc bm autopair auto-complete-auctex auctex-latexmk anzu ag ace-jump-mode ac-math ac-js2 ac-ispell)))
 '(popwin:popup-window-height 0.5)
 '(popwin:special-display-config
   (quote
    (("*git-guitter:diff*" :regexp nil)
     ("*bm-bookmarks*" :regexp nil)
     ("*Find*")
     ("*grep*")
     ("*Miniedit Help*" :noselect t)
     (help-mode)
     (completion-list-mode :noselect t)
     (compilation-mode :noselect t)
     (grep-mode :noselect t)
     (occur-mode :noselect t)
     ("*Pp Macroexpand Output*" :noselect t)
     ("*Shell Command Output*")
     ("*vc-diff*")
     ("*vc-change-log*")
     (" *undo-tree*" :width 60 :position right)
     ("^\\*anything.*\\*$" :regexp t)
     ("*slime-apropos*")
     ("*slime-macroexpansion*")
     ("*slime-description*")
     ("*slime-compilation*" :noselect t)
     ("*slime-xref*")
     (sldb-mode :stick t)
     (slime-repl-mode)
     (slime-connection-list-mode)
     ("*eww*" :regexp nil)
     ("*w3m*" :regexp nil)
     ("*Buffer List*" :regexp nil))))
 '(python-indent-offset 2)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(search-web-default-browser (quote eww-browse-url))
 '(search-web-engines
   (quote
    (("sitepoint" "http://reference.sitepoint.com/?s=%s" nil)
     ("google" "http://www.google.com/search?q=%s" nil)
     ("google ja" "http://www.google.com/search?hl=ja&q=%s" nil)
     ("google en" "http://www.google.com/search?hl=en&q=%s" nil)
     ("google maps" "http://maps.google.co.jp/maps?hl=ja&q=%s" External)
     ("google scholar" "https://scholar.google.co.jp/scholar?q=%s" nil)
     ("youtube" "http://www.youtube.com/results?search_type=&search_query=%s&aq=f" External)
     ("twitter" "https://twitter.com/search?q=%s" External)
     ("goo" "http://dictionary.goo.ne.jp/srch/all/%s/m0u/" nil)
     ("answers" "http://www.answers.com/topic/%s" nil)
     ("emacswiki" "http://www.google.com/cse?cx=004774160799092323420%%3A6-ff2s0o6yi&q=%s&sa=Search" nil)
     ("eijiro" "http://eow.alc.co.jp/%s/UTF-8/" In-Emacs)
     ("cinii" "http://ci.nii.ac.jp/search?q=%s" nil)
     ("amazon" "http://www.amazon.com/s/url=search-alias%%3Daps&field-keywords=%s" External)
     ("amazon jp" "http://www.amazon.co.jp/gp/search?index=blended&field-keywords=%s" External)
     ("yahoo" "http://search.yahoo.com/search?p=%s" nil)
     ("yahoo jp" "http://search.yahoo.co.jp/search?p=%s" nil)
     ("wikipedia en" "http://www.wikipedia.org/search-redirect.php?search=%s&language=en" nil)
     ("wikipedia ja" "http://www.wikipedia.org/search-redirect.php?search=%s&language=ja" nil)
     ("stackoveflow en" "http://stackoverflow.com/search?q=%s" nil)
     ("stackoveflow ja" "http://ja.stackoverflow.com/search?q=%s" nil))))
 '(search-web-external-browser (quote browse-url-chromium))
 '(search-web-in-emacs-browser (quote eww-browse-url))
 '(sml-compile-commands-alist
   (quote
    (("CMB.make()" . "all-files.cm")
     ("CMB.make()" . "pathconfig")
     ("CM.make \"sources.cm\"" . "sources.cm")
     ("use \"load-all\"" . "load-all"))))
 '(sml-indent-args 2)
 '(sml-indent-level 2)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#3b3b3b")
 '(vc-annotate-color-map
   (quote
    ((20 . "#dd5542")
     (40 . "#CC5542")
     (60 . "#fb8512")
     (80 . "#baba36")
     (100 . "#bdbc61")
     (120 . "#7d7c61")
     (140 . "#6abd50")
     (160 . "#6aaf50")
     (180 . "#6aa350")
     (200 . "#6a9550")
     (220 . "#6a8550")
     (240 . "#6a7550")
     (260 . "#9b55c3")
     (280 . "#6CA0A3")
     (300 . "#528fd1")
     (320 . "#5180b3")
     (340 . "#6380b3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(w3m-cookie-accept-bad-cookies (quote ask)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bm-face ((t (:background "chartreuse" :foreground "black"))))
 '(bm-fringe-face ((t (:background "DarkOrange1" :foreground "tan"))))
 '(bm-fringe-persistent-face ((t (:background "chartreuse" :foreground "White"))))
 '(bm-persistent-face ((t (:background "RoyalBlue4" :foreground "White"))))
 '(completions-common-part ((t (:background "black" :foreground "white smoke" :slant normal :weight normal :height 81 :width normal))))
 '(cursor ((t (:background "tan2"))))
 '(icomplete-first-match ((t (:background "orange red" :weight bold))))
 '(ido-first-match ((t (:foreground "light coral" :weight bold :height 1.1))))
 '(isearch ((t (:background "sienna" :foreground "gray"))))
 '(lazy-highlight ((t (:background "dark red" :foreground "light gray"))))
 '(merlin-type-face ((t (:inherit hi-blue))))
 '(mode-line ((t (:background "black" :foreground "firebrick"))))
 '(ocaml-help-face ((t (:background "dark green"))))
 '(show-paren-match ((t (:background "dark red"))))
 '(tuareg-font-lock-governing-face ((t (:foreground "orange red" :weight bold))))
 '(tuareg-font-lock-operator-face ((t (:foreground "dark khaki")))))
