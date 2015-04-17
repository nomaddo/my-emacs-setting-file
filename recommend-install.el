(when (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))

(package-refresh-contents)

(setq initial-recommend-packages
      '(tuareg magit git-gutter markdown-mode auto-complete
	       expand-region bm google-translate popup popwin redo+ viewer
	       ace-jump-mode anzu sml-mode wgrep point-undo
	       org-present image+ ac-math c-eldoc auctex auctex-latexmk fm
               recentf-ext guide-key w3m autopair minibuf-isearch search-web))

(mapc 'package-install initial-recommend-packages)
