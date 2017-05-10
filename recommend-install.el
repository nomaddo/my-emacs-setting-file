(when (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))

(package-refresh-contents)

(setq initial-recommend-packages
      '(tuareg magit git-gutter markdown-mode auto-complete
	       expand-region bm popup popwin redo+ viewer
	       ace-jump-mode anzu wgrep point-undo
	       image+ auctex auctex-latexmk fm 
	       recentf-ext guide-key w3m smartparens minibuf-isearch))

(mapc 'package-install initial-recommend-packages)
