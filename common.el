;;; package.elの初期化
(when (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))

(require 'em-glob)
(require 'cl)

;;; startup画面を消す
(setq inhibit-startup-screen t)

;;; elisp
(add-to-list 'load-path "~/.emacs.d/elisp")

;;; elispファイルの中のファイルをすべてload-pathに入れる
(let* ((files (eshell-extended-glob "~/.emacs.d/elisp/*"))
       (dirs
        (loop for item in files
              if (file-directory-p item)
              collect item)))
  (mapcar '(lambda (item) (add-to-list 'load-path item)) dirs))

;;; my-original-prefix-key-map
;;; 新しいキーマップの定義
(defvar my-original-map
  (make-sparse-keymap) "My original keymap binded to M-o.")
(defalias 'my-original-prefix my-original-map)
(define-key global-map (kbd "M-o") 'my-original-prefix)

;;; (yes/no) を (y/n)に
(fset 'yes-or-no-p 'y-or-n-p)

;;; opamがインストールされていればopamで入れた拡張をロード
(when (executable-find "opam")
  (setq opam-share (substring
    (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
  (require 'merlin nil t)
  (require 'utop nil t)
  (require 'ocp-indent nil t))

;;; fontの設定
(set-face-attribute 'default nil
  :height 120 ;; font size
  :family "Ricty Diminished")
;;; 日本語fontの設定
;; (set-fontset-font nil 'japanese-jisx0208
;;   (font-spec :family "IPAゴシック"))

;;; migemo
;;; http://rubikitch.com/2014/08/20/migemo/
(when (require 'migemo nil t)
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  ;; Set your installed path
  (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
 
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init)
  ;; migemoがなんかキーバインドを書きかえやがる
  (define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)
  )

;;; guide-key
;;; http://www.kaichan.info/blog/2012-12-03-emacs-advent-calendar-2012-03.html
(when (require 'guide-key)
  (setq guide-key/guide-key-sequence
        '("C-x r" "C-x 4" "C-c" "M-s" "M-s h" "C-z" "M-o" "M-o g"))
  (setq guide-key/idle-delay 0.1)
  (defun guide-key-for-tuareg-mode ()
    (guide-key/add-local-guide-key-sequence "C-c .")
    (guide-key/add-local-highlight-command-regexp "tuareg-"))
  (add-hook 'tuareg-mode-hook 'guide-key-for-tuareg-mode)
  (guide-key-mode 1)  ; Enable guide-key-mode
)

;;; ffap find-fileを強化
;;; urlやファイルの前でfind-fileすると
;;; その名前が存在すれば入力済みになる
(ffap-bindings)

;;; icomplete-mode
;;; M-xなどで候補を表示する
;; (icomplete-mode t)

;;; autopair
;; (when (require 'autopair)
;;   (autopair-global-mode))

;; smartparen
(require 'smartparens)
(smartparens-global-mode)

;; ido-switch-buffer
;; C-x bを強化する
(ido-mode)

;;; iswitch-buffer buffer切り替えを強化
;;; C-r, C-sで候補選択ができる
;;; obsoleteなのでいつか標準じゃなくなると思う…
;; (iswitchb-mode 1)
; 新しいバッファを作成するときに聞いてこない
;; (setq iswitch-prompt-newbuffer nil)

;;; 履歴を次回Emacs起動時にも保存する
(savehist-mode 1)

;;; 時刻の表示
(display-time)

;;; logの記録桁数を増やす
(setq message-log-max 10000)
(setq history-length 1000)

;;; キーストロークをエコーエリアに早く表示する
(setq echo-keystrokes 0.1)

;;; インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)

;;; diredの設定
;;; diredの説明自体は
;;; http://d.hatena.ne.jp/kakurasan/20070702/p1 などを参照してください
(when (require 'dired)
  ;; recursiveな操作をいちいち聞いて来ない
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)

  ;; copyを便利にする
  ;; 詳しくは http://keens.github.io/blog/2013/10/04/emacs-dired/
  (setq dired-dwim-target t)

  ;; rでファイル名変更モードに成る
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

  (define-key dired-mode-map (kbd "C-c C-c") `compile)
  ;; dired の上でもj,kで上下移動する
  (define-key dired-mode-map (kbd "j") `dired-next-line)
  (define-key dired-mode-map (kbd "k") `dired-previous-line)

  ;; dired上で簡単にファイル名などをrenameできるようにする
  ;; http://www.bookshelf.jp/soft/meadow_25.html#SEC298
  (require 'wdired)
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))

;;; dired上で表示するディレクトリやファイルの絞り込みを行うための拡張
;;; - / /で絞り込みリセット
;;; - / dでディレクトリのみ表示
;;; - / .で拡張子で絞り込み
;;; - / rで正規表現で絞り込み
;;;
;;; http://rubikitch.com/2015/04/07/dired-filter-2/
(when (require 'dired-filter nil t)
  (add-hook 'dired-mode-hook
            '(lambda () (dired-filter-mode t))))

;;; C-wはkill-regionでリージョンをカットするコマンドだが、
;;; リージョンが選択されていない時には後ろの1ワードを削除するコマンドになる
(defun kill-region-or-backward-kill-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (point) (mark))
    (backward-kill-word 1)))
(global-set-key (kbd "C-w") 'kill-region-or-backward-kill-word)

;;; 選択範囲に色を付ける
(setq-default transient-mark-mode t)

;;; file名の補完で大文字と小文字を区別しない
(setq completion-ignore-case t)

;;; 行末のスペースを保存するときに削除する
;;; ただしmarkdownではやられると困るのでgfm-modeではやらない
(add-hook 'before-save-hook
          (lambda ()
            (if (!= major-mode `gfm-mode)
                (delete-trailing-whitespace)
              )))

;;; 分割した画面間をShift+矢印で移動
(setq windmove-wrap-around t)
(windmove-default-keybindings)

;;; 最近開いたファイルを追加
;;; http://d.hatena.ne.jp/tomoya/20110217/1297928222
(setq recentf-max-menu-items 1000)
(setq recentf-max-saved-items 1000)
(setq recentf-auto-save-timer
      (run-with-idle-timer 30 t 'recentf-save-list))
(require `recentf-ext)

;;; cua-mode
;;; http://tech.kayac.com/archive/emacs-rectangle.html
(setq cua-enable-cua-keys nil)
(cua-mode t)

;;; 行間
(setq-default line-spacing 0)

;;; tool-bar を非表示にする
(tool-bar-mode -1)

;;; 括弧の範囲内を強調表示
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

;;; 行と桁の表示
(line-number-mode t)
(column-number-mode t)

;;; 選択範囲の情報表示
(defun count-lines-and-chars ()
  (if mark-active
      (format "[%3d:%4d]"
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ""))
(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))

;; 選択が重くならないように
;; (setq select-active-regions nil)

;;; bm.elの設定
;;; ファイルに印をつけて、印の間を移動するためのモード
;;; 印の行はハイライトされるので見やすい
;;; 今の設定だと、M-mで印をつけ、M-,で前の印に移動、M-.で次の印に移動する
;;; Emacsを終了しても印は残る！

;;; 永続化 http://rubikitch.com/?s=bm&x=0&y=0
(when (require 'bm)
  (defun bm-find-files-in-repository ()
    (interactive)
    (cl-loop for (key . _) in bm-repository
             when (file-exists-p key)
             do (find-file-noselect key)))

  (defun bm-repository-load-and-open ()
    (interactive)
    (bm-repository-load)
    (bm-find-files-in-repository))

  ;; (setq bm-repository-file "~/.emacs.d/bm-repository")
  (setq-default bm-buffer-persistence t)
  (setq bm-restore-repository-on-load t)
  (add-hook 'after-init-hook 'bm-repository-load-and-open)

  (defun bm-buffer-restore-safe ()
    (ignore-errors (bm-buffer-restore)))

  (add-hook 'find-file-hooks 'bm-buffer-restore-safe)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)

  (defun bm-save-to-repository ()
    (interactive)
    (unless noninteractive
      (bm-buffer-save-all)
      (bm-repository-save)))

  (add-hook 'kill-emacs-hook 'bm-save-to-repository)
  (run-with-idle-timer 60 t 'bm-save-to-repository)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  (add-hook 'before-save-hook 'bm-buffer-save)

  (define-key global-map (kbd "M-m") 'bm-toggle)
  (define-key global-map (kbd "M-,") 'bm-previous)
  (define-key global-map (kbd "M-.") 'bm-next)
  (define-key my-original-map (kbd "l") 'bm-show-all)
  )

;;; view-mode
;;; 閲覧専用モード
;;; emacsでテキストを見ていると、うっかり文字を入力してしまい
;;; コンパイル時まで気づかない、みたいなことが起きる。
;;; それを防ぐためにread-only-modeというものもある
;;; それを拡張し、キーバインドも読み込み専用のものにすると
;;; Ctrlをいっぱい押さずに済んで便利である
(setq view-read-only t)
(add-hook 'help-mode-hook 'view-mode)
;; (add-hook 'find-file-hook 'view-mode)

(require 'view)
;; terminalと同じような操作感にする
(define-key view-mode-map (kbd "h") 'backward-char)
(define-key view-mode-map (kbd "j") 'next-line)
(define-key view-mode-map (kbd "k") 'previous-line)
(define-key view-mode-map (kbd "l") 'forward-char)
(define-key view-mode-map (kbd "J") 'View-scroll-line-forward)
(define-key view-mode-map (kbd "K") 'View-scroll-line-backward)
(define-key view-mode-map (kbd "i") 'recenter-top-bottom)
(define-key view-mode-map (kbd "n") 'cua-scroll-up)
(define-key view-mode-map (kbd "p") 'cua-scroll-down)

(when (require 'viewer nil t)
  (setq viewer-modeline-color-unwritable "tomato")
  (setq viewer-modeline-color-view "orange")
  (viewer-change-modeline-color-setup))

;;; pont-undoの設定
;;; カーソルの位置のリデュ・アンドゥをするための拡張
;;; 非常に便利！
(when (require `point-undo nil t)
  (define-key dired-mode-map (kbd "[") 'point-undo)
  (define-key dired-mode-map (kbd "]") 'point-redo)
  (define-key view-mode-map (kbd "[") 'point-undo)
  (define-key view-mode-map (kbd "]") 'point-redo)
  (define-key global-map (kbd "M-[") 'point-undo)
  (define-key global-map (kbd "M-]") 'point-redo))

;;; ibuffer
(define-key global-map (kbd "C-x C-b") 'ibuffer)

;;; org-mode config
;;; 画像のインライン表示をデフォルトにする

(setq org-startup-with-inline-images t)
(add-hook 'org-mode-hook 'turn-on-iimage-mode)
(setq org-confirm-babel-evaluate nil)

;;; auto-complete.el
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  (setq ac-candidate-limit 15)
  (setq ac-auto-show-menu 0.3)
  (setq ac-auto-start 0)
  (setq ac-candidate-limit 100)
  (setq ac-ignore-case nil)
  (setq ac-menu-height 8))

;;; 括弧の範囲色
;;; #500 は暗い赤、背景が黒いと映える
;;; テーマによっては見づらいことがあるので、
;;; M-x list-colors-display を開いて、色一覧を見て、設定してみてください
(set-face-background 'show-paren-match-face "#500")

;;; redo+
;;; redoは標準にはないので拡張で入れる
(when (require 'redo+ nil t)
  (define-key global-map (kbd "C-?") 'redo))

;;; fm.el installed via elpa
;;; empower occur
;;; when a window is in occur-mode or compilation-mode,
;;; we can jump indivisual lines related to the message by moving the message.
;;; http://www.bookshelf.jp/soft/meadow_47.html#SEC696
(require 'fm)
(add-hook 'occur-mode-hook 'fm-start)
;;(add-hook 'compilation-mode-hook 'fm-start)

(define-key occur-mode-map (kbd "j") 'occur-next)
(define-key occur-mode-map (kbd "k") 'occur-prev)

;; Start merlin on ocaml files
(when (require 'merlin nil t)
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t)

  ;; Enable auto-complete
  (setq merlin-use-auto-complete-mode 'easy)

  ;; Use opam switch to lookup ocamlmerlin binary
  (setq merlin-command 'opam)
  (add-hook
   'tuareg-mode-hook
   '(lambda ()
      (ocp-setup-indent)
      (define-key tuareg-mode-map "\C-c\C-p" 'merlin-pop-stack)
      (local-set-key "\C-ch" 'merlin-pop-stack)
      (defun utop-eval-region-or-phrase ()
        (interactive)
        (if (region-active-p)
            (utop-eval-region (mark) (point))
          (utop-eval-phrase)))
      (define-key tuareg-mode-map "\C-x\C-e" 'utop-eval-region-or-phrase))
   ))

;;; ocamldebug
;;; 標準よりもキーバインドを使いやすくします
(when (require 'ocamldebug nil t)
  (def-ocamldebug "backstep" "\C-b" "Back step one source line with display.")
  (def-ocamldebug "previous" "\C-p" "")
  (define-key ocamldebug-mode-map [(C-up)] 'ocamldebug-up)
  (define-key ocamldebug-mode-map [(C-down)] 'ocamldebug-down)
  (define-key ocamldebug-mode-map [(C-left)] 'ocamldebug-backstep)
  (define-key ocamldebug-mode-map [(C-right)] 'ocamldebug-step)
  (define-key ocamldebug-mode-map [(C-S-left)] 'ocamldebug-previous)
  (define-key ocamldebug-mode-map [(C-S-right)] 'ocamldebug-next))

;;; expand-region
;;; http://d.hatena.ne.jp/syohex/20120117/1326814127
(when (require 'expand-region)
  (global-set-key (kbd "C-@") 'er/expand-region)
  (global-set-key (kbd "C-M-@") 'er/contract-region))

;;; add source for auto-complete
;;; this adds source for math symbol of latex
(require 'ac-math)

;;; auc-latex
(add-to-list 'ac-modes 'latex-mode) ; make auto-complete aware of `latex-mode`
(defun latex-mode-setup () ; add ac-sources to default ac-sources
  (setq ac-sources
        (append
         '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
         ac-sources))
  (add-to-list 'LaTeX-verbatim-environments "lstlisting"))
(add-hook 'latex-mode-hook 'latex-mode-setup)
(add-hook 'plain-tex-mode-hook 'latex-mode-setup)

(setq TeX-default-mode 'japanese-latex-mode)

;;; lookコマンドを英単語の情報源にした英単語の補完を自作する
;;; M-hにバインドしてます
(when (executable-find "look")
  (defun my-ac-look ()
    "look コマンドの出力をリストで返す"
    (interactive)
    (unless (executable-find "look")
      (error "look コマンドがありません"))
    (let ((search-word (thing-at-point 'word)))
      (with-temp-buffer
        (call-process-shell-command "look" nil t 0 "-f" search-word)
        (split-string-and-unquote (buffer-string) "\n"))))

  (defun ac-complete-look ()
    (interactive)
    (let ((ac-menu-height 50)
          (ac-candidate-limit t))
      (auto-complete '(ac-source-look))))

  (defvar ac-source-look
    '((candidates . my-ac-look)
      (requires . 2)))  ;; 2文字以上ある場合にのみ対応させる

  (global-set-key (kbd "M-h") 'ac-complete-look))

;;; popwin
;;; Emacs標準と比較して、激しく挙動を変化させるので
;;; 肌に合わなければコメントアウトしてください
;;; http://valvallow.blogspot.jp/2011/03/emacs-popwinel.html
(when (require 'popwin)
  (popwin-mode 1)
  (global-set-key (kbd "s-v") popwin:keymap)

  (defadvice eww-render (around eww-render-popwin activate)
    (save-window-excursion ad-do-it)
    (unless (get-buffer-window "*eww*")
      (pop-to-buffer "*eww*")))

  (defadvice w3m-browse-url (around w3m-browse-url-popwin activate)
    (save-window-excursion ad-do-it)
    (unless (get-buffer-window "*w3m*")
      (pop-to-buffer "*w3m*")))

  (defadvice find-dired (around find-dired-popwin activate)
    (save-window-excursion ad-do-it)
    (unless (get-buffer-window "*Find*")
      (pop-to-buffer "*Find*")))

  (defadvice bm-show-all (around bm-show-all-popwin activate)
    (save-window-excursion ad-do-it)
    (unless (get-buffer-window "*bm-bookmarks*")
      (pop-to-buffer "*bm-bookmarks*")))
  )



;; 最近出したminibufferをpopupさせる
(define-key popwin:keymap (kbd "j") `popwin:popup-last-buffer)
;; 最近出したminibufferを通常通り表示させる
(define-key popwin:keymap (kbd "k") `popwin:original-pop-to-last-buffer)
;; set the height of popup-buffer
(setq popwin:popup-window-height 0.5)

;; to avoid bug
(setq popwin:close-popup-window-timer-interval 0.05)

;;; google-translate
;;; http://qiita.com/okonomi/items/f18c9221420eca47ebc6
;;; デフォルトの翻訳はC-x tで英語から日本語への翻訳
;;; この言語縛りを外すには、コマンドの前にC-uを押す
(when (require 'google-translate nil t)
  (global-set-key (kbd "C-x t") 'google-translate-at-point)
  (global-set-key (kbd "C-x T") 'google-translate-query-translate)
  ;; 翻訳のデフォルト値を設定（en -> ja）
  (custom-set-variables
   '(google-translate-default-source-language "en")
   '(google-translate-default-target-language "ja")))

;;; ace-jump-mode
;;; http://d.hatena.ne.jp/syohex/20120304/1330822993
;;; C-c SPCで素早くカーソルを移動する(上のリンク参照)
;;; C-x SPCで戻ってくる
(when (require 'ace-jump nil t)
  (autoload
    'ace-jump-mode
    "ace-jump-mode"
    "Emacs quick move minor mode"
    t)
  (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
  (autoload
    'ace-jump-mode-pop-mark
    "ace-jump-mode"
    "Ace jump back:-)"
    t)
  (eval-after-load "ace-jump-mode"
    '(ace-jump-mode-enable-mark-sync))
  (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
  (define-key global-map (kbd "C-c l") 'ace-jump-line-mode)
  (define-key global-map (kbd "C-c w") 'ace-jump-word-mode)
  (require 'shell)
  (define-key shell-mode-map (kbd "C-c SPC") 'ace-jump-mode))

;;; git-gutter
;;; http://qiita.com/syohex/items/a669b35fbbfcdda0cbf2
(when (require 'git-gutter nil t)
  (global-git-gutter-mode t)
  (global-set-key (kbd "M-n") 'git-gutter:next-hunk)
  (global-set-key (kbd "M-p") 'git-gutter:previous-hunk)

  ;; originalのprefixコマンドを作る
  (defvar my-git-map (make-sparse-keymap))
  (defalias 'my-git-prefix my-git-map)

  (define-key global-map (kbd "M-o g") my-git-map)

  (define-key my-git-map (kbd "s") 'git-gutter:set-start-revision)
  (define-key my-git-map (kbd "p") 'git-gutter:popup-hunk)
  (define-key my-git-map (kbd "r") 'git-gutter:revert-hunk)
)

(when (require 'magit nil t)
  (define-key my-original-map (kbd "M-o") 'magit-dispatch-popup)
  (define-key magit-popup-mode-map (kbd "s") `magit-status)
  (setq magit-auto-revert-mode nil)
  )

;;; org-mode
(require 'org)
(setq org-export-latex-date-format "%Y-%m-%d")
(setq org-export-latex-classes nil)
(add-to-list 'org-export-latex-classes
             '("jarticle"
               "\\documentclass[a4j]{jarticle}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
               ))
(add-to-list 'org-export-latex-classes
             '("beamer"
               "\\documentclass[compress,dvipdfm]{beamer}"
               org-beamer-sectioning
               ))
(define-key org-mode-map (kbd "M-i") 'pcomplete)

;;;
;;; markdown-mode
;;; 標準的な markdown では物足りないので，GitHub Flavored Markdown (GFM)
;;; という Github の独自拡張を使う．redcarpet は GFM スタイルのマークダウンを
;;; HTML に変換するツールで，ruby を使って色々拡張することもできる．
;;;

(when (require 'markdown-mode nil t)
  (setq auto-mode-alist (cons '("\\.markdown" . gfm-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.md" . gfm-mode) auto-mode-alist))
  (setq markdown-command "redcarpet")
  (add-hook 'gfm-mode-hook 'flyspell-mode))

;;; image+.el
;;; emacs上で表示する画像の大きさなどを調整するための拡張
(when (require 'image+ nil t)
  (imagex-auto-adjust-mode 1))

;;; anzu
;;; http://qiita.com/syohex/items/56cf3b7f7d9943f7a7ba
(when (require 'anzu nil t)
  (global-anzu-mode +1)
  (setq anzu-mode-lighter "")
  (setq anzu-deactivate-region t)
  (setq anzu-search-threshold 1000)
  (global-set-key (kbd "M-s a") 'anzu-query-replace-at-cursor)
  (global-set-key (kbd "M-s q") 'anzu-query-replace)
  (global-set-key (kbd "M-s r") 'anzu-replace-at-cursor-thing)
)

(setq css-indent-offset 2)

;;; search-web.el
(when (require 'search-web nil t)
  (global-set-key (kbd "M-s s") 'search-web-dwim))

;;; howm
;;; emacs内ローカルwikiツール
;;; メモ書きに便利
;;; (手動で入れてください)
(when (require 'howm nil t)
  (add-to-list 'guide-key/guide-key-sequence "C-c ,")
  (defun howm-my-initial-setup ()
    ;; org-modeを同時に使う！
    (org-mode)
    (howm-mode))
  (add-hook 'howm-create-file-hook 'howm-my-initial-setup)
  (add-hook 'howm-view-open-hook 'howm-my-initial-setup))

;;;
;;; 80 文字のハイライト
;;;
(defun warn-column80 ()
  (font-lock-add-keywords
     nil
     '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t))))

(add-hook 'cc-mode-hook 'warn-column80)
(add-hook 'tuareg-mode-hook 'warn-column80)

;;; jedi
;;; pythonでメソッドなどの保管を行うためのプラグイン
(when (require `jedi nil t)
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)                 ; optional
)

;;; sml-mode
(when (require 'sml-mode nil t)
  (define-key sml-mode-map (kbd "C-c C-c") `compile)
  )

;;; makefile-mode
(add-hook 'makefile-mode-hook
  '(lambda ()
     (local-set-key (kbd "C-c C-c") `compile)
     ))

;; ghc-mod
(add-to-list 'exec-path (concat (getenv "HOME") "/.cabal/bin"))
(add-to-list 'load-path "~/.cabal/share/x86_64-linux-ghc-7.6.3/ghc-mod-5.4.0.0/elisp/")
(autoload 'ghc-init "ghc")

(defun my-ac-haskell-mode ()
  (add-to-list ac-sources '(ac-source-ghc-mod)))

(add-hook 'haskell-mode-hook
          '(lambda ()
             (ghc-init)
             (my-ac-haskell-mode)))
;; (define-key haskell-mode-map (kbd "M-i") 'ghc-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keymapの設定やキーバインドの変更部分
;; ここにまとめて書くこと
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; キーバインドの変更
;;; お好みでどうぞ

(define-key global-map (kbd "C-q") 'read-only-mode)
(define-key global-map (kbd "C-^") `enlarge-window)
(define-key global-map (kbd "C-x i") 'indent-region)

(define-key global-map [(C /)] 'undo)

(define-key global-map [(C x)(h)] 'recentf-open-files)
(define-key global-map [(C x)(C z)] `just-one-space)

(when (require 'popwin nil t)
  (define-key global-map [(C z)] popwin:keymap)
  (add-to-list 'popwin:special-display-config '("*Find*")))

(define-key compilation-mode-map (kbd "j") 'compilation-next-error)
(define-key compilation-mode-map (kbd "k") 'compilation-previous-error)

(define-key global-map (kbd "s-g") `keyboard-quit)

;;; auto-complete-mode start
(define-key my-original-map (kbd "C-a") `auto-complete-mode)

(define-key my-original-map (kbd "d") `delete-other-windows-vertically)
(define-key my-original-map (kbd "d") `delete-other-windows-vertically)

;;; back-space
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key [(C -)] 'help-command)

;;; view-mode
(global-set-key (kbd "C-c C-v") 'view-mode)

;;; auto-complete
(global-set-key (kbd "M-i") 'auto-complete)

;;; enlarge window horizontally
(global-set-key (kbd "C-M-^") 'enlarge-window-horizontally)

;; grep, find
(global-set-key (kbd "C-x g") 'grep)
(global-set-key (kbd "C-x f") 'find-dired)

;; forward-list
(global-set-key (kbd "M-j") 'forward-list)
(global-set-key (kbd "M-k") 'backward-list)

;; font-size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-decrease)

;; customize-group
(global-set-key [f5] 'customize-group)

;; magit-log
(define-key my-original-map (kbd "s") 'magit-log)

;; compile
(define-key global-map (kbd "C-c C-c") 'compile)
