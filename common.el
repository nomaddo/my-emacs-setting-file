;;; package.elの初期化
(when (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))

;;; elisp
(add-to-list 'load-path "~/.emacs.d/elisp")

;;; my-original-prefix-key-map
;;; 新しいキーマップの定義
(defvar my-original-map
  (make-sparse-keymap) "My original keymap binded to s-c.")
(defalias 'my-original-prefix my-original-map)
(define-key global-map (kbd "s-c") 'my-original-prefix)

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
  :height 130
  :family "IPAゴシック") ;; font size

;;; guide-key
(when (require 'guide-key nil t)
  (setq guide-key/guide-key-sequence
        '("C-x r" "C-x 4" "C-c"))
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

;;; iswitch-buffer buffer切り替えを強化
;;; C-r, C-sで候補選択ができる
;;; obsoleteなのでいつか標準じゃなくなると思う…
(iswitchb-mode 1)
; 新しいバッファを作成するときに聞いてこない
(setq iswitch-prompt-newbuffer nil)

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
;;; 詳しくは、
;;; http://keens.github.io/blog/2013/10/04/emacs-dired/
;;; diredの説明自体は
;;; http://d.hatena.ne.jp/kakurasan/20070702/p1 などを参照してください
(require 'dired)
(setq dired-dwim-target t)

(add-hook 'dired-load-hook
  (lambda ()
    (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)))
;;; dired の上でもj,kで上下移動する
(define-key dired-mode-map (kbd "j") `dired-next-line)
(define-key dired-mode-map (kbd "k") `dired-previous-line)

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
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; 全角スペースを黄色でハイライト
(global-hi-lock-mode 1)
(highlight-phrase "　")

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
;;; cua-modeの説明
;;; http://tech.kayac.com/archive/emacs-rectangle.html
(setq cua-enable-cua-keys nil)
(cua-mode t)

;;; 行間
(setq-default line-spacing 0)

;;; tool-bar を非表示にする
(tool-bar-mode nil)

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
(when (require 'bm nil t)
  (setq-default bm-buffer-persistence nil)
  (setq bm-restore-repository-on-load t)
  (add-hook 'find-file-hook 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save)))
  (define-key global-map (kbd "M-m") 'bm-toggle)
  (define-key global-map (kbd "M-,") 'bm-previous)
  (define-key global-map (kbd "M-.") 'bm-next)
  (custom-set-faces
   '(bm-face ((t (:background "chartreuse" :foreground "black"))))
   '(bm-fringe-face ((t (:background "DarkOrange1" :foreground "tan"))))))

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


;; F11 フルスクリーン
(defun fullboth-screen ()
  (interactive)
  (let ((fullscreen (frame-parameter (selected-frame) 'fullscreen)))
    (cond
     ((null fullscreen)
      (set-frame-parameter (selected-frame) 'fullscreen 'fullboth))
     (t
      (set-frame-parameter (selected-frame) 'fullscreen 'nil))))
  (redisplay))

(global-set-key [f11] 'fullboth-screen)

;; F12 最大化
(defun maximized-screen ()
  (interactive)
  (let ((fullscreen (frame-parameter (selected-frame) 'fullscreen)))
    (cond
     ((null fullscreen)
      (set-frame-parameter (selected-frame) 'fullscreen 'maximized))
     (t
      (set-frame-parameter (selected-frame) 'fullscreen 'nil))))
  (redisplay))

(global-set-key [f12] 'maximized-screen)

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


;;; org-mode config
; 画像のインライン表示をデフォルトにする
(setq org-startup-with-inline-images t)
(add-hook 'org-mode-hook 'turn-on-iimage-mode)

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

;;; c-eldoc
(when (require 'c-eldoc nil t)
      (add-hook 'c-mode-hook
         (lambda ()
            (set (make-local-variable 'eldoc-idle-delay) 0.20)
     (c-turn-on-eldoc-mode))))

;;; 括弧の範囲色
;;; #500 は暗い赤、背景が黒いと映える
;;; テーマによっては見づらいことがあるので、
;;; M-x list-colors-display を開いて、色一覧を見て、設定してみてください
(set-face-background 'show-paren-match-face "#500")

;;; redo+
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

(when (executable-find "ocp-indent")
  (ocp-setup-indent))

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
      (define-key tuareg-mode-map "\C-c\C-p" 'merlin-pop-stack)
      (local-set-key "\C-ch" 'merlin-pop-stack))))

;;; ocamlspot
;;; ocamlspotは現状自動的にロードできる仕組みになっていません
;;; 手動で入れてください。merlinがあれば何とかなりますが。
(require 'caml)
(when (require 'ocamlspot nil t)
; tuareg mode hook (use caml-mode-hook instead if you use caml-mode)
  (add-hook
   'tuareg-mode-hook
   '(lambda ()
      (ocp-setup-indent)
      ;; turn on auto-fill minor mode
      ;; (auto-fill-mode 1)

      ;; ocamlspot
      (local-set-key "\C-c;" 'ocamlspot-query)
      (local-set-key "\C-c:" 'ocamlspot-query-interface)
      (local-set-key "\C-c'" 'ocamlspot-query-uses)
      (local-set-key "\C-ct" 'ocamlspot-type)
      (local-set-key "\C-c\C-i" 'ocamlspot-xtype)
      (local-set-key "\C-c\C-y" 'ocamlspot-type-and-copy)
      (local-set-key "\C-cp" 'ocamlspot-pop-jump-stack)
      (define-key tuareg-mode-map (kbd "C-M-n") 'forward-list)
      (define-key tuareg-mode-map (kbd "C-M-p") 'backward-list)
      )
   )

 ; set the path of the ocamlspot binary.
 ; If you did make opt, ocamlspot.opt is recommended.
 ; (setq ocamlspot-command "~/Dropbox/prog/ocamlspot.opt")
)

;;; ocamldebug
;;; 標準よりもキーバインドを使いやすくします(でないと死ぬ)
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
(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "C-M-@") 'er/contract-region)

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

(setq japanese-LaTeX-default-style "jarticle")
(setq TeX-output-view-style '(("^dvi$" "." "xdvi '%d'")))
(setq preview-image-type 'dvipng)
(add-hook 'LaTeX-mode-hook (function (lambda ()
  (add-to-list 'TeX-command-list
    '("direct" "%(PDF)platex %`%S%(PDFout)%(mode)%' %t && dvipdfmx -V 4 '%s'"
        TeX-run-command t nil))
  (add-to-list 'TeX-command-list
    '("pTeX" "%(PDF)ptex %`%S%(PDFout)%(mode)%' %t"
     TeX-run-TeX nil (plain-tex-mode) :help "Run ASCII pTeX"))
  (add-to-list 'TeX-command-list
    '("pLaTeX" "%(PDF)platex %`%S%(PDFout)%(mode)%' %t"
     TeX-run-TeX nil (latex-mode) :help "Run ASCII pLaTeX"))
  (add-to-list 'TeX-command-list
    '("evince" "evince '%s.pdf' " TeX-run-command t nil))
  (add-to-list 'TeX-command-list
    '("pdf" "dvipdfmx -V 4 '%s' " TeX-run-command t nil))

)))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; Change key bindings
(add-hook 'reftex-mode-hook
 '(lambda ()
    (define-key reftex-mode-map (kbd "\C-cr") 'reftex-reference)
    (define-key reftex-mode-map (kbd "\C-cl") 'reftex-label)
    (define-key reftex-mode-map (kbd "\C-cc") 'reftex-citation)))

;; 数式のラベル作成時にも自分でラベルを入力できるようにする
(setq reftex-insert-label-flags '("s" "sfte"))

;; \eqrefを使う
(setq reftex-label-alist
      '((nil ?e nil "\\eqref{%s}" nil nil)))

; RefTeXで使用するbibファイルの位置を指定する
(setq reftex-default-bibliography '("~/tex/biblio.bib" "~/tex/biblio2.bib"))

(define-key ac-completing-map (kbd "<tab>") 'ac-complete)
(setq ac-auto-start 4)
(setq ac-auto-show-menu 0.5)

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
(require 'popwin)
(popwin-mode 1)
(global-set-key (kbd "s-v") popwin:keymap)

; 最近出したminibufferをpopupさせる
(define-key popwin:keymap (kbd "j") `popwin:popup-last-buffer)
; 最近出したminibufferを通常通り表示させる
(define-key popwin:keymap (kbd "k") `popwin:original-pop-to-last-buffer)
; set the height of popup-buffer
(setq popwin:popup-window-height 0.5)
; add popup buffers
(setq popwin:special-display-config
      (append `(("*grep*"))
              popwin:special-display-config))
; *grep*に素早く切り換える
(defun my-switch-grep-buffer()
  "grepバッファに切り替える"
  (interactive)
  (if (get-buffer "*grep*")
      (pop-to-buffer "*grep*")
(message "No grep buffer")))
(define-key popwin:keymap (kbd "g") `my-switch-grep-buffer)

;; to avoid bug
(setq popwin:close-popup-window-timer-interval 0.05)

;;; google-translate
;;; http://qiita.com/okonomi/items/f18c9221420eca47ebc6
;;; デフォルトの翻訳はC-x tで英語から日本語への翻訳
;;; この言語縛りを外すには、コマンドの前にC-uを押す
(require 'google-translate)
(global-set-key (kbd "C-x t") 'google-translate-at-point)
(global-set-key (kbd "C-x T") 'google-translate-query-translate)
;; 翻訳のデフォルト値を設定（en -> ja）
(custom-set-variables
  '(google-translate-default-source-language "en")
  '(google-translate-default-target-language "ja"))

;;; ace-jump-mode
;;; http://d.hatena.ne.jp/syohex/20120304/1330822993
;;; C-c SPCで素早くカーソルを移動する(上のリンク参照)
;;; C-x SPCで戻ってくる
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
(define-key shell-mode-map (kbd "C-c SPC") 'ace-jump-mode)

;;; git-guitter
;;; http://qiita.com/syohex/items/a669b35fbbfcdda0cbf2
(when (require 'git-gutter)
  (global-git-gutter-mode t)
  (global-set-key (kbd "M-n") 'git-gutter:next-hunk)
  (global-set-key (kbd "M-p") 'git-gutter:previous-hunk))

;;; org-mode
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

;;; image+.el
;;; emacs上で表示する画像の大きさなどを調整するための拡張
(require 'image+)
(imagex-auto-adjust-mode 1)

(add-to-list 'popwin:special-display-config '("*Find*"))

;;; anzu
;;; http://qiita.com/syohex/items/56cf3b7f7d9943f7a7ba
(when (require 'anzu) nil t
  (global-anzu-mode +1)
  (setq anzu-mode-lighter "")
  (setq anzu-deactivate-region t)
  (setq anzu-search-threshold 100)
  (global-set-key (kbd "M-s a") 'anzu-query-replace-at-cursor)
  (global-set-key (kbd "M-s q") 'anzu-query-replace)
)

(setq css-indent-offset 2)

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
(when (require 'popwin)
  (define-key global-map [(C z)] popwin:keymap))

;;; compile
(define-key my-original-map (kbd "s-c") `compile)

(define-key compilation-mode-map (kbd "j") 'compilation-next-error)
(define-key compilation-mode-map (kbd "k") 'compilation-previous-error)

(define-key my-original-map (kbd "o") `org-mode)
(define-key global-map (kbd "s-g") `keyboard-quit)
;;; 警告なしでrevert-bufferする
(define-key my-original-map (kbd "s-b")
    '(lambda () (interactive) (revert-buffer nil t t)))


;;; auto-complete-mode start
(define-key my-original-map (kbd "a") `auto-complete-mode)

;;; delete-other-windows-vertically
(define-key my-original-map (kbd "d") `delete-other-windows-vertically)

;;; back-space
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key [(C -)] 'help-command)

;;; M-x めんどくさい
(global-set-key (kbd "C-;") 'execute-extended-command)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start phase
;; 起動時に実行するコマンドを書く
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; maximize the buffer initially
;;;
;; (custom-set-variables
;;  '(initial-frame-alist (quote ((fullscreen . maximized)))))
