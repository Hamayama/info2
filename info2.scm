;; -*- coding: utf-8 -*-
;;
;; info2.scm
;; 2017-9-24 v1.33
;;
;; ＜内容＞
;;   Gauche で info 手続きを拡張した info2 手続きを使用可能にするためのモジュールです。
;;   標準の info 手続きは、検索する infoファイル名が gauche-refe.info に固定と
;;   なっていますが、info2 手続きは、検索する infoファイル名を 指定することができます。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/info2
;;
;; ＜インストール方法＞
;;   info2.scm を Gauche でロード可能なフォルダにコピーします。
;;   (例えば (gauche-site-library-directory) で表示されるフォルダ等)
;;
;; ＜使い方＞
;;   (use info2)                     ; モジュールをロードします
;;   (info2 'car)                    ; car 手続きの説明を英語で表示します
;;   (info2 'car 'gauche-refj 'SJIS) ; car 手続きの説明を日本語のシフトJISで表示します
;;
;; ＜書式＞
;;   info2 手続きの書式は以下の通りです。
;;   info2  name  [file]  [ces1]  [ces2]  [cache-reset]
;;   ・第1引数の name には、調べたい手続きの名前をシンボルか文字列で指定します。
;;   ・第2引数の file には、infoファイルの名前をシンボルか文字列で指定します。
;;     このとき、ファイル名の末尾の .info または -refe.info は省略可能です。
;;     また、第2引数全体も省略可能です。省略した場合には gauche-refe.info が
;;     読み込まれます。
;;   ・第3引数の ces1 には、出力する説明文の文字エンコーディングを指定します。
;;     例えば、Windowsのコンソールに日本語を出力する場合には 'SJIS を指定してください。
;;     第3引数に #f を指定すると、文字エンコーディングは未指定になります。
;;     また、第3引数は省略可能です。省略した場合には #f を指定したことになります。
;;   ・第4引数の ces2 には、検索結果が複数存在する場合に出力するセクション名の
;;     文字エンコーディングを指定します。
;;     第4引数に #f を指定すると、文字エンコーディングは未指定になります。
;;     第4引数に #t を指定すると、文字エンコーディングは ces1 と同じものになります。
;;     また、第4引数は省略可能です。省略した場合には #t を指定したことになります。
;;   ・第5引数の cache-reset には、キャッシュをリセットするかどうかを指定します。
;;     すでに読み込んだ infoファイルは、キャッシュに保存され高速検索が可能になりますが、
;;     本引数に #t を指定すると、キャッシュを破棄してファイルを再読み込みします。
;;     第5引数は省略可能です。省略した場合には #f を指定したことになります。
;;
(define-module info2
  (use srfi-1)
  (use srfi-13)
  (use text.info)
  (use file.util)
  (use util.match)
  (use gauche.process)
  (use gauche.config)
  (use gauche.sequence)
  (use gauche.charconv)
  (use gauche.version)
  (cond-expand [gauche.os.windows (use os.windows)] [else])
  (export info2 info2-page info2-search *info-file-ces*))
(select-module info2)

(define *info-file-ces* "*JP") ; set encoding of info files

(define *info-file-default* "gauche-refe.info")
(define *info-file-plus*    '("" ".info" "-refe.info"))
(define *index-node-name*
  (cond-expand
   [gauche.ces.none
    '("Function and Syntax Index" ; gauche-refe.info, gauche-gl-refe.info
      "Function Index"            ; gauche-al-refe.info
      "Module Index"              ; gauche-refe.info, gauche-gl-refe.info
      "Class Index"               ; gauche-refe.info, gauche-gl-refe.info, gauche-al-refe.info
      "Variable Index"            ; gauche-refe.info, gauche-gl-refe.info, gauche-al-refe.info
      )]
   [else
    '("Function and Syntax Index" ; gauche-refe.info, gauche-gl-refe.info
      "Function Index"            ; gauche-al-refe.info
      "Module Index"              ; gauche-refe.info, gauche-gl-refe.info
      "Class Index"               ; gauche-refe.info, gauche-gl-refe.info, gauche-al-refe.info
      "Variable Index"            ; gauche-refe.info, gauche-gl-refe.info, gauche-al-refe.info
      ;; Japanese
      "Index - 手続きと構文索引"  ; gauche-refj.info
      "手続きと構文索引"          ; gauche-gl-refj.info
      "手続き索引"                ; gauche-al-refj.info
      "Index - モジュール索引"    ; gauche-refj.info
      "モジュール索引"            ; gauche-gl-refj.info
      "Index - クラス索引"        ; gauche-refj.info
      "クラス索引"                ; gauche-gl-refj.info, gauche-al-refj.info
      "Index - 変数索引"          ; gauche-refj.info
      "変数索引"                  ; gauche-gl-refj.info, gauche-al-refj.info
      )]))

(define-class <repl-info> ()
  ((info  :init-keyword :info)   ;; <info-file>
   (index :init-keyword :index)  ;; hashtable name -> [(node-name line-no)]
   ))
(define *repl-info-cache* (make-hash-table 'equal?))

(define *pager* #f)

(define (get-pager)
  (cond-expand
   [gauche.os.windows
    (or (sys-getenv "PAGER")
        ;; These commands don't work well on windows console.
        ;(find-file-in-paths "less.exe") ; It has a problem of printing wide characters.
        ;(find-file-in-paths "more.com") ; It works only when ces is a kind of Shift_JIS.
        )]
   [else
    (or (sys-getenv "PAGER")
        (find-file-in-paths "less")
        (find-file-in-paths "more")
        )]))

(define (viewer-pager s ces)
  (let1 p (run-process
           (cond-expand
            ;; for MSYS (mintty)
            [gauche.os.windows `("cmd.exe" "/c" ,*pager*)]
            [else              *pager*])
           :input :pipe)
    (guard (e (else #f))
      (with-output-conversion (process-input p)
        (cut display s) :encoding (or ces "none")))
    (close-output-port (process-input p))
    (process-wait p)))

(define (viewer-dumb s ces)
  (with-output-conversion (current-output-port)
    (cut display s) :encoding (or ces "none")))

;; for MSYS (mintty)
(define sys-mintty?
  (if (global-variable-bound? 'gauche.internal '%sys-mintty?)
    (with-module gauche.internal %sys-mintty?)
    (lambda (port-or-fd) #f)))

;; for windows console conversion ports
(define port-attribute-ref
  (if (global-variable-bound? 'gauche 'port-attribute-ref)
    (with-module gauche port-attribute-ref)
    (lambda (port key :optional fallback) #f)))

(define (redirected? port)
  (cond-expand
   [gauche.os.windows
    (not (or (sys-isatty port)
             ;; for MSYS (mintty)
             (sys-mintty? port)
             ;; for windows console conversion ports
             (port-attribute-ref port 'windows-console-conversion #f)))]
   [else
    (not (sys-isatty port))]))

(define (select-viewer)
  (set! *pager* (get-pager))
  (cond [(or (equal? (sys-getenv "TERM") "emacs")
             (equal? (sys-getenv "TERM") "dumb")
             (redirected? (current-output-port))
             (not *pager*))
         viewer-dumb]
        [else
         viewer-pager]))

(define (nounicode s)
  (cond-expand
   [(and gauche.os.windows
         (not gauche.ces.none))
    ;; for windows console
    (if (and (sys-isatty (standard-output-port))
             (not (= (sys-get-console-output-cp) 65001)))
      ($ regexp-replace-all* s
         #/\u21d2/ "==>"      ; @result{}
         #/\u2026/ "..."      ; @dots{}
         #/\u2018/ "`"        ; @code{}
         #/\u2019/ "'"        ; @code{}
         #/\u201C/ "``"       ; ``         (e.g. ,i do)
         #/\u201D/ "''"       ; ''         (e.g. ,i do)
         #/\u2261/ "=="       ; @equiv{}   (e.g. ,i cut)
         #/\u2212/ "-"        ; @minus     (e.g. ,i modulo)
         #/\u2022/ "*"        ; @bullet    (e.g. ,i lambda)
         #/\u2013/ "--"       ; --         (e.g. ,i utf8-length)
         #/\u2014/ "---"      ; ---        (e.g. ,i lambda)
         #/\u00df/ "[Eszett]" ; eszett     (e.g. ,i char-upcase)
         )
      s)]
   [else s]))

(define (viewer s ces)
  ((select-viewer) (nounicode s) ces))

(define (get-info-paths)
  (let* ([syspath (cond [(sys-getenv "INFOPATH")
                         => (cut string-split <> (cond-expand
                                                  [gauche.os.windows #\;]
                                                  [else #\:]))]
                        [else '()])]
         [instpath (list (gauche-config "--infodir"))]
         [in-place (cond-expand
                    [gauche.in-place (if (member "../../lib" *load-path*)
                                       '("../../doc")
                                       '("../doc"))]
                    [else '()])])
    (append in-place syspath instpath)))

(define (find-info-file info-file)
  (let1 paths (get-info-paths)
    (or (any
         (lambda (plus)
           (find-file-in-paths (string-append info-file plus)
                               :paths paths
                               :pred (^p (or (file-is-readable? p)
                                             (file-is-readable? #`",|p|.gz")
                                             (file-is-readable? #`",|p|.bz2")))))
         *info-file-plus*)
        (errorf "couldn't find info file ~s in paths: ~s" info-file paths))
    ))

(define (get-repl-info info-file cache-reset)
  (rlet1 repl-info1 (hash-table-get *repl-info-cache* info-file #f)
    (when (or (not repl-info1) cache-reset)
      (let ([info1      (open-info-file (find-info-file info-file))]
            [index1     (make-hash-table 'string=?)]
            [node       #f]
            [entry-name ""])
        (dolist [node-name *index-node-name*]
          (set! node (info-get-node info1 node-name))
          (when node
            (dolist [p (info-parse-menu node)]
              (set! entry-name (car p))
              ;; When there are more than one entry with the same name, texinfo appends
              ;; " <n>" in the index entry.  This strips that.
              (if-let1 m (#/ <\d+>$/ entry-name)
                (set! entry-name (rxmatch-before m)))
              ;; class index doesn't have surrounding '<>', but we want to search
              ;; with them.
              (if (#/(class|クラス)/i node-name)
                (set! entry-name #"<~|entry-name|>"))
              ;; for Gauche v0.9.4 compatibility
              ;(hash-table-push! index1 entry-name (cdr p)))
              (hash-table-push! index1 entry-name
                                (if (pair? (cdr p)) (cdr p) (list (cdr p)))))))
        (if (<= (hash-table-num-entries index1) 0)
          (errorf "no index in info file ~s" info-file))
        ($ hash-table-for-each index1
           ;; reverse v here so that earlier entry listed first
           (^[k v] (hash-table-put! index1 k (reverse v))))
        (set! repl-info1 (make <repl-info> :info info1 :index index1))
        (hash-table-put! *repl-info-cache* info-file repl-info1)))))

(define (lookup&show key index1 ces2 show)
  (define node&lines (hash-table-get index1 (x->string key) '()))
  (match node&lines
    [()  (print "No info document for " key)]
    [(e) (show e)]
    [(es ...)
     (print "There are multiple entries for " key ":")
     (for-each-with-index
      (^[i e]
        (with-output-conversion (current-output-port)
          (^[] (format #t "~2d. ~s\n" (+ i 1) (car e)))
          :encoding (or ces2 "none")))
      es)
     (let loop ()
       (format #t "Select number, or q to cancel [1]: ") (flush)
       ;; for Gauche v0.9.4 compatibility
       (if (version<=? (gauche-version) "0.9.4") (read-line))
       (rxmatch-case (read-line)
         [test eof-object? #f]
         [#/^\s*$/ (_) (show (car es))]  ; the first entry by default
         [#/^\s*(\d+)\s*$/ (_ N)
          (let1 n (- (x->integer N) 1)
            (if (and (<= 0 n) (< n (length es)))
              (show (list-ref es n))
              (loop)))]
         [#/^\s*q\s*$/ (_) #f]
         [else (loop)]))])
  (values))

(define (info2-sub key info-file-sym ces1 ces2 cache-reset page-flag)
  (let* ([info-file  (if info-file-sym
                       (x->string info-file-sym)
                       *info-file-default*)]
         [repl-info1 (get-repl-info info-file cache-reset)])
    (if (eq? ces2 #t) (set! ces2 ces1))
    ($ lookup&show key (~ repl-info1 'index) ces2
       (^[node&line]
         (let* ((node (info-get-node (~ repl-info1 'info) (car node&line)))
                (str  (if (or (null? (cdr node&line)) page-flag)
                        (~ node 'content)
                        (info-extract-definition node (cadr node&line)))))
           (viewer str ces1))))))

;; API
(define (info2 key :optional (info-file-sym #f) (ces1 #f) (ces2 #t) (cache-reset #f))
  (info2-sub key info-file-sym ces1 ces2 cache-reset #f))

;; API
(define (info2-page key :optional (info-file-sym #f) (ces1 #f) (ces2 #t) (cache-reset #f))
  (info2-sub key info-file-sym ces1 ces2 cache-reset #t))

;;
;; Search info entries by regexp
;;

(define (search-entries rx index1)
  (sort (filter (^e (rx (car e))) (hash-table->alist index1))
        string<? car))

(define *search-entry-indent* 25)

(define (format-search-result-entry entry) ; (key (node line) ...)
  (define indent (make-string *search-entry-indent* #\space))
  (define (subsequent-lines node&lines)
    (dolist [l node&lines]
      ;; for Gauche v0.9.4 compatibility
      ;(format #t "~va~a:~d\n" *search-entry-indent* " " (car l) (cadr l))))
      (format #t "~va~a:~d\n" *search-entry-indent* " " (car l) (if (pair? (cdr l))
                                                                  (cadr l)
                                                                  ""))))
  (match-let1 (key node&lines ...) entry
    (if (> (string-length key) (- *search-entry-indent* 1))
      (begin (print key) (subsequent-lines node&lines))
      ;; for Gauche v0.9.4 compatibility
      ;(begin (format #t "~va ~a:~d\n" (- *search-entry-indent* 1) key
      ;               (caar node&lines) (cadar node&lines))
      (begin (format #t "~va ~a:~d\n" (- *search-entry-indent* 1) key
                     (caar node&lines) (if (pair? (cdar node&lines))
                                         (cadar node&lines)
                                         ""))
             (subsequent-lines (cdr node&lines))))))

;; API
(define (info2-search rx :optional (info-file-sym #f) (ces1 #f) (cache-reset #f))
  (let* ((info-file  (if info-file-sym
                       (x->string info-file-sym)
                       *info-file-default*))
         (repl-info1 (get-repl-info info-file cache-reset)))
    ;; for Gauche v0.9.4 compatibility
    ;(assume-type rx <regexp>)
    (check-arg (cut is-a? <> <regexp>) rx)
    (let1 entries (search-entries rx (~ repl-info1 'index))
      (if (null? entries)
        (print #"No entry matching ~|rx|")
        (let1 str (with-output-to-string
                    (^[] (for-each format-search-result-entry entries)))
          (viewer str ces1))))
    (values)))



(select-module text.info)

(use gauche.charconv)

;; Overwrite some definitions in text.info module.

;; Find bzip2 location
(define bzip2
  (cond-expand
   [gauche.os.windows
    (find-file-in-paths "bzip2.exe")]
   [else
    (find-file-in-paths "bzip2")]))

;; Read an info file FILE, and returns a list of strings splitted by ^_ (#\u001f)
;; If FILE is not found, look for compressed one.
(define (read-info-file-split file opts)
  (define ces (if (global-variable-bound? 'info2 '*info-file-ces*)
                (or (with-module info2 *info-file-ces*) "none")
                "none"))
  (define (with-input-from-info thunk)
    (cond [(file-exists? file)
           (call-with-input-file file
             (^p (let1 cp (wrap-with-input-conversion p ces) ; encoding conversion
                   (unwind-protect (with-input-from-port cp thunk)
                     (close-input-port cp)))))]
          [(file-exists? #`",|file|.gz")
           (call-with-input-file #`",|file|.gz"
             (^p (let1 zp (open-inflating-port p :window-bits 31) ; force gzip format
                   (let1 cp (wrap-with-input-conversion zp ces) ; encoding conversion
                     (unwind-protect (with-input-from-port cp thunk)
                       (close-input-port cp))))))]
          [(and bzip2 (file-exists? #`",|file|.bz2"))
           ;; For windows, a file name should be surrounded in quotation marks.
           (call-with-input-process #`",bzip2 -c -d ',|file|.bz2'"
             (^p (let1 cp (wrap-with-input-conversion p ces) ; encoding conversion
                   (unwind-protect (with-input-from-port cp thunk)
                     (close-input-port cp)))))]
          [else (error "can't find info file" file)]))
  (with-input-from-info
   (lambda ()
     (let loop ([c (skip-while (char-set-complement #[\u001f]))]
                [r '()])
       (if (eof-object? c)
         (reverse! r)
         (let* ([head (next-token #[\u001f\n] '(#[\u001f\n] *eof*))]
                [body (next-token #[\n] '(#[\u001f] *eof*))])
           (loop (read-char) (acons head body r)))))))
  )

;; API
;; Extract one definition from the node's content.  Assumes the definition
;; begins from the specified line; then we go forward to find the end of
;; the definition.  The end of definition is when we see the end of content,
;; or we see a line begins with less than or equal to 3 whitespaces.
;; (Except the 'defunx'-type multi entry)
(define (info-extract-definition info-node start-line)

  ;; Skip the lines before the entry.
  ;; START-LINE counts from the beginning of the info doc; the first 3 lines
  ;; are taken by the node header and the node's content doesn't include them.
  ;; Also note that line count starts from 1.
  ;;
  ;; Caveat: If the entry header spans multiple lines because of large
  ;; number of arguments, the texinfo menu's line number somehow points to the
  ;; last line of the entry header.  For example, the entry of http-get
  ;; begins with this:
  ;;
  ;;   -- Function: http-get server request-uri :key sink flusher
  ;;        redirect-handler secure ...
  ;;
  ;; And the texinfo menu's line points to "redirect-handler secure ..." line
  ;; instead of "-- Function: http-get" line.  So we have to check the lines
  ;; to find out the last #/^ --/ line before START-LINE.
  (define (entry-line? line)
    (or (#/^ --/ line)           ; start of entry line
        (#/^ {10}/ line)         ; folded entry line
        (#/^ {5}\.\.\.$/ line)   ; dots between entry line
        (#/^ {5}\u2026$/ line))) ; dots between entry line (unicode)
  (define (skip-lines)
    (let loop ([n (- start-line 3)]
               [lines '()])
      (if (<= n 0)
        (let1 line (read-line)
          (for-each print (reverse lines))
          line)
        (let1 line (read-line)
          (cond [(eof-object? line) line] ; something's wrong, but tolerate.
                [(entry-line? line) (loop (- n 1) (cons line lines))]
                [else (loop (- n 1) '())])))))

  ;; Once the start line is found, we find the start of description (since
  ;; the entry may have multiple entry line, e.g. @defunx.) then scan the
  ;; description until we get an emtpy line.
  (with-string-io (~ info-node'content)
    (^[]
      (let entry ([line (skip-lines)])
        (cond [(eof-object? line)]
              [(entry-line? line) (print line) (entry (read-line))]
              [(#/^$/ line)]     ; no description
              [(#/^ {5}\S/ line) ; start description
               (print line)
               (let desc ([line (read-line)])
                 (cond [(eof-object? line)]
                       [(#/^$/ line) (print) (desc (read-line))]
                       [(#/^ {4}/ line) (print line) (desc (read-line))]
                       [else]))])))))

