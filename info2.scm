;; -*- coding: utf-8 -*-
;;
;; info2.scm
;; 2016-10-27 v1.18
;;
;; ＜内容＞
;;   Gauche で info 手続きを拡張した info2 手続きを使用可能にするための
;;   モジュールです。
;;   標準の info 手続きは、検索する infoファイル名が gauche-refe.info に固定となって
;;   いますが、info2 手続きは、検索する infoファイル名を 指定することができます。
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
;;   info2 手続きの書式は以下の通りです。
;;
;;   info2  name  [file]  [ces1]  [ces2]  [cache-reset]
;;   ・第1引数の name には、調べたい手続きの名前をシンボルか文字列で指定します。
;;   ・第2引数の file には、infoファイルの名前をシンボルか文字列で指定します。
;;     このとき、ファイル名の末尾の .info または -refe.info は省略可能です。
;;     また、第2引数全体も省略可能です。省略した場合は gauche-refe.info が
;;     読み込まれます。
;;   ・第3引数の ces1 には、出力する説明文の文字エンコーディングを指定します。
;;     例えば、Windowsのコンソールに日本語を出力する場合は 'SJIS を指定してください。
;;     第3引数に #f を指定すると、文字エンコーディングは未指定になります。
;;     また、第3引数は省略可能です。省略した場合は #f を指定したことになります。
;;   ・第4引数の ces2 には、検索結果が複数存在する場合に出力するセクション名の
;;     文字エンコーディングを指定します。
;;     第4引数に #f を指定すると、文字エンコーディングは未指定になります。
;;     第4引数に #t を指定すると、文字エンコーディングは ces1 と同じものになります。
;;     また、第4引数は省略可能です。省略した場合は #t を指定したことになります。
;;   ・第5引数の cache-reset には、キャッシュをリセットするかどうかを指定します。
;;     すでに読み込んだ infoファイルは、キャッシュに保存され高速検索が可能になりますが、
;;     本引数に #t を指定すると、キャッシュを破棄してファイルを再読み込みします。
;;     第5引数は省略可能です。省略した場合は #f を指定したことになります。
;;
;; ＜注意事項＞
;;   ・本モジュールは、gauche.interactive.info をベースに改造しました。
;;     また、内部で text.info モジュールの定義をいくつか上書きしています。
;;     このため、Gauche の将来のバージョンアップで動かなくなる可能性があります。
;;   ・Gauche の infoファイルの格納先は、以下のコマンドで確認可能です。
;;     gauche-config --infodir
;;   ・検索する infoファイルには、手続きの索引のノードがある必要があります。
;;     そして、そのノード名が、info2.scm 内の *index-node-name* の項目のいずれかと
;;     一致している必要があります。
;;   ・Gauche v0.9.5_pre1 で info 手続きの仕様が変わり、指定した手続きの説明のみを
;;     表示するようになりました。
;;     これに追従して info2 手続きも、v0.9.5_pre1 以後で可能な場合には、指定した
;;     手続きの説明のみを表示するようにしました。
;;     以前と同様に章全体を表示したい場合は、info2-page 手続きを使用してください。
;;     info2-page 手続きの書式は、info2 手続きと同様です。
;;   ・例えば (info2 'if) のように 検索結果が複数存在する項目については、対応する
;;     各セクション名が表示されます。そこで、番号を入力すると、選択したセクションの
;;     説明文が表示されます。
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
  (export info2 info2-page))
(select-module info2)

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
(define *info-table*        (make-hash-table 'equal?))
(define *info-index-table*  (make-hash-table 'equal?))

(define *pager* #f)

(define (get-pager)
  (cond-expand
   [gauche.os.windows
    (or (sys-getenv "PAGER")
        ;; These commands don't work well on Windows console.
        ;(find-file-in-paths "less.exe") ; It has a problem of printing wide characters.
        ;(find-file-in-paths "more.com") ; It works only when ces is a kind of Shift_JIS.
        )]
   [else
    (or (sys-getenv "PAGER")
        (find-file-in-paths "less")
        (find-file-in-paths "more"))]))

(define (viewer-pager s)
  (let1 p (run-process
           (cond-expand
            ;; for MSYS (mintty)
            [gauche.os.windows `("cmd.exe" "/c" ,*pager*)]
            [else              *pager*])
           :input :pipe)
    (guard (e (else #f))
      (display s (process-input p)))
    (close-output-port (process-input p))
    (process-wait p)))

(define (viewer-dumb s) (display s))

(define viewer
  (lambda (s)
    (set! *pager* (get-pager))
    (cond [(or (equal? (sys-getenv "TERM") "emacs")
               (equal? (sys-getenv "TERM") "dumb")
               (cond-expand
                ;; for MSYS (mintty)
                [gauche.os.windows]
                [else (not (sys-isatty (current-output-port)))])
               (not *pager*))
           (viewer-dumb s)]
          [else
           (viewer-pager s)])))

(define (get-info-paths)
  (let* ([syspath (cond [(sys-getenv "INFOPATH") => (cut string-split <> #\:)]
                        [else '()])]
         [instpath (list (gauche-config "--infodir"))]
         [in-place (list "../doc")])
    (append syspath instpath in-place)))

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

(define (get-info&index info-file cache-reset)
  (let ((info1  (hash-table-get *info-table*       info-file #f))
        (index1 (hash-table-get *info-index-table* info-file #f)))
    (when (or (not info1) cache-reset)
      (set! info1  (open-info-file (find-info-file info-file)))
      (set! index1 (make-hash-table 'string=?))
      (let ((node       #f)
            (entry-name "")
            (hit-flag   #f)
            (class-flag #f))
        (dolist [node-name *index-node-name*]
          (set! class-flag (if (#/(class|クラス)/i node-name) #t #f))
          (set! node (info-get-node info1 node-name))
          (when node
            (dolist [p (info-parse-menu node)]
              (set! entry-name (car p))
              (if-let1 m (#/ <\d+>$/ entry-name)
                (set! entry-name (rxmatch-before m)))
              (if class-flag
                (set! entry-name #"<~|entry-name|>"))
              ;; For Gauche v0.9.4 compatibility
              ;(hash-table-push! index1 entry-name (cdr p)))
              (hash-table-push! index1 entry-name
                                (if (pair? (cdr p)) (cdr p) (list (cdr p)))))
            (set! hit-flag #t)))
        (if (not hit-flag) (errorf "no index in info file ~s" info-file)))
      (hash-table-put! *info-table*       info-file info1)
      (hash-table-put! *info-index-table* info-file index1))
    (cons info1 index1)))

(define (ces-conv-str ces str)
  (if ces (ces-convert str (gauche-character-encoding) ces) str))

(define (lookup&show key index1 ces2 show)
  (match (reverse (hash-table-get index1 (x->string key) '()))
    [()  (print "No info document for " key)]
    [(e) (show e)]
    [(es ...)
     (print "There are multiple entries for " key ":")
     (for-each-with-index
      (^[i e]
        (print (ces-conv-str ces2 (format "~2d. ~s" (+ i 1) (car e)))))
      es)
     (let loop ()
       (format #t "Select number, or q to cancel [1]: ") (flush)
       ;; For Gauche v0.9.4 compatibility
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

(define (info2-sub fn info-file-sym ces1 ces2 cache-reset page-flag)
  (let* ((info-file  (if info-file-sym
                       (x->string info-file-sym)
                       *info-file-default*))
         (info&index (get-info&index info-file cache-reset))
         (info1      (car info&index))
         (index1     (cdr info&index)))
    (if (eq? ces2 #t) (set! ces2 ces1))
    ($ lookup&show fn index1 ces2
       (^[node&line]
         (let* ((node (info-get-node info1 (car node&line)))
                (str  (if (or (null? (cdr node&line)) page-flag)
                        (~ node 'content)
                        (info-extract-definition node (cadr node&line)))))
           (viewer (ces-conv-str ces1 str)))))
    (values)))

;; API
(define (info2 fn :optional (info-file-sym #f) (ces1 #f) (ces2 #t) (cache-reset #f))
  (info2-sub fn info-file-sym ces1 ces2 cache-reset #f))

;; API
(define (info2-page fn :optional (info-file-sym #f) (ces1 #f) (ces2 #t) (cache-reset #f))
  (info2-sub fn info-file-sym ces1 ces2 cache-reset #t))



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
  (define (with-input-from-info thunk)
    (cond [(file-exists? file)
           (call-with-input-file file
             (^p (let1 cp (wrap-with-input-conversion p "*JP") ; encoding conversion
                   (unwind-protect (with-input-from-port cp thunk)
                     (close-input-port cp)))))]
          [(file-exists? #`",|file|.gz")
           (call-with-input-file #`",|file|.gz"
             (^p (let1 zp (open-inflating-port p :window-bits 31) ; force gzip format
                   (let1 cp (wrap-with-input-conversion zp "*JP") ; encoding conversion
                     (unwind-protect (with-input-from-port cp thunk)
                       (close-input-port cp))))))]
          [(and bzip2 (file-exists? #`",|file|.bz2"))
           ;; For Windows, a file name should be surrounded in quotation marks.
           (call-with-input-process #`",bzip2 -c -d ',|file|.bz2'"
             (^p (let1 cp (wrap-with-input-conversion p "*JP") ; encoding conversion
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
  (define (skip-lines)
    ;; On makeinfo v6.0, start-line is decremented by one.
    ;(let loop ([n (- start-line 4)]
    (let loop ([n (- start-line 3)]
               [lines '()])
      ;(if (= n 0)
      (if (<= n 0)
        (let1 line (read-line)
          ;(print n " " line)
          ;; For header printing problem (e.g. (info 'set!) )
          ;(unless (#/^ --/ line) (for-each print (reverse lines)))
          (for-each print (reverse lines))
          line)
        (let1 line (read-line)
          ;(print "+" n " " line)
          (cond [(eof-object? line) line]  ;something's wrong, but tolerate.
                ;; For header printing problem (e.g. (info 'cdr) )
                ;[(#/^ --/ line) (loop (- n 1) (list line))]
                [(#/^ --/ line) (loop (- n 1) (cons line lines))]
                [(#/^ {10}/ line) (loop (- n 1) (cons line lines))]
                [else (loop (- n 1) '())])))))

  ;; Once the start line is found, we find the start of description (since
  ;; the entry may have multiple entry line, e.g. @defunx.) then scan the
  ;; description until we get an emtpy line.
  (with-string-io (~ info-node'content)
    (^[]
      (let entry ([line (skip-lines)])
        (unless (eof-object? line)
          (cond [(#/^ --/ line) (print line) (entry (read-line))]
                [(#/^$/ line)]       ;; no description
                [(#/^ {6}/ line)     ;; folded entry line
                 (print line) (entry (read-line))]
                [(#/^ {5}...$/ line) ;; dots between entry line
                 (print line) (entry (read-line))]
                [(#/^ {5}\S/ line)   ;; start description
                 (print line)
                 (let desc ([line (read-line)])
                   (unless (eof-object? line)
                     (cond [(#/^$/ line) (print) (desc (read-line))]
                           [(#/^ {4}/ line) (print line) (desc (read-line))]
                           [else])))]))))))

