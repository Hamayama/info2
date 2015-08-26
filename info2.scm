;; -*- coding: utf-8 -*-
;;
;; info2.scm
;; 2015-8-27 v1.02
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
;;   info2  name  [file]  [ces]  [cache-reset]
;;   ・第1引数の name には、調べたい手続きの名前をシンボルで指定します。
;;   ・第2引数の file には、infoファイルの名前をシンボルで指定します。
;;     このとき、ファイル名の末尾の .info または -refe.info は省略可能です。
;;     また、第2引数全体も省略可能です。省略した場合は gauche-refe.info が
;;     読み込まれます。
;;   ・第3引数の ces には、出力する文字のエンコーディングを指定します。
;;     例えば、Windowsのコンソールに日本語を出力する場合は 'SJIS を指定してください。
;;     第3引数に #f を指定すると、エンコーディングは未指定になります。
;;     また、第3引数は省略可能です。省略した場合は #f を指定したことになります。
;;   ・第4引数の cache-reset には、キャッシュをリセットするかどうかを指定します。
;;     すでに読み込んだ infoファイルは、キャッシュに保存され高速検索が可能になりますが、
;;     本引数に #t を指定すると、キャッシュを破棄してファイルを再読み込みします。
;;     第4引数は省略可能です。省略した場合は #f を指定したことになります。
;;
;; ＜注意事項＞
;;   ・本モジュールは、gauche.interactive.info をベースに改造しました。
;;     また、内部で text.info モジュールの定義をいくつか上書きしています。
;;     このため、Gauche の将来のバージョンアップで動かなくなる可能性があります。
;;   ・Gauche の infoファイルの格納先は、以下のコマンドで確認可能です。
;;     gauche-config --infodir
;;
(define-module info2
  (use srfi-1)
  (use srfi-13)
  (use text.info)
  (use file.util)
  (use gauche.process)
  (use gauche.config)
  (use gauche.charconv)
  (export info2))
(select-module info2)

(define *info-file-default* "gauche-refe.info")
(define *info-file-plus*    '("" ".info" "-refe.info"))
(define *index-node-name*
  (cond-expand
   [gauche.ces.none
    '("Function and Syntax Index")
    ]
   [else
    '("Function and Syntax Index"
      "Index - 手続きと構文索引" ; gauche-refj.info
      "手続きと構文索引"         ; gauche-gl-refj.info
      "手続き索引"               ; gauche-al-refj.info
      )
    ])
  )
(define *info-table*        (make-hash-table 'equal?))
(define *info-index-table*  (make-hash-table 'equal?))

(define *pager*
  (cond-expand
   [gauche.os.windows
    (or (sys-getenv "PAGER")
        ;(find-file-in-paths "less.exe")
        (find-file-in-paths "more.com"))
    ]
   [else
    (or (sys-getenv "PAGER")
        (find-file-in-paths "less")
        (find-file-in-paths "more"))
    ])
  )

(define viewer
  (if
      (cond-expand
       [gauche.os.windows
        (or (equal? (sys-getenv "TERM") "emacs")
            (equal? (sys-getenv "TERM") "dumb")
            (not *pager*))
        ]
       [else
        (or (equal? (sys-getenv "TERM") "emacs")
            (equal? (sys-getenv "TERM") "dumb")
            (not (sys-isatty (current-output-port)))
            (not *pager*))
        ])
    display
    (^s
     (let1 p (run-process *pager* :input :pipe)
       (guard (e (else #f))
         (display s (process-input p)))
       (close-output-port (process-input p))
       (process-wait p)))))

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
                                             (file-is-readable? #"~|p|.gz")
                                             (file-is-readable? #"~|p|.bz2")))))
         *info-file-plus*)
        (errorf "couldn't find info file ~s in paths: ~s" info-file paths))
    ))

(define (info2 fn :optional (info-file-sym #f) (ces #f) (cache-reset #f))
  (let* ((info-file   (if info-file-sym
                        (x->string info-file-sym)
                        *info-file-default*))
         (info1       (hash-table-get *info-table*       info-file #f))
         (info1-index (hash-table-get *info-index-table* info-file #f)))
    (when (or (not info1) cache-reset)
      (set! info1       (open-info-file (find-info-file info-file)))
      (set! info1-index (make-hash-table 'string=?))
      (if-let1 node1 (any (lambda (nodename)
                            (info-get-node info1 nodename))
                          *index-node-name*)
        (dolist [p (info-parse-menu node1)]
          (hash-table-put! info1-index (car p) (cdr p)))
        (errorf "no index in info file ~s" info-file))
      (hash-table-put! *info-table*       info-file info1)
      (hash-table-put! *info-index-table* info-file info1-index))
    (if-let1 nodename (hash-table-get info1-index (x->string fn) #f)
      (let1 str (ref (info-get-node info1 nodename) 'content)
        (if ces (set! str (ces-convert str (gauche-character-encoding) ces)))
        (viewer str))
      (errorf "no info document for ~a" fn))
    (values)))



(select-module text.info)

(use gauche.charconv)

;; overwrite some definitions in text.info module.

;; Find bzip2 location
(define bzip2
  (cond-expand
   [gauche.os.windows
    (find-file-in-paths "bzip2.exe")
    ]
   [else
    (find-file-in-paths "bzip2")
    ])
  )

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
           ;; for Windows, file name should be surrounded in quotation marks.
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

