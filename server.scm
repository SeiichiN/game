;;; server.scm

(use gauche.net)
(use util.match)
(use rfc.822)
(use rfc.uri)
(use text.tree)
(use text.html-lite)
(use www.cgi)

;;
;; make-server-socket
;;     サーバソケットを開いて返す手続き
;;     inetドメイン ポート8080
;;     :reuse-addr? -- ポート8080が開放された直後に同じポートが使えるようにする
;; let1 -- 変数をひとつだけ束縛する簡易構文
;; socket-accept
;;     サーバソケットにリクエストが到着するのを待つ。リクエストが到着すると
;;     socket-acceptはリクエスト元のクライアントに接続された新たなソケット
;;     返す。それが変数clientに束縛される。
;; handle-request
;;     ソケットに対する入出力
;;       入力 -- (get-request (socket-input-port client))
;;       出力 -- (socket-output-port client)
;;
(define (run-server)
  (let1 server-sock (make-server-socket 'inet 8080 :reuse-addr? #t)
        (guard (e (else (socket-close server-sock) (raise e)))
               (let loop ((client (socket-accept server-sock)))
                 (guard (e (else (socket-close client) (raise e)))
                        (handle-request (get-request (socket-input-port client))
                                        (socket-output-port client))
                        (socket-close client))
                 (loop (socket-accept server-sock))))))

;;
;; get-request
;;     クライアントからのリクエストを読み取って解析する
;; params: iport
;; rxmatch-case
;;     (<正規表現> (<変数> ...) <式> ...)
;;     GET|HEAD                             -> meth
;;     \s -- 空白文字
;;     \S -- 空白文字の補集合（英数字かな） -> abs-path
;; rfc822-header->list iport
;;     リクエストの残り（RFC2822形式のヘッダ）を一気に読み取る
;; Return:
;;     GET/HEADリクエストの場合 -- リスト（meth abs-path リクエストヘッダのリスト）
;;     ほかのリクエストらしきものの場合 -- not-implemented
;;     それ以外 -- bad-request
;;
(define (get-request iport)
  (rxmatch-case (read-line iport)
                (test eof-object? 'bad-request)
                [#/^(GET|HEAD)\s+(\S+)\s+HTTP\/\d+\.\d+$/ (_ meth abs-path)
                 (list* meth abs-path (rfc822-header->list iport))]
                (#/^[A-Z]+/ () 'not-implemented)
                (else 'bad-request)))

;;
;; handle-request
;;     レスポンスメッセージをクライアントに返す手続き
;; Params:
;;     request -- get-requestの結果
;;     oport   -- 出力ポート
;; match
;;     rquest が リスト(meth abs-path . headers) にマッチしたら
;;     abs-path を uri-decompose-hierarchical の手続きにかける
;; receive
;;     uri-decompose-hierarchicalの結果を以下の変数で受け取る
;;       auth -- ドメイン（localhost:8080）
;;       path -- ドメイン名以後のパス名（/test/path）
;;       q    -- クエリ（a=b&c=d）
;;       fraq -- フラグメント（今回は #f）
;;     上記のうち、欠けているものがあれば、#f の値がセットされる。
;; cgi-parse-parameters
;;     "a=b&c=d&e"というクエリをパースして、(("a" "b") ("c" "d") ("e" #t))
;;     というリストを返す
;;
(define (handle-request request oport)
  (match request
    ('bad-request (display "HTTP/1.1 400 Bad Request\r\n\r\n" oport))
    ('not-implemented (display "HTTP/1.1 501 Not Implemented\r\n\r\n" oport))
    ((meth abs-path . headers)
     (receive (auth path q fraq) (uri-decompose-hierarchical abs-path)
       (let1 content
           (render-content path (cgi-parse-parameters :query-string (or q "")))
         (display "HTTP/1.1 200 OK\r\n" oport)
         (display "Content-Type: text/html; charset=utf-8\r\n" oport)
         (display #`"Content-Length: ,(string-size content)\r\n" oport)
         (display "\r\n" oport)
         (when (equal? meth "GET") (display content oport)))))))

(define (render-content path params)
  (tree->string
   (html:html
    (html:head (html:title "sinple httpd"))
    (html:body (html:h1 "Welcome to simple httpd")
               (html:p "Path: " (html-escape-string path))
               (map (lambda (p)
                      (html:p (html-escape-string (car p)) " : "
                              (html-escape-string (cdr p))))
                    params)))))

(define (main args)
  (run-server)
  0)
