;;; -*- coding: utf-8 -*-
;;; game.scm

(use gauche.net)
(use util.match)
(use rfc.822)
(use rfc.uri)
(use text.tree)
(use text.html-lite)
(use www.cgi)
(use util.list)  ; リスト処理の補助ユーティリティ
(use srfi-27)    ; 疑似乱数発生器を提供するモジュール

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

;;; ここまでは、server.scm と同じ。========================================


;; 疑似乱数発生器に種を与えて初期化する
(random-source-randomize! default-random-source)

;; それぞれのシンボルから方向を表す文字列を得る
(define (get-direction dir)
  (assoc-ref '((n . "北") (e . "東") (w . "西") (s . "南")) dir)) 

;; p127, p422
;; マップデータ
;;
;;    # 0
;;    |
;;    |
;;  1 #--# 3沼
;;    |
;;    |
;; #--# 2
;; 4
;; 広場
;;
(define *dungeon*
  '(("あなたは森の北側にいる。道は南に続いている。"
     (s . 1))
    ("あなたは鬱蒼とした森の中の道にいる。
道は南北に伸びている。東に降りていく小径がある。"
     (n . 0)
     (s . 2)
     (e . 3))
    ("足元がぬかるんでいる。道は直角に折れ、北と西に伸びている。
西に続く道の先が明るくなっている。"
     (n . 1)
     (w . 4))
    ("あなたは沼のほとりにいる。空気の動きが止まり、暑さを感じる。
西に登ってゆく小径がある。"
     (w . 1))
    ("突然目の前が開けた。あなたは森の中の広場にいる。
丈の短い、柔らかそうな草が一面に広場を覆っている。
道が東に伸びている。"
     (e . 2))))

;;
;; 継続を登録するためのハッシュテーブル
;;
(define *conts* (make-hash-table 'eqv?))

(define *max-id* (expt 2 64))  ;; 2 の 64乗

;;
;;
(define (push-cont! cont)
  (let1 cid (random-integer *max-id*)
    (cond ((hash-table-get *conts* cid #f) (push-cont! cont))
          (else (hash-table-put! *conts* cid cont) cid))))

(define (get-cont params)
  (hash-table-get *conts*
                  (cgi-get-parameter "c" params :convert string->number)
                  #f))

;;
;; Summery:
;;     セッションオブジェクトを呼び出し、sessionにセット。
;;     クエリの d 文字列（方向）を取得し、dir にセット。
;; Params:
;;     params -- (ex) http://localhost:8080/?s=123456&d=e
;;
;; (session (get-session params))
;;     Params: params -- 上の例
;;     Return: <session> -- セッションオブジェクト
;;
;; (dir (cgi-get-parameter "d" params :convert string->symbol))
;;     cgi-get-parameter
;;         -- "d=e" から、"e" を抽出して、dir に "e"をセットする
;;                      
;; ---------------------------------------------------------------------
;; (let ((session (get-session params))
;;       (dir (cgi-get-parameter "d" params :convert string->symbol)))
;;   )
;; ---------------------------------------------------------------------

;;
;; (ref session 'location) -- 現在のノード("説明" (s . 1))
;; ---------------------------------------------------------------------
;; (and-let* ((index (assoc-ref (cdr (ref session 'location)) dir)))
;;           (push! (ref session 'history) (ref session 'location))
;;           (set! (ref session 'location) (list-ref *dungeon* index)))
;; ---------------------------------------------------------------------


;;
;; render-content
;;
;; Summery:
;;
;; Params:
;;     path -- (ex) http://localhost:8000
;;     params -- s=12345&d=e
;; Return:
;;
;;
(define (render-content path params)
                                        ; 継続の起動
  (cond ((get-cont params) => (cut <> params))
                                        ; アプリケーションの起動
        (else (run-application params))))

(define (run-application params)
  (let loop ((location (list-ref *dungeon* 0))
             (history '()))
                                        ; rener-selector
                                        ; Summery:
                                        ;     リンク項目を作成する.
                                        ;     （例）・北へ進む
                                        ; Param:
                                        ;     selector -- (n . 0)
                                        ; Return:
                                        ;     <li><a href="localhost:8080?s=XXXXXXX&t=YYYYYYYYY&d=n">北へ進む</a></li>
    (define (render-selector selector)
      (let1 cid (push-cont! (lambda (params)
                              (loop (list-ref *dungeon* (cdr selector))
                                    (cons location history))))
        (html:li (html:a :href #`"?c=,|cid|"
                         (get-direction (car selector)) "へ進む")))
      (tree->string
       (html:html
        (html:head (html:title "simple httpd"))
        (html:body (html:p (html-escape-string (car location)))
                   (html:ul (map render-selector (cdr location)))
                   (html:hr)
                   (map (lambda (p) (html:p (html-escape-string (car p))))
                        (ref session 'history))))))))


(define (main args)
  (run-server)
  0)
