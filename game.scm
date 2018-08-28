;;; -*- coding: utf-8 -*-
;;; game.scm

(use util.list)  ; リスト処理の補助ユーティリティ
(use srfi-27)    ; 疑似乱数発生器を提供するモジュール

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

(define-class <session> ()
  ((sid      :init-keyword :sid)                  ; セッションID
   (location :init-value (list-ref *dungeon* 0))  ; 現在のノード
   (history  :init-value '())                     ; 訪れたノードの履歴
   ))

;; 整数をキーとしたハッシュテーブルを作成
(define *sessions* (make-hash-table 'eqv?))       ; アクティブなセッション

;; get-session
;;
;; Params:
;;     params -- (ex) http://localhost:8080/?s=123456
;;
;; Summery:
;;     テーブル *sessions* からキー（sの値=sid）の値（オブジェクト）を
;;     探し出し、もしなければmake-session を実行する。
;;
;;     cgi-get-parameter
;;         -- クエリ文字列から s の値を取得し整数とする
;;            もし見つからなければ #f を返す
;;     hash-table-get
;;         -- テーブル *sessions* から cgi-get-parameter の結果の整数を
;;            キーとした値を探す。もし、cgi-get-parameter が #f を返して
;;            きたら、#f をキーとした値はないので、既定値として第3引数
;;            （この場合は #f）を返す。
;;     #f -- キーが見つからない場合の返り値
;;
(define (get-session params)
  (or (hash-table-get *sessions*
                      (cgi-get-parameter "s" params :convert string->number)
                      #f)
      (make-session)))

(define *max-id* (expt 2 64))  ;; 2 の 64乗

;; make-session
;;
;; Summery:
;;     ランダム関数で sid（セッションID） を生成する。それが既存のIDで
;;     あれば、make-session のやり直し。
;;     IDに重複が無ければ、sid をスロットにもつ新たなセッションオブジェクト
;;     を作成し（sessに束縛）、それを *sessions* テーブルに登録する。
;;     最後にその sess を返す。
;;
(define (make-session)
  (let1 sid (random-integer *max-id*)
    (cond ((hash-table-get *sessions* sid #f) (make-session))
          (else (let1 sess (make <session> :sid sid)
                  (hash-table-put! *sessins* sid sess)
                  sess)))))


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
(let ((session (get-session params))
      (dir (cgi-get-parameter "d" params :convert string->symbol)))
  )

;;
(and-let* ((index (assoc-ref (cdr (ref session 'location)) dir)))
          (push! (ref session 'history) (ref session 'location))
          (set! (ref session 'location) (list-ref *dungeon* index)))
