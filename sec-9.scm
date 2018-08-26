;;; -*- coding: utf-8 -*-
;;; sec-9.scm
;;; 第９章 状態の管理
;;;
;; 集合
;;
;;==================================================================
(define *inventory* (list 'potion 'potion 'dagger' cookie 'daggaer))

;;----------------------------------------------------------------
;; member -- elt が lis の中に含まれているかどうかを調べる
;; lis    -- リスト
;; options -- オプションでeltを調べるための手続きを指定できる。
;;            指定しない場合は、デフォルトで「equal?」を使う。
;; cmp-fn  -- オプションで指定した手続きは、このシンボルに
;;            束縛される。
;;
(define (member elt lis . options)
  (let-optionals* options ((cmp-fn equal?))
                  (cond ((null? lis) #f)
                        ((cmp-fn elt (car lis)) lis)
                        (else (member elt (cdr lis) cmp-fn)))))

;; let-optionals*
(define (proc x . args)
  (let-optionals* args ((a 'aa)
                        (b 'bbb)
                        (c 'cccc))
                  (list x a b c)))

(define (member2 elt lis . options)
  (let-optionals* options ((cmp-fn equal?))
                  (define (loop lis)
                    (cond ((null? lis) #f)
                          ((cmp-fn elt (car lis)) lis)
                          (else (loop (cdr lis)))))
                  (loop lis)))

;; itemが持ち物リスト*inventory*にあるかどうかを調べる
;; p111
;;(define (has-item? item)
;;  (member item *inventory*))

;; 持ち物リストのうち、同じものがあればそのうちの
;; ひとつだけを取り除く
(define (delete-1 elt lis . options)
  (let-optionals* options ((cmp-fn equal?))
                  (define (loop lis)
                    (cond ((null? lis) '())
                          ((cmp-fn elt (car lis)) (cdr lis))
                          (else (cons (car lis) (loop (cdr lis))))))
                  (loop lis)))

;; 練習問題 p112
;; delete-1のコピーしないバージョン
;; 考えてもわからんかったから、参考サイトを見た。
;; http://www.serendip.ws/archives/1953
(define (delete-one elt lis . options)
  (let-optionals* options ((cmp-fn equal?))
                  (define (loop lis)
                    (cond ((null? lis) lis)
                                        ; eltとリストの先頭が同じだったら、残りのリストを返す
                          ((cmp-fn elt (car lis)) (cdr lis))
                                        ; 残りのリストを調べて何もなかったら、
                                        ; つまり、残りのリストの調査結果と残りのリストが
                                        ; 同じだったら、
                                        ; 残りのリストを返す。
                          ((eq? (loop (cdr lis)) (cdr lis)) lis)
                                        ; eltとリストの先頭が同じではなかったら
                          (else
                                        ; 残りのリストを調べて、その結果と残りのリストが
                                        ; 同じだったら、すなわち、同じものがなかったら、
                           (if (eq? (loop (cdr lis)) (cdr lis))
                                        ; リストをそのまま返す。
                               lis
                                        ; 同じものがあったので、それをconsしておく。
                                        ; そして、リストの次の要素を調べる
                               (cons (car lis) (loop (cdr lis)))))))
                  (loop lis)))

;; 練習問題用のテストスクリプト
(use gauche.test)
(let ((data (list 1 2 3 4 5)))
  (test* "non-copy delete-one" data (delete-one 6 data) eq?))

;;-----------------------------------------------------------
;; 持ち物*inventory*からアイテムをひとつ取り除く手続き
;;
(define (delete-item! item)
  (set! *inventory* (delete-1 item *inventory*)))

;;---------------------------------------------------------------
;; 持ち物を*inventory*に追加する手続き
;;
(define (add-item! item)
  (set! *inventory* (cons item *inventory*)))

(define (add-item2! item)
  (push! *inventory* item))

;; p114
;; (assoc key '((key1 . value1) (key2 . value2) ...))
;; 見つかれば、(key . value) の対を返す
;; 見つからなければ、#f を返す
;;
(define (assoc key alist . options)
  (let-optionals* options ((cmp-fn equal?))
                  (define (loop alis)
                    (cond ((null? alis) #f)
                          ((cmp-fn key (caar alis)) (car alis))
                          (else (loop (cdr alis)))))
                  (loop alist)))


;; traverse -- 以下の引数を外部から受け取り、共通の骨組みにあてはめた
;;             手続きを返す
;; fallback -- リスト終端に達したときの値
;; get-key  -- 要素・キーと比較するために、現在のリストから値を
;;             取り出す動作
;; return   -- 比較が成功した場合に、現在のリストから返すべき値を
;;             創りだす操作
;; repeat   -- 比較が成功しない時に行う動作
;;
(define (traverse fallback get-key return repeat)
  (lambda (elt lis . options)
    (let-optionals* options ((cmp-fn equal?))
                    (define (loop lis)
                      (cond ((null? lis) fallback)
                            ((cmp-fn elt (get-key lis)) (return lis))
                            (else (repeat loop lis))))
                    (loop lis))))

(define member2
  (traverse #f car values
            (lambda (loop lis) (loop (cdr lis)))))

;; (define delete-1
;;   (traverse '() car cdr
;;             (lambda (loop lis) (cons (car lis) (loop (cdr lis))))))

(define assoc
  (traverse #f caar car
            (lambda (loop lis) (loop (cdr lis)))))

(define *item-database*
  '((potion (drink . #t) (throw . #t))
    (elixir (drink . #t) (throw . #t))
    (pancake (eat . #t) (throw . #t))
    (cookie (eat . #t) (throw . #t))
    (dagger (throw . #t))))

(define (item-properties item)
  (cond ((assoc item *item-database* ) => cdr)
        (else '())))

(define (item-property-get item prop)
  (cond ((assoc prop (item-properties item)) => cdr)
        (else #f)))

;; (define (use-item! what item)    ;; whatはdrink, eat, throwのどれか
;;   (cond ((not (has-item? item))
;;          (print item " を持っていません。"))
;;         ((not (item-property-get item what))
;;          (print item " を " what " することはできません。"))
;;         (else
;;          (print what " " item)
;;          (delete-item! item)))   ;; itemは使ってしまったので減らす
;;   #t)

(use util.match)

(define (make-player . args)
  (define (loop lis)
    (match lis
           (() '())
           ((attr value . rest) (cons (cons attr value) (loop rest)))
           (_ (error "Number of argument must be even:" args))))
  (loop args))

;; (define (make-player . args)
;;   (let loop ((lis args))
;;     (match lis
;;            (() '())
;;            ((attr value . rest) (cons (cons attr value) (loop rest))))))

(define *player*
  (make-player 'hp 320 'mp 66 'position #f
               'inventory '(posion posion dagger cookie dagger)))

(define (has-item? player item)
  (member item (cdr (assoc 'inventory player))))

;; playerにitemを追加する
;; p に playerのinventoryの対をセット '(inventory posion positon dagger ...)
;; pのcdr (posion posion ...) に item を追加
;;
(define (add-item! player item)
  (let ((p (assoc 'inventory player)))
    (push! (cdr p) item)))

;; playerのitemを削除する
;; set! はクオートされたリテラルデータも変更できてしまう。
;; 処理系によれば、エラーが出る。
;; もし、使いたくなければ、この下にある delete-item!を使う
;;
(define (delete-item! player item)
  (let ((p (assoc 'inventory player)))
    (set! (cdr p) (delete-1 item (cdr p)))))

;; 以下は、set-cdr! を使ったやり方
;; p121
;; p の cdr部を新しい値（リスト）で置き換える
;;
;; (define (add-item! player item)
;;   (let ((p (assoc 'inventory player)))
;;     (set-cdr! p  (cons item (cdr p)))))

;; (define (delete-item! player item)
;;   (let ((p (assoc 'inventory player)))
;;     (set-cdr! p (delete-1 item (cdr p)))))

;; player の体力の取得
;; (hp . 320) の対を表示
;;
(define (get-hp player)
  (cdr (assoc 'hp player)))

;; player の体力の変更 (+n)
;; (hp . 320) の cdr部を +n
;;
(define (add-hp! player n)
  (let ((p (assoc 'hp player)))
    (set! (cdr p) (+ n (cdr p)))))

;; ==========================================
;;                                       p122
;; 抽象化
;;
;; player の属性を取り出す
;;
(define (get-player-attr player attr)
  (cdr (assoc attr player)))

;; player の属性を変更する
;; updater -- 更新の方法
;;
(define (update-player-attr! player attr updater)
  (let ((p (assoc attr player)))
    (set! (cdr p) (updater (cdr p)))))

;; player の持ち物を表示
;;
(define (get-inventory player)
  (get-player-attr player 'inventory))

;; player が item を持っているか調べる
;;
(define (has-item? player item)
  (member item (get-inventory player)))

;; player から item を取り去る
;;
(define (delete-item! player item)
  (update-player-attr! player 'inventory (cut delete-1 item <>)))

;; player に item を追加する
;;
(define (add-item! player item)
  (update-player-attr! player 'inventory (cut cons item <>)))

(define (get-hp player)
  (get-player-attr player 'hp))

(define (add-hp! player n)
  (update-player-attr! player 'hp (cut + n <>)))

(define (get-mp player)
  (get-player-attr player 'mp))

(define (add-mp! player n)
  (update-player-attr! player 'mp (cut + n <>)))

(define (get-position player)
  (get-player-attr player 'position))

(define (set-position! player pos)
  (update-player-attr! player 'position (lambda (_) pos)))

;; p125
;; 準クォートを使ってアイテムデータベースにアクション手続きを埋め込む
(define *item-database*
  `((potion (drink . ,(cut add-hp! <> 50))
            (throw . ,(cut add-hp! <> -3)))
    (elixir (drink . ,(cut add-mp! <> 50))
            (throw . ,(cut add-mp! <> -3)))
    (pancake (eat  . ,(cut add-hp! <> 30))
             (throw . ,(cut add-hp! <> -2)))
    (cookie (eat   . ,(lambda (player)
                        (sys-system "fortune -s")
                        (add-hp! player 7)))
            (throw . ,(cut add-hp! <> -1)))
    (dagger (throw . ,(lambda (_) #f)))))

             
;; p126
;; use-item!でアイテムのアクションを起動するようにする
;; （例）
;; (get-hp *player*)          ==> 320
;; (use-item! 'eat 'cookie)   ==> #t
;; (get-hp *player*)          ==> 327
;; (use-item! 'eat 'cookie)   ==> cookieを持っていません。
;;
(define (use-item! what item)    ;; whatはdrink, eat, throwのどれか
  (cond ((not (has-item? *player* item))
         (print item " を持っていません。"))
        ((item-property-get item what)
         => (lambda (action)
              (delete-item! *player* item)
              (action *player*)))
        (else
         (print item " を " what " することはできません。")))
  #t)

;; p127
;; ダンジョンをつくる
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

;; 現在位置の説明を表示する
;;
(define (describe)
  (print (car (get-position *player*)))
  #t)

;; (move! direction) -- その方向に移動。移動できれば、その場所の説明が表示される。
;;                      移動できなければ、「移動できません」と表示。
;;
;; list-ref -- listのn番目の要素を返す（先頭は0番目）
;; assoc key list -- listの中にkeyが見つかれば、その対を返す
;;         このときのlistは ((key1 . value1)(key2 . vaalue2) ...)
;; position -- *dengeon*の中にある一つの要素。("説明" (方向 . ノード番号)...)
;; cdr position -- (方向 . ノード番号)(方向 . ノード番号)...
;; (assoc direction (cdr position)) -- 方向が(方向 . ノード番号)(方向 . ノード番号)の
;;          組の中に見つかれば、その(方向 . ノード番号)を返す
;; (cdr p) -- (方向 . ノード番号)のcdr要素、すなわち、ノード番号
;;
(define (move! direction)
  (let ((position (get-position *player*)))
    (cond ((assoc direction (cdr position))
           => (lambda (p)
                (set-position! *player* (list-ref *dungeon* (cdr p)))
                (describe)))
          (else
           (print "そちらには移動できません。"))))
  #t)

;; 現在のパラメータを表示する
;;
(define (status)
  (print "hp : " (get-hp *player*))
  (print "mp : " (get-mp *player*))
  (print "inventory : " (get-inventory *player*))
  #t)

;; パラメータを初期値に戻す
;;
(define (reset)
  (set! *player*
        (make-player 'hp 320 'mp 66 'position (car *dungeon*)
                     'inventory '(potion potion dagger cookie dagger)))
  #t)

