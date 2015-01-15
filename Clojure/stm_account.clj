;; define two accounts that we want to transfer money between:
;test (load-file "d:/workspace/lispscripts/clojure/stm_account.clj")
(def account-a (ref 2000))

(def account-b (ref 0))

;; launch 10000 tasks to transfer a random amount of money
;; each of which can happens on a different thread or core
;; each takes place inside a (dosync ....) transaction

(dotimes [i 100] 
  (future 
    (let [transfer (rand-int 100)]
      (dosync
       	(alter account-a - transfer)
        (alter account-b + transfer)
        (if (<= @account-a 0) (throw (Exception. "no money in account-a"))
        	(printf "[%d]account-a:%d, account-b:%d\n" i @account-a @account-b)
        	)
        ))))

;; a transactional read of the two accounts should then 
;; always have the same total amount, at any point in time
;; (even while the above operation is still running)
(Thread/sleep 100) ; wait the transfer finish
(dosync 
 (println @account-a @account-b (+ @account-a @account-b)))

