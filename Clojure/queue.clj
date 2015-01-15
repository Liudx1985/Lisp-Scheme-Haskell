; solve n-queue x.
;test (load-file "d:/workspace/lispscripts/clojure/stm_account.clj")

; test the coll using for-each pos @ coll, test prec(pos, coll[pos]) 
; any-c any pred return true, then true, else false
; all-c all pred return true,  then true, else false
(defn abs [a]
	(if (< a 0) (- a) a))

(defn some-c [pred coll]
	(loop [i 0] 
		(when (< i (count coll))
			(if (pred i (nth coll i)) true
				(recur (inc i))
				)
			)
	))

