(ns looping-is-recursion)

(defn power [base exp]
(let [ p (fn [acc exp]
(cond
(= exp 0)
1
(= exp 1)
acc
:else
(recur (* acc base) (dec exp))))]
(p base exp)))


(defn last-element [a-seq]
(if (empty? (rest a-seq))
(first a-seq)
(recur (rest a-seq))))

(defn seq= [seq1 seq2]
(cond
(and
(empty? seq1)
(empty? seq2))
true
(or
(empty? seq1)
(empty? seq2)
(not=
(first seq1)
(first seq2)))
false
:else
(recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
(loop [a-seq a-seq index 0]
(cond
(empty? a-seq)
nil
(pred (first a-seq))
index
:else
(recur (rest a-seq) (inc index)))))

(defn avg [a-seq]
(if (empty? a-seq)
nil
(loop [a-seq a-seq sum 0 amount 0]
(if (empty? a-seq)
(/ sum amount)
(recur (rest a-seq) (+ sum (first a-seq)) (inc amount))))))

(defn parity [a-seq]
(loop [a-seq a-seq set #{}]
(cond
(empty? a-seq)
set
(contains? set (first a-seq))
(recur (rest a-seq) (disj set (first a-seq)))
:else
(recur (rest a-seq) (conj set (first a-seq))))))

(defn fast-fibo [n]
(loop [n n fib-2 0 fib-1 1]
(if (< n 1)
fib-2
(recur (dec n) fib-1 (+ fib-2 fib-1)))))

(defn cut-at-repetition [a-seq]
(loop [a-seq a-seq result []]
(if
(or
(empty? a-seq)
(some (partial = (first a-seq)) result))
result
(recur (rest a-seq) (conj result (first a-seq))))))

