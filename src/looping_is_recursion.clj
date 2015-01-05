(ns looping-is-recursion)

(defn power [base exp]
  (let [f (fn [a e]
            (if (zero? e)
              a
              (recur (* a base) (dec e))))]
    (f 1 exp)))

(defn last-element [a-seq]
  (cond
    (empty? a-seq) nil
    (empty? (rest a-seq)) (first a-seq)
    :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
    :else false))


(defn find-first-index [pred a-seq]
  (loop [i 0 s a-seq]
    (cond
      (empty? s) nil
      (pred (first s)) i
      :else (recur (inc i) (rest s)))))

(defn avg [a-seq]
  (let [[sum items] (loop [s 0 i 0 xs a-seq]
                      (if (empty? xs)
                        [s i]
                        (recur (+ s (first xs)) (inc i) (rest xs))))]
    (/ sum items)))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [rec #{} xs a-seq]
    (if (empty? xs)
      rec
      (recur (toggle rec (first xs)) (rest xs)))))

(defn fast-fibo [n]
  (if (<= n 0)
    0
    (loop [an 1 x 0 y 1]
      (if (= an n)
        y
        (recur (inc an) y (+ x y))))))


(defn cut-at-repetition [a-seq]
  (loop [xs a-seq res [] rec #{}]
    (cond
      (empty? xs) res
      (contains? rec (first xs)) res
      :else (recur (rest xs) (conj res (first xs)) (conj rec (first xs))))))

