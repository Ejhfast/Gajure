(ns gajure
  (:use [clojure.contrib.seq-utils])
  (:import [java.util.Random]))

(defn random-int [x]
  (let [rnd (new java.util.Random)]
    (try
     (.nextInt rnd x)
     (catch Exception _
      0))))

(defn apply-mutation [list mut-fn & [prob]]
  (let [mut-with-prob (fn [x] (mut-fn x prob))]
    (map mut-with-prob list)))

(defn roulette-select [pop fit-fn num]
  (let [pop-fits (map fit-fn pop)
        inc-fits (iterate (fn [[pfit idx]]
                             [(+ (nth pop-fits (+ idx 1)) pfit) (+ idx 1)])
                          [(first pop-fits) 0])
        max-fitness (apply + pop-fits)
        pick-one (fn [num] (second (first (drop-while #(< (first %) num) inc-fits))))]
    (map (fn [x] (nth pop (pick-one (random-int max-fitness)))) (range num))))

(defn do-crossover [pop cross-fn num-parents]
  (map cross-fn (partition num-parents pop)))

(defn run-ga
  [func-map setting-map]
  ;; I need to fix the 2*parents issue....
  (let [ipop ((:init-fn func-map) (:pop-sz setting-map))]
    (loop [pop ipop
           num (:gen setting-map)]
      (if (zero? num)
        (do
          (println (map (:fit-fn func-map) pop))
          (first (sort-by (:fit-fn func-map) > pop )))
            (let [total-left (- (:pop-sz setting-map) (:parents setting-map))]
              (println (map (:fit-fn func-map) pop))
              (recur
               (concat
                ((:mut-fn func-map)
                 (do-crossover
                  ((:sel-fn func-map)
                   pop
                   (:fit-fn func-map)
                   (* (:parents setting-map) 2))
                  (:cross-fn func-map)
                  2)
                 (:mut-r setting-map))
                ((:init-fn func-map) total-left))
               (dec num)))))))
