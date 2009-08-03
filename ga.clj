(ns gajure
  (:use [clojure.contrib.seq-utils])
  (:import [java.util.Random]))

(comment
  "This is a framework for creating genetic algorithms in clojure"
  "It requires the user to create a few specific operators, but otherwise works generically on sequences.")

(defn roulette-select [pop fit-fn num]
  (let [pop-fits (map fit-fn pop)
        inc-fits (iterate (fn [[pfit idx]]
                             [(+ (nth pop-fits (+ idx 1)) pfit) (+ idx 1)])
                          [(first pop-fits) 0])
        max-fitness (apply + pop-fits)
        pick-one (fn [num] (second (first (drop-while #(< (first %) num) inc-fits))))]
    (map (fn [x] (nth pop (pick-one (rand-int max-fitness)))) (range num))))

(defn do-crossover [pop cross-fn num-parents]
  (map cross-fn (partition num-parents pop)))

(defn run-ga
  "Pass two maps, one for functions, the other for settings.
For funcmap -- init-fn: takes one argument (a number) and initializes population.
fit-fn: takes a population member and outputs fitness
mut-fn: takes a population, and returns a mutated population.
sel-fn: takes a population, a fitness function, and a number to select. Returns selected members.
For setting-map -- pop-sz is size of population; gen is number of generations to run;
children is the number of children to create each generation; mut-r is the rate of mutation (0-100)"
  [func-map setting-map]
  (let [ipop ((:init-fn func-map) (:pop-sz setting-map))]
    (loop [pop ipop
           num (:gen setting-map)]
      (if (zero? num)
        (do
          (println (map (:fit-fn func-map) pop))
          (first (sort-by (:fit-fn func-map) > pop )))
            (let [total-left (- (:pop-sz setting-map) (:children setting-map))]
              (println (map (:fit-fn func-map) pop))
              (recur
               (concat
                ((:mut-fn func-map)
                 (do-crossover
                  ((:sel-fn func-map)
                   pop
                   (:fit-fn func-map)
                   (* (:children setting-map) 2))
                  (:cross-fn func-map)
                  2)
                 (:mut-r setting-map))
                ((:init-fn func-map) total-left))
               (dec num)))))))
