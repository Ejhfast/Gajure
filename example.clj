(ns gajure
  (:load-file "ga.clj"))

(comment
  "This provides an example use of the GA framework. We evolve the string helloworld.")

(def dna (map str
              ['q 'w 'e 'r 't 'y 'u 'i 'o 'p 'a 's 'd 'f 'g 'h 'j 'k 'l
               'z 'x 'c 'v 'b 'n 'm]))

(defn print-nicely
     [char-lst]
     (reduce str char-lst))

(defn hello-fitness
     [lst]
     (reduce
      +
      (map #(if (= %1 %2) 1 0)
           lst
           '("h" "e" "l" "l" "o" "w" "o" "r" "l" "d"))))

(def func-map {:fit-fn hello-fitness :mut-fn generic-mutation
               :sel-fn roulette-select :init-fn (partial rand-pop dna 10)
               :cross-fn list-crossover})

(def set-map {:pop-sz 100 :children 50 :mut-r 1 :gen 100})

(defn run-example []
     (run-ga func-map set-map))