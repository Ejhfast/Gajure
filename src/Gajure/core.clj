(ns gajure.core)

(comment
  "This is a framework for creating genetic algorithms in clojure"
  "It requires the user to create a few specific operators, but otherwise works generically on sequences.")

(defn roulette-select
  "Select num individuals from pop, with an individual's fitness porportional to selection likelihood."
  [pop fit-fn num]
  (let [pop-fits (map fit-fn pop)
        inc-fits (iterate (fn [[pfit idx]]
                             [(+ (nth pop-fits (inc idx)) pfit) (inc idx)])
                          [(first pop-fits) 0])
        max-fitness (apply + pop-fits)
        pick-one (fn [num] (second (first (drop-while #(< (first %) num) inc-fits))))]
    (map (fn [x] (nth pop (pick-one (rand-int max-fitness)))) (range num))))

(defn do-crossover
  "Apply cross-fn to plist, partitioned into groups of num-parents. For instance,
traditional 2-parent crossover requires that num-parents equal 2."
  [p-list cross-fn num-parents]
  (map cross-fn (partition num-parents p-list)))

(defn keys-not-nil [lst hash]
  "For each key in a list of keys, make sure that is is not nil in the hash"
  (reduce #(and %1 %2) (map #(not (nil? (hash %))) lst)))

(defn run-ga
  "Pass two maps, one for functions, the other for settings.
For funcmap -- init-fn: takes one argument (a number) and initializes population.
fit-fn: takes a population member and outputs fitness
mut-fn: takes a population, and returns a mutated population.
sel-fn: takes a population, a fitness function, and a number to select. Returns selected members.
cross-fn: takes a list of vectors, each vector containing the parent to cross.
For setting-map -- pop-sz is size of population; gen is number of generations to run;
children is the number of children to create each generation; mut-r is the rate of mutation (0-100)"
  [func-map setting-map]
  {:pre [(and (keys-not-nil (list :init-fn :fit-fn :mut-fn :sel-fn :cross-fn) func-map)
              (keys-not-nil (list :pop-sz :gen :children :mut-r) setting-map))]}
  (let [ipop ((:init-fn func-map) (:pop-sz setting-map))]
    (loop [pop ipop
           num (:gen setting-map)]
      (if (zero? num)
        (do ;; insert information you would like printed here
          (println (first (sort-by (:fit-fn func-map) > pop ))))
            (let [total-left (- (:pop-sz setting-map) (:children setting-map))]
              (do ;; and here
                (println (first (sort-by (:fit-fn func-map) > pop ))))
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

;; Some helpers for using the framework

(defn list-crossover
  "A generic crossover function for simple lists. You may need to write your own."
  [[s1 s2]]
  (let [point (rand-int
               (min (count s1)
                    (count s2)))]
    (concat (take point s1)
            (drop point s2))))

(defn generic-mutation
  "Randomly mutates lists with elements from other lists in the population."
  [list prob]
  {:pre [(and (>= prob 1) (<= prob 100))]}
  (map
   (fn [s-list]
     (map
      (fn [test]
        (if (> prob (rand-int 100))
          (let [r-s (rand-int (count list))
                r-t (rand-int (count (nth list r-s)))]
            (nth (nth list r-s)
                 r-t))
          test))
      s-list))
   list))

(defn rand-from-list
  "Given a list of elements, construct a new list using the elements in that set.
Ex: (rand-from-list '(1 2 3 4 5) 2) => (3 5)"
  [lst num]
  (let [total-el (count lst)]
    (map (fn [x] (nth lst (rand-int total-el))) (range 0 num))))

(defn rand-pop
  "Creates a population using rand-from list. Helpful for creating init-fn."
  [lst num num-pop]
  (map (fn [x] (rand-from-list lst num)) (range 0 num-pop)))

