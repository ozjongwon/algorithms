(ns algorithms.stack)

(doseq [in (clojure.string/split "to be or not to - be - - that - - - is " #" ")]
  (if (= in "-")
    (println :pop)
    (println :push in)))
