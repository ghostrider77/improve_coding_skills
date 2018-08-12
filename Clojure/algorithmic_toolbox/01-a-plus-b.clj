
(defn add-two-numbers [a b]
    (+ a b))

(defn -main []
    (def a (Integer/parseInt (read-line)))
    (def b (Integer/parseInt (read-line)))
    (println (add-two-numbers a b)))

(-main)
