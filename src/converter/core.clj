(ns converter.core
  (:use [seesaw core])
  (:gen-class))

(def metric-data (hash-map :y -24, :z -21, :a -18, :f -15, :p -12, :n -9, :mu -6, :m -3, :c -2, :d -1,
                           :none 0,
                           :da 1, :h 2, :k 3, :M 6, :G 9, :T 12, :P 15, :E 18, :Z 21, :Y 24))

(defn get-prefix [unit]
  ;; Takes a metric unit (e.g. "kg") returns keyword of prefix (e.g. :k)
  (let [prefix (subs unit 0 (- (count unit) 1))]
    (if (clojure.string/blank? prefix)
      (keyword "none")
      (keyword prefix))))


(defn get-factor [from to]
  (let [from (get-prefix from)
        to (get-prefix to)]
    (let [difference (- (metric-data from) (metric-data to))]
      (Math/pow 10 difference))))

(defn set-output [data out]
  (let [words (clojure.string/split data #"\s")]
    (let [value (first words)
          from-unit (nth words 1)
          to-unit (last words)]
      (let [factor (get-factor from-unit to-unit)]
        (let [result (float (* (read-string value) factor))]
          (value! out (str value " " from-unit " * " factor " = " result " " to-unit)))))))

(defn add-behaviours [root]
  (listen (select root [:#input])
          :action (fn [e] (set-output (value (select root [:#input])) (select root [:#output]))))
  root)

(defn make-frame []
  (frame
    :title  "Converter - By Andreas From"
    :width  400
    :height 200
    :on-close :exit
    :content (grid-panel
               :vgap 25
               :hgap 25
               :columns 1
               :rows 2
               :items [(text :id :input
                             :halign :center)
                       (label :id :output
                              :halign :center
                              :text "Example: 0.23 cm in m")])))

(defn -main []
  (native!)
  (-> (make-frame)
    add-behaviours
    show!))
