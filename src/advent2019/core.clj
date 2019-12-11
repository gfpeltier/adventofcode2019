(ns advent2019.core)

(defprotocol Predict
  (predict [model x]))

(defrecord Neuron [w b]
  Predict
  (predict [_ x] (-> x (* w) (+ b))))

(defn ctof [temp] (-> temp (* 1.8) (+ 32)))

(def train-data (mapv #(hash-map :c % :f (ctof %)) (range 100)))

(def test-data (mapv #(hash-map :c % :f (ctof %)) (range 0.5 100.5)))

(defn cost [real pred]
  (-> (- real pred)
      (Math/pow 2)
      (/ 2)))

(defn forward-prop [model data]
  (let [preds (map #(predict model (:c %)) data)]
    (vector preds (/ (reduce + (map #(cost (:f %1) %2) data preds))
                     (count preds)))))

(defn back-prop [predictions data]
  (-> (reduce (fn [dm der]
                (-> dm
                    (update :dw + (:dw der))
                    (update :db + (:db der))))
              {:dw 0 :db 0}
              (map (fn [pred actual]
                     {:dw (* (- (:f actual) pred)
                             (:c actual))
                      :db (- (:f actual) pred)})
                   predictions
                   data))
      (update :dw / (count predictions))
      (update :db / (count predictions))))


(defn train-model [{:keys [model epochs alpha data]}]
  (loop [epoch 0 curr-mod model hist []]
    (if (= epoch epochs)
      {:history hist
       :model curr-mod}
      (let [[preds cost] (forward-prop curr-mod data)
            {:keys [dw db]} (back-prop preds data)]
        (recur (inc epoch)
               (->Neuron (+ (:w curr-mod)
                            (* alpha dw))
                         (+ (:b curr-mod)
                            (* alpha db)))
               (conj hist cost))))))
