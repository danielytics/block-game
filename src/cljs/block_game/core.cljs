(ns ^:figwheel-always block-game.core
    (:require
              [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(def NUM_ROWS 12)
(def NUM_COLS 7)

(defonce app-state (atom nil))

(defn enumerate
  [s]
  (map-indexed vector s))


(defn new-game
  []
  (->> (for [row (range NUM_ROWS)
             col (range NUM_COLS)]
         (if (= row col)
           :red
           :empty))
       (partition NUM_COLS)
       (into [])))


(defn reset-game
  []
  (swap! app-state
         (fn [state]
           {:score    0
            :tick     0
            :board    (new-game)
            :running  true})))


#_(defn need-new-block?
  [board]
  (->>
    board
    ; Flip
    (apply map vector)
    (map
      #(some 
        (fn [[a b]] (println "x") (and (not= a :empty)
                            (= b :empty)))
        (partition 2 1 %)))
    (some identity)
    not))


(defn transpose
  [board]
  (apply map vector board))


(defn apply-gravity
  [board]
  (for [col board]
    (->>
      (reduce
        (fn [[top & stack] cur]
          (if (= top :empty)
            (conj stack cur :empty)
            (conj stack top cur)))
        [:bottom]
        (reverse col))
      (drop-last 1))))


(defn process-blocks
  [board]
  (->> board
       transpose
       apply-gravity
       transpose
       (map
         (fn [col]
           col))))


(def intermediary transpose)


(defn do-tick
  [{:keys [running tick board] :as state}]
  (when running
    (assoc
      state
      :tick   (inc tick)
      :board  (let [new-board (process-blocks board)]
                (if (= new-board board)
                  (conj
                    (drop 1 board) 
                    (let [row (first board)]
                      (-> (take-last 3 row)
                          (conj :green)
                          (->> (concat (take 3 row))))))
                  new-board)))))


(defn game-board
  [board]
  [:div.game-board
    (for [[idx row] (enumerate board)]
      [:div.row
        {:key idx}
        (for [[idx col] (enumerate row)]
          [:div.block
            {:key idx
             :class (name col)}])])])


(defn score-board
  [score tick]
  [:div.score-board
    [:div.title
      "Block Game"]
    [:div.score
      score]
    [:div.tick tick]
    [:div.control
      [:button
        {:on-click reset-game}
        "New Game"]]])


(defn game-ui
  []
  [:div
    [game-board (:board @app-state)]
    [score-board (:score @app-state) (:tick @app-state)]
    (when (:running @app-state)
      [:div
        {:style {:float "left"}}
        [game-board (-> app-state deref :board transpose)]  
        [:div {:style {:clear "both"}}]
        [game-board (-> app-state deref :board transpose apply-gravity)]])])


(defonce init
  (js/setInterval
    #(swap! app-state do-tick)
    1000))

(reagent/render-component [game-ui]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
) 

