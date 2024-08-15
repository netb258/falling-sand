(ns falling-sand.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:gen-class))

;; Window size.
(def WIDTH 700)
(def HEIGHT 700)

(def SQUARE-WIDTH 7)
(def SQUARE-HEIGHT 7)

;; Returns a single vector populated with 0s.
(defn make-matrix-row []
  (into (vector) (take (/ WIDTH SQUARE-WIDTH) (repeat 0))))

;; Returns a matrix that looks something like this:
;; [[0 0 0 0 0]
;;  [0 0 0 0 0]
;;  [0 0 0 0 0]
;;  [0 0 0 0 0]
;;  [0 0 0 0 0]]
(defn make-matrix []
  (into (vector) (take (/ HEIGHT SQUARE-HEIGHT) (repeat (make-matrix-row)))))

(defn setup []
  (q/frame-rate 30)
  (q/background 0)
  ;; (q/stroke 255)
  ;; Here we return the initial state that the other function(update-state, draw-state!, etc...) will work with.
  (make-matrix))

(defn print-row!
  [arr row-num]
  (doall
    (map-indexed
      (fn [idx item]
        ;; (q/stroke 255)
        (if (= item 1) (q/fill 255) (q/fill 40 40 80))
        (q/rect (* idx SQUARE-WIDTH) (* row-num SQUARE-HEIGHT) SQUARE-WIDTH SQUARE-HEIGHT))
      arr)))

(defn print-matrix!
  [matrix offset]
  (doall
    (map-indexed (fn [idx item] (print-row! item (+ idx offset))) matrix)))

(defn draw-state! [state]
  (print-matrix! state 0))

(defn map-indexed-v [f coll]
  (into (vector) (map-indexed f coll)))

(defn move-grain
  [matrix-atom row-origin col-origin row-des col-des]
  (swap!
    matrix-atom
    (fn [state]
      (->
        state
        (update-in [row-origin col-origin] (constantly 0))
        (update-in [row-des col-des] (constantly 1))))))

;; NOTE: Tried doing this with double nested map-indexed, but it was awkward.
(defn update-state [state]
  (let [matrix (atom state)]
    ;; Here we're looping through a nexted list of rows and indexes like this: (((0 0 0 1 0 0) 0) ((0 1 0 0 0 0) 1) ((1 0 0 1 0 1) 2))
    (doseq [[row row-idx] (map list @matrix (range (count @matrix)))]
    ;; Same, but for each individual col.
      (doseq [[col col-idx] (map list row (range (count row)))]
        (let [below (get-in @matrix [(inc row-idx) col-idx])
              below-left (get-in state [(inc row-idx) (dec col-idx)])
              below-right (get-in state [(inc row-idx) (inc col-idx)])]
          (if (= col 1)
            (cond
              (= below 0) (move-grain matrix row-idx col-idx (inc row-idx) col-idx)
              (= below-right 0) (move-grain matrix row-idx col-idx (inc row-idx) (inc col-idx))
              (= below-left 0) (move-grain matrix row-idx col-idx (inc row-idx) (dec col-idx)))))))
    @matrix))

;; We need the bounds check, because the user can drag the mouse way out of the grid.
(defn is-in-bounds? [row col matrix]
  (and
    (not (nil? row))
    (not (nil? col))
    (> (quot row SQUARE-WIDTH) -1)
    (> (quot col SQUARE-WIDTH) -1)
    (< (quot row SQUARE-WIDTH) (count matrix))
    (< (quot col SQUARE-WIDTH) (count (first matrix)))))

;; NOTE: The event info parameter is a hash-map like this: {:x 239, :y 322, :button :left}
;; NOTE: We are using the quot function here to get normal integer division.
(defn mouse-drag [state event-info]
  (let [x (:x event-info)
        y (:y event-info)]
    (if (is-in-bounds? x y state)
      (let [col (quot x SQUARE-WIDTH)
            row (quot y SQUARE-WIDTH)]
        (update-in state [row col] (constantly 1)))
      state)))

(defn mouse-click [state event-info]
  (let [x (:x event-info)
        y (:y event-info)]
    (let [col (quot x SQUARE-WIDTH)
          row (quot y SQUARE-WIDTH)]
      (update-in state [row col] (constantly 1)))))

(defn -main [& args]
  (q/defsketch falling-sand
    :title "Click and Drag the mouse"
    :renderer :java2d
    :features [:exit-on-close]
    :setup setup
    :draw draw-state!
    :size [WIDTH HEIGHT]
    :update update-state
    :mouse-clicked mouse-click
    :mouse-dragged mouse-drag 
    :middleware [m/fun-mode]))
