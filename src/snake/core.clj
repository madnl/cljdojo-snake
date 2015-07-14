(ns snake.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [snake.data :refer :all]))

(def scale-factor 5)
(def height 100)
(def width 100)

(defn gen-food []
  [(rand-int height) (rand-int width)])

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 45)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :rgb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:snake (initial-snake 100 100 100)
   :food (gen-food)})

(defn update-state [{snake :snake food :food}]
  ; Update sketch state by changing circle color and position.
  (if (collides? snake food scale-factor)
    (let [increment (inc (rand-int 20))]
      {:snake (nth (iterate advance snake) increment)
       :food (gen-food)})
    {:snake (advance snake)
     :food food}))

(defn draw-segment [[x1 y1] [x2 y2]]
  (q/line x1 y1 x2 y2))

(defn draw-snake [snake]
  (doseq [[p1 p2] (segments snake)]
    (draw-segment p1 p2)))

(defn draw-state [{snake :snake [x-food y-food] :food}]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 0)
  ; Set circle color.
  (q/stroke 255 255 255)
  (q/stroke-weight scale-factor)
  ; Calculate x and y coordinates of the circle.
  (q/scale 2)
  (draw-snake snake)
  (q/point x-food y-food))

(def vectors {:up [0 -1]
              :down [0 1]
              :left [-1 0]
              :right [1 0]})

(defn steer-point [p dir]
  (if-let [v (get vectors dir)]
    (move-point p v)
    p))

(defn steer-snake [snake dir]
  (let [new-head (steer-point (head snake) dir)]
    (add-head snake new-head)))

(defn handle-key [state key]
  (let [snake (:snake state)]
    ;(println key)
    (assoc state :snake (steer-snake snake (:key key)))))

(q/defsketch snake
  :title "SNAKE"
  :size [(* height scale-factor) (* width scale-factor)]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :key-pressed handle-key
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
