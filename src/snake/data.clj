(ns snake.data)

(defn initial-snake [x y len]
  [[(+ x len) y] [x y]])

(defn get-x [p] (p 0))
(defn get-y [p] (p 1))

(def head first)
(def tail last)

(defn but-last-tail [snake] (nth snake (- (count snake) 2)))

(defn segments [snake]
  (partition 2 1 snake))

(defn move-point [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn sign [x]
  (cond
    (< x 0) -1
    (> x 0) 1
    :else 0))

(defn head-segment [snake]
  (first (segments snake)))

(defn tail-segment [snake]
  (last (segments snake)))

(defn add-head [snake p]
  (into [p] snake))

(defn get-direction [[[xf yf] [xb yb]]]
  [(sign (- xf xb)) (sign (- yf yb))])

(defn change-head [snake h]
  (assoc snake 0 h))

(defn change-tail [snake t]
  (assoc snake (dec (count snake)) t))

(defn normalize [snake]
  (if (= (tail snake) (but-last-tail snake))
    (vec (butlast snake))
    snake))

(defn abs [n] (max n (- n)))

(defn collides? [snake [px py] tolerance]
  (let [[hx hy] (head snake)]
    (and (< (abs (- px hx)) tolerance)
         (< (abs (- py hy)) tolerance))))

(defn advance [snake]
  (let [head-dir (get-direction (head-segment snake))
        tail-dir (get-direction (tail-segment snake))
        new-head (move-point (head snake) head-dir)
        new-tail (move-point (tail snake) tail-dir)]
    (-> snake
        (change-head new-head)
        (change-tail new-tail)
        normalize)))

;(def snake [[10 10] [10 7] [7 7]])