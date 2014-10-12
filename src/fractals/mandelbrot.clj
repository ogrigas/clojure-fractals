(ns fractal.mandelbrot
  (:require [quil.core :refer :all]))

(def max-iterations 100)

(def color-scheme [30 244 144])

(defn color-distance [[r g b]]
  (sqrt (+ (sq r) (sq g) (sq b))))

(defn avg [a b]
  (/ (+ a b) 2))

(defn random-color []
  [(rand 255) (rand 255) (rand 255)])

(def color-table
  (->> (for [_ (range (* max-iterations))]
         (map avg color-scheme (random-color)))
       (sort-by color-distance)
       (vec)))

(defn interpolate [t c0 c1]
  (+ (* (- 1 t) c0) (* t c1)))

(defn color-at [x0 y0]
  (loop [x 0, y 0, iteration 0]
    (if (and (< iteration (dec max-iterations))
             (< (+ (sq x) (sq y)) 4))
      (recur (+ x0 (sq x) (- (sq y)))
             (+ y0 (* 2 x y))
             (inc iteration))
      (if (< iteration (dec max-iterations))
        (let [zn (sqrt (+ (sq x) (sq y)))
              nu (/ (log (/ (log zn) (log 2))) (log 2))
              color1 (color-table iteration)
              color2 (color-table (inc iteration))]
          (map #(interpolate (- 1 nu) %1 %2) color1 color2))
        '(0 0 0)))))

(defn draw! []
  (smooth)
  (background 0)
  (doseq [x (range (width))
          y (range (height))]
    (let [rx (- (* x (/ 2.7 (width))) 1.7)
          ry (- (* y (/ 2.0 (height))) 1.0)]
      (set-pixel x y (apply color (color-at rx ry))))))

(sketch :title "Mandelbrot Fractal"
        :setup draw!
        :size [640 480])
