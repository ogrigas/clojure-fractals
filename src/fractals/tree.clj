(ns fractals.tree
  (:require [quil.core :refer :all]
            [quil.middlewares.fun-mode :refer :all]))

;; immutable data & pure functions

(def max-depth 12)

(defn tree-root [x y]
  [{:depth    1
    :branches [{:start  [x y]
                :end    [x (- y 100)]
                :angle  (- (/ PI 2))}]}])

(defn branch-from [[x y] length angle]
  {:start  [x y]
   :end    [(+ x (* (cos angle) length))
            (+ y (* (sin angle) length))]
   :angle  angle})

(defn outgrowths [branch]
  (let [[x1 y1] (:start branch)
        [x2 y2] (:end branch)
        length (dist x1 y1 x2 y2)
        angle (:angle branch)]
    [(branch-from [x2 y2] (* length 0.8) (- angle (/ PI 9)))
     (branch-from [x2 y2] (* length 0.7) (+ angle (/ PI 6)))]))

(defn grow [tree]
  (conj tree (-> (last tree)
                 (update-in [:depth] inc)
                 (update-in [:branches] #(mapcat outgrowths %)))))

;; mutations & side effects

(defn setup! []
  (smooth)
  (stroke 0 0 0)
  (stroke-weight 2)
  (fill 0 0 0)
  (frame-rate 6)
  (tree-root (/ (width) 2) (- (height) 50)))

(defn draw! [tree]
  (background 255 255 255)
  (doseq [level tree, branch (:branches level)]
    (let [depth (:depth level)
          [x1 y1] (:start branch)
          [x2 y2] (:end branch)]
      (stroke (* depth 20) 0 0)
      (line x1 y1 x2 y2))))

(sketch :title "Tree Fractal"
        :setup setup!
        :draw draw!
        :update (fn [tree] (if (< (count tree) max-depth) (grow tree) tree))
        :size [800 500]
        :middleware [fun-mode])
