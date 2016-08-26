(ns test-clj.core
  (:gen-class))


(defn- make-row
  ([nb] (make-row nb 0))
  ([nb init]
   (vec (repeat nb init))))

(defn make-grid
  ([nbrow nbcol] (make-grid nbrow nbcol 0))
  ([nbrow nbcol init]
   (vec (repeat nbcol (make-row nbrow init)))))


(defn getbyxy
  ([coll x y]
   (getbyxy coll x y 0))

  ([coll x y err]
   (if (and (>= y 0) (>= x 0) (< y (.length coll)) (< x (.length (nth coll y))))
    (nth (nth coll y) x)
    err)))


(defn assocbyxy
  [coll x y val]
  (assoc coll y (assoc (nth coll y) x val)))

(defn aliveCell
  [grid [x y]]
  (assocbyxy grid x y 1))

(defn killCell
  [grid [x y]]
  (assocbyxy grid x y 0))

(defn initGrid
  [grid points]
  (reduce aliveCell grid points))

(defn nextStateCell
  [grid [x y]]
  (let [nbAlive
        (+ (getbyxy grid (dec x) y)
           (getbyxy grid (inc x) y)
           (getbyxy grid x (dec y))
           (getbyxy grid x (inc y))
           (getbyxy grid (dec x) (dec y))
           (getbyxy grid (dec x) (inc y))
           (getbyxy grid (inc x) (dec y))
           (getbyxy grid (inc x) (inc y)))]
       (or (= nbAlive 3) (and (= (getbyxy grid x y) 1) (= nbAlive 2)))))

(defn stepCell
  [previous]
  (fn [grid point]
      (if (nextStateCell previous point)
        (aliveCell grid point)
        (killCell grid point))))

(defn display-grid
  [grid]
  (doall (for [line grid]
          (println line))))

(defn step-grid
  [grid]
  (reduce (stepCell grid) grid (for [ y (range 0 (.length grid))
                                      x (range 0 (.length (nth grid 0)))]
                                    [x y])))

(defn stateLoop
  [grid continue?]
  (loop [ gr grid]
    (do
      (display-grid gr)
      (newline)
      (if (continue?)
        (recur (step-grid gr))
        nil))))

(defn countfunc 
  [number]
  (let [counter (atom number)]
    (fn [] (do (swap! counter dec) (> @counter 0)))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (do 
    (println args)
    (let [w (load-string (first args))
          h (load-string (second args))
          c (load-string (nth args 2))
          startPoints (load-string (last args))]
      (stateLoop  (-> (make-grid w h) 
                      (initGrid startPoints))
                  (countfunc c)))))
  
