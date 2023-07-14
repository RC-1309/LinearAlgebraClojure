(defn eq-size? [args] (or (every? number? args) (apply = (mapv count args))))
(defn check-vector [v] (and (vector? v) (every? number? v)))
(defn check-vectors [args] (and (every? check-vector args) (eq-size? args)))
(defn opObj [f args] (apply mapv f args))
(defn opV [f args]
  {:pre [(check-vectors args)]
   :post [(vector? %)]}
  (opObj f args))
(defn v+ [& args] (opV + args))
(defn v- [& args] (opV - args))
(defn v* [& args] (opV * args))
(defn vd [& args] (opV / args))
(defn scalar [& args] (apply + (apply v* args)))
(defn vect [& args]
  {:pre [(check-vectors args)]
   :post [(check-vector %)]}
  (reduce (fn [[x1 x2 x3] [y1 y2 y3]] (vector (- (* x2 y3) (* x3 y2)) (- (* x3 y1) (* x1 y3)) (- (* x1 y2) (* x2 y1)))) args))
(defn v*s [v & c]
  {:pre [(check-vector v) (every? number? c)]
   :post [(check-vector %)]}
  (mapv (fn [elem] (* elem (reduce * c))) v))
(defn check-matrix [m] (check-vectors m))
(defn transpose [m] (opV vector m))
(defn check-matrices [args] (and (every? vector? args) (every? check-matrix args) (eq-size? args)))
(defn opM [f args]
  {:pre [(check-matrices args)]
   :post [(check-matrix %)]}
  (opObj f args))
(defn m+ [& args] (opM v+ args))
(defn m- [& args] (opM v- args))
(defn m* [& args] (opM v* args))
(defn md [& args] (opM vd args))
(defn m*s [m & c]
  {:pre [(check-matrix m) (every? number? c)]
   :post [(check-matrix %)]}
  (mapv (fn [v] (v*s v (reduce * c))) m))
(defn m*v [m v] (mapv (fn [vec] (scalar vec v)) m))
(defn m*m [& args] (reduce (fn [m1 m2] (mapv (fn [v] (m*v (transpose m2) v)) m1)) args))
(defn opT [f args]
  {:pre [(eq-size? args)]}
  (if (every? number? args) (apply f args)
    (if (check-vectors args) (opV f args) (apply mapv (fn [& args] (opT f args)) args))))
(defn t+ [& args] (opT + args))
(defn t- [& args] (opT - args))
(defn t* [& args] (opT * args))
(defn td [& args] (opT / args))
(defn get-form [t] (if (check-vector t) (vector (count t)) (concat (vector (count t)) (get-form (nth t 0)))))
(defn t-op-s [f t s] (if (check-vector t) (mapv (fn [elem] (f elem s)) t) (mapv (fn [v] (t-op-s f v s)) t)))
(defn inverse [f args] (if (number? args) (f args) (mapv (fn [elem] (inverse f elem)) args)))
(defn opTB [f t1 t2]
  {:pre [(or (number? t1) (number? t2) (eq-size? [t1 t2]))]}
  (if (number? t1) (if (number? t2) (f t1 t2) (inverse f (t-op-s f t2 t1)))
                   (if (number? t2) (t-op-s f t1 t2) (apply mapv (fn [t1 t2] (opTB f t1 t2)) [t1 t2]))))
(defn opTb [f args] (reduce (fn [t1 t2] (opTB f t1 t2)) args))
(defn tb+ [& args] (opTb + args))
(defn tb- ([arg] (inverse - arg)) ([arg & args] (opTb - (cons arg args))))
(defn tb* [& args] (opTb * args))
(defn tbd ([arg] (inverse / arg)) ([arg & args] (opTb / (cons arg args))))
