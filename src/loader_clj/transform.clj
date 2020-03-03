(ns transform.core)

;---------------------------------------
;-------------- FUNCTIONS --------------
;---------------------------------------


(defn- vec-remove
  "Remove an element in coll (thanks to a position)"
  [pos coll]
  (vec (concat (subvec (into [] coll) 0 pos) (subvec (into [] coll) (inc pos)))))

(defn- transNormal
  "Transform :normals of the model into :normals of a mesh"
  [normals n]
  (loop [ks (keys normals), res {}]
    (if (seq ks)
      (recur (rest ks) (assoc res (first ks) (into [] (reduce concat (repeat n (get normals (first ks)))))))
      res)))

(defn- getVt
  "Get coordinates textures of a face"
  [f]
  (into [] (map #(first (rest %)) f)))

(defn- getVn
  "Get normals of a face"
  [f]
  (into [] (map #(first (rest (rest %))) f)))

(defn- transFaces
  "Transform :faces of the model into :faces of a mesh"
  ([mapFaces] (transFaces (vals mapFaces) {}))
  ([list res]
   (if (seq list)
     (let [s (map #(first %) (first list)) ;Get nodes of a face
           f (keyword (str "f" (first (getVn (first list)))))
           newList (rest list)]
       (if (contains? res f)
         (transFaces newList (assoc res f (into [] (concat s (get res f)))))
         (transFaces newList (assoc res f (into [] s)))))
     res)))

(defn- addVt
  "Add coordinates texttures to the structure (maps)"
  [tc res vts kw]
  (if (seq vts)
    (addVt tc (assoc res kw (into [] (concat (get res kw) (get tc (keyword (str "vt" (first vts))))))) (rest vts) kw)
    res))

;(addVt {:vt0 [1 1] :vt1 [0 0]} {:vt3 [0 1 0 1]} [0 1 0] :vt2)


(defn- transTextCoord
  "Transform :text_coord of the model into :text_coord of a mesh"
  ([faces tc] (transTextCoord (vals faces) tc {}))
  ([list tc res]
   (if (seq list)
     (let [kw (keyword (str "vt" (first (getVn (first list)))))
           vts (getVt (first list))]
       (transTextCoord (rest list) tc (addVt tc res vts kw)))
     res)))

(defn- transFnT
  "Transform :faces and :text_coord of the model into a mesh"
  ([mapFaces textures] (transFnT textures (vals mapFaces) {} {}))
  ([tc list res coord]
   (if (seq list)
     (let [s (map #(first %) (first list)) ;Get nodes of a face
           f (keyword (str "f" (first (getVn (first list)))))
           newList (rest list)]
       (transFnT tc newList (assoc res f (into [] (concat s (get res f)))) (addVt tc coord s f)))
     [res coord])))

;---------------------------------------
;-------------- TRANSFORM --------------
;---------------------------------------


(defn transformModel
  "Transform model of loader into a correct mesh"
  [model]
  (let [[f t] (transFnT (get model :faces) (get model :text_coord))
        n (transNormal (get model :normals) (count (set (get f :f1))))]
    (-> model
        (assoc :faces f)
        (assoc :normals n)
        (assoc :text_coord t))))

(transformModel {:faces {:f8 [[7 6 3] [5 12 3] [6 7 3]]
                         :f1 [[3 2 2] [8 4 2] [4 5 2]]
                         :f10 [[1 3 5] [3 2 5] [4 5 5]]
                         :f5 [[5 12 6] [2 9 6] [6 7 6]]
                         :f11 [[5 12 6] [1 3 6] [2 9 6]]
                         :f3 [[2 9 4] [8 10 4] [6 11 4]]
                         :f2 [[7 6 3] [6 7 3] [8 8 3]]
                         :f0 [[5 1 1] [3 2 1] [1 3 1]]
                         :f7 [[3 2 2] [7 14 2] [8 4 2]]
                         :f9 [[2 9 4] [4 5 4] [8 10 4]]
                         :f6 [[5 1 1] [7 13 1] [3 2 1]]
                         :f4 [[1 3 5] [4 5 5] [2 9 5]]}
                 :normals {:vn0 [0.0 1.0 0.0]
                           :vn1 [0.0 0.0 1.0]
                           :vn2 [-1.0 0.0 0.0]
                           :vn3 [0.0 -1.0 0.0]
                           :vn4 [1.0 0.0 0.0]
                           :vn5 [0.0 0.0 -1.0]}
                 :text_coord {:vt3 [0.625 0.75]
                              :vt9 [0.625 0.25]
                              :vt10 [0.375 0.25]
                              :vt8 [0.625 0.0]
                              :vt12 [0.375 0.5]
                              :vt2 [0.875 0.75]
                              :vt7 [0.375 0.0]
                              :vt6 [0.375 1.0]
                              :vt0 [0.625 0.5]
                              :vt11 [0.125 0.5]
                              :vt5 [0.625 1.0]
                              :vt4 [0.375 0.75]
                              :vt13 [0.125 0.75]
                              :vt1 [0.875 0.5]}})

(transFnT  {:f8 [[7 6 3] [5 12 3] [6 7 3]]
            :f1 [[3 2 2] [8 4 2] [4 5 2]]
            :f10 [[1 3 5] [3 2 5] [4 5 5]]
            :f5 [[5 12 6] [2 9 6] [6 7 6]]
            :f11 [[5 12 6] [1 3 6] [2 9 6]]
            :f3 [[2 9 4] [8 10 4] [6 11 4]]
            :f2 [[7 6 3] [6 7 3] [8 8 3]]
            :f0 [[5 1 1] [3 2 1] [1 3 1]]
            :f7 [[3 2 2] [7 14 2] [8 4 2]]
            :f9 [[2 9 4] [4 5 4] [8 10 4]]
            :f6 [[5 1 1] [7 13 1] [3 2 1]]
            :f4 [[1 3 5] [4 5 5] [2 9 5]]}
           {:vt3 [0.625 0.75]
            :vt9 [0.625 0.25]
            :vt10 [0.375 0.25]
            :vt8 [0.625 0.0]
            :vt12 [0.375 0.5]
            :vt2 [0.875 0.75]
            :vt7 [0.375 0.0]
            :vt6 [0.375 1.0]
            :vt0 [0.625 0.5]
            :vt11 [0.125 0.5]
            :vt5 [0.625 1.0]
            :vt4 [0.375 0.75]
            :vt13 [0.125 0.75]
            :vt1 [0.875 0.5]})

