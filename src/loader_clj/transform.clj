(ns transform.core
  (:require [clojure.string :as s]
            [clojure.pprint :as p]))

;---------------------------------------
;-------------- STRUCTURE --------------
;---------------------------------------

(def ^:private normals
  {})

(def ^:private faces
  {})

(def ^:private text_coord
  {})

;---------------------------------------
;-------------- FUNCTIONS --------------
;---------------------------------------
(defn- vec-remove
  "Remove an element in coll (thanks to a position)"
  [pos coll]
  (vec (concat (subvec (into [] coll) 0 pos) (subvec (into [] coll) (inc pos)))))

(defn- getV
  "Get nodes of a face"
  [f]
  (map #(first %) f))

(defn- getVt
  "Get coordinates textures of a face"
  [f]
  (into [] (map #(first (rest %)) f)))

(defn- getVn
  "Get normals of a face"
  [f]
  (into [] (map #(first (rest (rest %))) f)))

(defn- transNormal
  "Transform :normals of the model into :normals of a mesh"
  [normals n]
  (loop [ks (vec (keys normals)), res {}]
    (if (seq? ks)
      (recur (vec (rest ks)) (assoc res (first normals) (into [] (reduce concat (repeat n (get ks normals))))))
      res)))


(defn- transTextCoord
  "Transform :text_coord of the model into :text_coord of a mesh"
  [])

(defn- transFaces
  "Transform :faces of the model into :faces of a mesh"
  ([mapFaces] (transFaces (vals mapFaces) faces))
  ([list res]
    (if (seq list)
      (let [s (getV (first list))
            f (keyword (str "f" (first (getVn (first list)))))
            newList (vec-remove 0 list)]
        (if (contains? res f)
          (transFaces newList (assoc res f (into [] (concat s (get res f)))))
          (transFaces newList (assoc res f (into [] s)))))
      res)))



;---------------------------------------
;-------------- TRANSFORM --------------
;---------------------------------------
(defn transformModel
  "Transform model of loader into a correct mesh"
  [model]
  (let [n (transNormal (get :normals model) (count (get (get model :faces) :f0)))]))

(first (vals {:f8 [[7 6 3] [5 12 3] [6 7 3]],
             :f1 [[3 2 2] [8 4 2] [4 5 2]],
             :f10 [[1 3 5] [3 2 5] [4 5 5]],
             :f5 [[5 12 6] [2 9 6] [6 7 6]],
             :f11 [[5 12 6] [1 3 6] [2 9 6]],
             :f3 [[2 9 4] [8 10 4] [6 11 4]],
             :f2 [[7 6 3] [6 7 3] [8 8 3]],
             :f0 [[5 1 1] [3 2 1] [1 3 1]],
             :f7 [[3 2 2] [7 14 2] [8 4 2]],
             :f9 [[2 9 4] [4 5 4] [8 10 4]],
             :f6 [[5 1 1] [7 13 1] [3 2 1]],
             :f4 [[1 3 5] [4 5 5] [2 9 5]]} ))

(keyword (str "f" (first (getVn [[7 6 3] [5 12 3] [6 7 3]]))))
(vec-remove 0 [[[7 6 3] [5 12 3] [6 7 3]] [[3 2 2] [8 4 2] [4 5 2]] [[3 2 2] [8 4 2] [4 5 2]]])

(transFaces {:f8 [[7 6 3] [5 12 3] [6 7 3]],:f1 [[3 2 2] [8 4 2] [4 5 2]]})
