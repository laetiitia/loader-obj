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
    (if (seq ks)
      (recur (vec (rest ks)) (assoc res (first normals) (into [] (reduce concat (replicate n)))))
      res)))

(defn- transTextCoord
  "Transform :text_coord of the model into :text_coord of a mesh"
  [])

;---------------------------------------
;-------------- TRANSFORM --------------
;---------------------------------------
(defn transformModel
  "Transform model of loader into a correct mesh"
  [model]
  (let [n (transNormal (get :normals model) (count (get (get model :faces) :f0)))]))
