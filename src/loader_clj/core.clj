(ns loader-clj.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :as p])
  (:gen-class))



;---------------------------------------
;-------------- STRUCTURE --------------
;---------------------------------------

;; Initial model state
(def ^:private model
  {:texture-name ""
   :rbg []
   :weight 1
   :vertices {}
   :normals {}
   :faces {}
   :text_coord {}})


;---------------------------------------
;-------------- FUNCTIONS --------------
;---------------------------------------
(defn- add-to-model
  "Add a value in the model."
  ([model key val] ;;For :texture-name, rbg and weight doesn't change
   (case key
         :texture-name (assoc model key val)
         :rbg (assoc model key (conj (get model key) val))
         :else model))
  ([model k1 k2 val]
   (assoc model k1 (assoc (get model k1) k2 val)))) ;;For the maps of vertices, normals, faces and text_coord


;------------- HANDLE OBJ --------------
(defn- handle-v
  "Add to the model the node in vertices."
  [model x y z]
  (let [nb (count (get model :vertices))]
    (add-to-model model :vertices (keyword (str "v" nb)) [(Float/parseFloat x)
                                                          (Float/parseFloat y)
                                                          (Float/parseFloat z)])))

(defn- handle-vn
  "Add to the model the normal in normals."
  [model x y z]
  (let [nb (count (get model :normals))]
    (add-to-model model :normals (keyword (str "vn" nb)) [(Float/parseFloat x)
                                                          (Float/parseFloat y)
                                                          (Float/parseFloat z)])))

(defn- handle-vt
  "Add to the model the texture coordinates in text_coord."
  [model u v]
  (let [nb (count (get model :text_coord))]
    (add-to-model model :text_coord (keyword (str "vt" nb)) [(Float/parseFloat u)
                                                             (Float/parseFloat v)
                                                             (Float/parseFloat "0")])))

(defn- create-face
  "Create a list of (vertice normal text_cood). "
  [args]
  (into [] (map (fn [arg] (map #(if (empty? %)
                                  nil
                                  (Integer/parseInt %)) (s/split arg #"/"))) args)))

(defn- handle-f
  "Add to the model the face definitions in faces."
  [model & args]
  (let [nb (count (get model :faces))]
    (add-to-model model :faces (keyword (str "f" nb)) (create-face args))))

(defn- handle-usemtl
  "Add to the model the Material use in texture-name."
  [model mtl]
  (add-to-model model :texture-name mtl))


(declare split-line)
(defn- handle-mtllib
  "Acces to mtllib in order to get rbg"
  [model mtllib]
  (let [lines (with-open [r (io/reader mtllib)]
                (vec (line-seq r)))]
    (let [color (split-line (s/trim (first (filter #(re-matches #"Kd.*" %) lines))))]
      (map (fn [n] (add-to-model model :rbg n)) (into [] (rest color))))))      
;;(add-to-model model :rbg (into [] (rest color))))))

;---------------- UPDATE MODEL -------------------

(def ^:private handlers
  {:v handle-v
   :vn handle-vn
   :vt handle-vt
   :f handle-f
   :usemtl handle-usemtl
   :mtllib handle-mtllib})


(defn- update-model
  "Update the model using the handlers"
  ([model] model)
  ([model [kw & data]]
   (let [which-handler (kw handlers)]
     (apply which-handler (conj data model)))))

;---------------------------------------
;------- NORMALIZATION OBJ -------------
;---------------------------------------
(defn- delete-comment
  "Delete an OBJ comment in the string."
  [string]
  (s/replace string #"#.*" ""))

(defn- split-line
  "Change a line into a vector using whitespace as a delimiter."
  [string]
  (let [v (s/split string #"\s+")]
    (assoc v 0 (keyword (first v))))) ;The 1st element (keyword) define
                                          ;the vector (use in handlers)

(defn- isValid?
  "Check if the line is what we want."
  [[kw & data]]
  (contains? handlers kw))


;---------------------------------------
;---------------- PRINT ----------------
;---------------------------------------

(defn- show
  "Print the model"
  [item]
  (print "texture-name : ")
  (println (get item :texture-name))
  (print "rbg : [")
  (map print (get item :rbg))
  (print "] \nweight : ")
  (println (str (get item :weight)))
  (print "vertice : ")
  (p/pprint (get item :vertices))
  (print "normals : ")
  (p/pprint (get item :normals))
  (print "faces : ")
  (p/pprint (get item :faces))
  (print "text_coord : ")
  (p/pprint (get item :text_coord)))

;---------------------------------------
;------------- LOADER OBJ --------------
;---------------------------------------

; (s/trim str) -> Removes whitespace from both ends of string. So "  a  " become "a".
; (comp fns) -> Takes a set of functions and returns a fn that is the composition of those fns.
;

(defn load-model
  "Load an OBJ file to a model."
  [file]
  (let [item (with-open [r (io/reader file)]
               (transduce (comp (map delete-comment)
                              (map s/trim)
                              (remove empty?)
                              (map split-line)
                              (filter isValid?))
                        update-model
                        model
                        (line-seq r)))]
     (show item)))


;---------------------------------------
;---------------- Main -----------------
;---------------------------------------
(defn -main
  [& args]
  (if-not (empty? args)
    (doseq [arg *command-line-args*]
      (load-model arg))
    (throw (Exception. "Must have at least one argument!"))))
