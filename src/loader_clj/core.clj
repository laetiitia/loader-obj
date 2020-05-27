(ns loader-clj.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :as p])
  (:gen-class))

;---------------------------------------
;-------------- STRUCTURE --------------
;---------------------------------------
(def ^:private model
  {:texture-name []
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
  "Add a value in the model"
  ([model key val]
   (assoc model key (conj (get model key) val)))
  ([model k1 k2 val]
   (assoc model k1 (assoc (get model k1) k2 val)))) ;;For the maps of vertices, normals, faces and text_coord

(assoc {} :key 1)
;------------- HANDLE OBJ --------------
(defn- handle-v
  "Add to the model the node in vertices"
  [model x y z]
  (let [nb (count (get model :vertices))]
    (add-to-model model :vertices nb [(Float/parseFloat x)
                                      (Float/parseFloat y)
                                      (Float/parseFloat z)])))

(defn- handle-vn
  "Add to the model the normal in normals"
  [model x y z]
  (let [nb (count (get model :normals))]
    (add-to-model model :normals nb [(Float/parseFloat x)
                                     (Float/parseFloat y)
                                     (Float/parseFloat z)])))

(defn- handle-vt
  "Add to the model the texture coordinates in text_coord"
  [model u v]
  (let [nb (count (get model :text_coord))]
    (add-to-model model :text_coord nb [(Float/parseFloat u)
                                        (Float/parseFloat v)])))

(defn- create-face
  "Create a list of (vertice text_cood normal)"
  [args]
  (into [] (map (fn [arg] (into [] (map #(Integer/parseInt %) (into [] (s/split arg #"/"))))) args)))

(defn- handle-f
  "Add to the model the face definitions in faces"
  [model & args]
  (let [nb (count (get model :faces))]
    (add-to-model model :faces nb (create-face args))))

(defn- handle-usemtl
  "Add to the model the Material use in texture-name"
  [model mtl]
  (add-to-model model :texture-name mtl))

(declare split-line)
(defn- handle-mtllib
  "Acces to mtllib in order to get rbg"
  [model mtllib]
  (let [lines (with-open [r (io/reader mtllib)] (vec (line-seq r)))
        colors (filter #(re-matches #"Kd.*" %) lines)]
    (loop [l colors, mod model]
      (if (seq l)
        (recur (rest l) (add-to-model mod :rbg (into [] (map (fn [x] (Float/parseFloat x)) (rest (split-line (first l)))))))
        mod))))

;---------------- UPDATE MODEL -------------------

; The list of informations in obj file
; that we need to handle
(def ^:private handlers
  {:v handle-v
   :vn handle-vn
   :vt handle-vt
   :f handle-f
   :usemtl handle-usemtl
   :mtllib handle-mtllib})

; Gather informations in our model
(defn- update-model
  "Update the model using the handlers"
  ([model] model)
  ([model [kw & data]]
   (let [which-handler (kw handlers)]
     (apply which-handler (conj data model)))))

;---------------------------------------
;------- NORMALIZATION OBJ -------------
;---------------------------------------

; In order to get informations of the obj file
; without the comments 
(defn- delete-comment
  "Delete an OBJ comment in the string"
  [string]
  (s/replace string #"#.*" ""))

(defn- split-line
  "Change a line into a vector using whitespace as a delimiter"
  [string]
  (let [v (s/split string #"\s+")]
    (assoc v 0 (keyword (first v))))) ;The 1st element (keyword) define the vector (use in handlers)

; Check if there is a information that can be 
; handle by our functions 
(defn- isValid?
  "Check if the line is what we want"
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
  (print "rbg : ")
  (println (get item :rbg))
  (print "vertice : ")
  (println (get item :vertices))
  (print "normals : ")
  (println (get item :normals))
  (print "faces : ")
  (p/pprint (get item :faces))
  (print "text_coord : ")
  (p/pprint (get item :text_coord)))

;---------------------------------------
;------------- TRANSFORM ---------------
;---------------------------------------


; Each face have a number of vertices that have the same normal 
; (so we use repeat) 
;  * normals is the normals of the model that will be change
;  * n is the sequence 
(defn- transNormal
  "Transform :normals of the model into :normals of a mesh"
  [normals n]
  (loop [ks (keys normals), res {}, nb n]
    (if (and (seq ks) (seq nb))
      (recur (rest ks) (assoc res (first ks) (into [] (reduce concat (repeat (first nb) (get normals (first ks)))))) (rest nb))
      res)))

(defn- addVt
  "Add coordinates texttures to the structure (maps)"
  [tc res vt keyw acc]
  (if (seq vt)
    (if (contains? acc (first vt))
      (addVt tc res (rest vt) keyw acc)
      (addVt tc (assoc res keyw (into [] (concat (get res keyw) (get tc (first vt)))))
             (rest vt) keyw (conj acc (first vt))))
    [res acc]))


(defn- getVn
  "Get normals of a face"
  [f]
  (into [] (map #(first (rest (rest %))) f)))

; Transform Faces and Textures
;  * mapFaces : information about faces
;  * textures : information about textures
; Return couple of the new faces and textures
(defn- transFnT
  "Transform :faces and :text_coord of the model into a mesh"
  ([mapFaces textures] (transFnT textures (vals mapFaces) {} {} {}))
  ([tc list res coord acc]
   (if (seq list)
     (let [face (first list)
           v (map #(dec %) (map #(first %) face)) ;Get nodes of a face
           f (first (getVn face)) ;Get number of the face
           vt (map #(first (rest %)) face)
           [coord2 acc2] (addVt tc coord v f (get acc f #{}))]
       (transFnT tc (rest list) (assoc res f (into [] (concat v (get res f)))) ;Add indices of a face
                 coord2 (assoc acc f acc2)))
     [res coord])))

; Change a vertice according to it's face update
;  * f : vector of vertice for one face
;  * vs : list of all vertices 
;  * n : number of vertices without duplicate
; Return couple of the new vertices and faces
(defn- modifVertices
  [f vs n]
  (loop [face f, resV [], resF [], acc {}, cpt n]
    (if (seq face)
      (let [ind (first face)]
        (if (get acc (first face))
          (recur (rest face) (vec (concat resV (get vs ind))) (conj resF (get acc ind)) acc cpt)
          (recur (rest face) (vec (concat resV (get vs ind))) (conj resF cpt) (assoc acc ind cpt) (inc cpt))))
      [resV, resF])))

; Change a vertice according to it's face update
;  * f : vector of vertice of all faces
;  * vs : list of all vertices 
; Return couple of the new vertices and faces
(defn- transVertices
  "Transforme Vertices and Face according to the update"
  ([f vs]
  (loop [size (map #(count (set %)) (vals (into (sorted-map) f))), k (keys (into (sorted-map) f)), resV [], resF {}, n 0]
    (if (and (seq k) (seq size))
      (let [[v face] (modifVertices (get f (first k)) vs n)]
        (recur (rest size) (rest k) (conj resV v) (assoc resF (first k) face) (+ n (first size))))
      [resV, resF]))))






(if (get {1 "a"} 1)
  :ok
  :nil)


(defn transformModel
  "Transform model of loader into a correct mesh"
  [model]
  (let [[f t] (transFnT (get model :faces) (get model :text_coord))
        n (transNormal (get model :normals) (map #(count (set %)) (vals (into (sorted-map) f))))
        [v f2] (transVertices f (get model :vertices))]
    (-> model
        (assoc :faces f2)
        (assoc :vertices v)
        (assoc :normals n)
        (assoc :text_coord t))))



;---------------------------------------
;------------- LOADER OBJ --------------
;---------------------------------------

; (s/trim str) -> Removes whitespace from both ends of string. So "  a  " become "a".
; (comp fns) -> Takes a set of functions and returns a fn that is the composition of those fns.
;

(defn load-model
  "Load an OBJ file to a model"
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
    (show  (transformModel item))))

;---------------------------------------
;---------------- Main -----------------
;---------------------------------------
(defn -main
  [& args]
  (if-not (empty? args)
    (doseq [arg *command-line-args*]
      (load-model arg))
    (throw (Exception. "Must give at least the file path."))))

