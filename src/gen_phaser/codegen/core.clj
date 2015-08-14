(ns gen-phaser.codegen.core
  (:require [cheshire.core :as c]
            [cljfmt.core :as cfmt]
            [clojure.java.io :as io]
            [cuerdas.core :as str]
            [gen-phaser.codegen.files.accessor-file :as faf]
            [gen-phaser.codegen.files.src-file :as fsf]
            [gen-phaser.codegen.files.extend-all-file :as feaf]
            [gen-phaser.codegen.files.test-all-file :as ftaf]
            [gen-phaser.util :as u]))


(defn ^:private build-data
  [json]
  (let [json-data (c/parse-string json true)
        class-col (:classes json-data)]
    (into {} (for [klass class-col] [(:name klass) klass]))))

(defn ^:private fix-bad-doc-in-image-pre-update
  [data]
  (let [image-klass         (get data "Phaser.Image")
        img-functions       (:functions image-klass)
        fixed-img-functions (u/distinct-by img-functions
                                           #(= "preUpdate" (:name %)))]
    (if-not (= (count img-functions) (count fixed-img-functions))
      (assoc data
             "Phaser.Image"
             (assoc image-klass :functions fixed-img-functions))
      data)))

(defn ^:private remove-duplicate-just-pressed-from-gamepad
  [data]
  (let [gamepad-klass      (get data "Phaser.Gamepad")
        gp-functions       (:functions gamepad-klass)
        fixed-gp-functions (u/distinct-by gp-functions
                                          #(= "justPressed" (:name %)))]
    (if-not (= (count gp-functions) (count fixed-gp-functions))
      (assoc data
             "Phaser.Gamepad"
             (assoc gamepad-klass :functions fixed-gp-functions))
      data)))

(defn ^:private fix-bad-doc-in-tween-repeat-all
  [data]
  (let [tween-klass        (get data "Phaser.Tween")
        tween-functions    (:functions tween-klass)
        fixed-tw-functions (map #(if (and (= "repeat" (:name %))
                                          (= 1 (count (:parameters %))))
                                   (assoc % :name "repeatAll")
                                   %)
                                tween-functions)]
    (assoc data
           "Phaser.Tween"
           (assoc tween-klass :functions fixed-tw-functions))))

(defn ^:private fix-bad-doc-in-debug-geom
  [data]
  (let [debug-klass         (get data "Phaser.Utils.Debug")
        debug-functions     (:functions debug-klass)
        fixed-dbg-functions (map #(if (and (= "geom" (:name %))
                                           (= 3 (count (:parameters %))))
                                    (assoc % :name "rectangle")
                                    %)
                                 debug-functions)]
    (assoc data
           "Phaser.Utils.Debug"
           (assoc debug-klass :functions fixed-dbg-functions))))

(defn ^:private fix-bad-create-body-in-p2
  [data]
  (let [p2-klass     (get data "Phaser.Physics.P2")
        p2-functions (:functions p2-klass)
        cb-fn        (first (filter #(= "createBody" (:name %)) p2-functions))
        fixed-params (->> (:parameters cb-fn)
                          (remove #(re-find #"options\." (:name %)))
                          (map #(if (#{"options" "points"} (:name %))
                                  (assoc % :optional true)
                                  %)))
        fixed-cb-fn  (assoc cb-fn :parameters fixed-params)
        fixed-p2-fns (map #(if (= "createBody" (:name %))
                             fixed-cb-fn
                             %)
                          p2-functions)]
    (assoc data
           "Phaser.Physics.P2"
           (assoc p2-klass
                  :functions fixed-p2-fns))))

(defn ^:private fix-bad-create-particle-in-p2
  [data]
  (let [p2-klass     (get data "Phaser.Physics.P2")
        p2-functions (:functions p2-klass)
        cp-fn        (first (filter #(= "createParticle" (:name %))
                                    p2-functions))
        fixed-params (->> (:parameters cp-fn)
                          (remove #(re-find #"options\." (:name %)))
                          (map #(if (#{"options" "points"} (:name %))
                                  (assoc % :optional true)
                                  %)))
        fixed-cp-fn  (assoc cp-fn :parameters fixed-params)
        fixed-p2-fns (map #(if (= "createParticle" (:name %))
                             fixed-cp-fn
                             %)
                          p2-functions)]
    (assoc data
           "Phaser.Physics.P2"
           (assoc p2-klass
                  :functions fixed-p2-fns))))

(defn ^:private fix-bad-add-polygon-in-p2-body
  [data]
  (let [p2b-klass     (get data "Phaser.Physics.P2.Body")
        p2b-functions (:functions p2b-klass)
        ap-fn         (first (filter #(= "addPolygon" (:name %))
                                     p2b-functions))
        fixed-params  (->> (:parameters ap-fn)
                           (remove #(re-find #"options\." (:name %)))
                           (map #(if (#{"options" "points"} (:name %))
                                   (assoc % :optional true)
                                   %)))
        fixed-ap-fn   (assoc ap-fn :parameters fixed-params)
        fixed-p2b-fns (map #(if (= "addPolygon" (:name %))
                              fixed-ap-fn
                              %)
                           p2b-functions)]
    (assoc data
           "Phaser.Physics.P2.Body"
           (assoc p2b-klass
                  :functions fixed-p2b-fns))))

(defn ^:private fix-pixi-class-name
  [data]
  (let [pixi-klass (get data "PIXI.PIXI")]
    (-> data
        (dissoc "PIXI.PIXI")
        (assoc "PIXI" (assoc pixi-klass :name "PIXI")))))

(defn ^:private make-class-static
  [data class-name]
  (let [klass     (get data class-name)
        functions (:functions klass)
        fixed-fns (map #(assoc % :static true) functions)]
    (assoc data
           class-name
           (assoc klass :functions fixed-fns))))

(defn ^:private massage-data
  [data]
  (-> data
      (fix-bad-doc-in-image-pre-update)
      (remove-duplicate-just-pressed-from-gamepad)
      (fix-bad-doc-in-tween-repeat-all)
      (fix-bad-doc-in-debug-geom)
      (fix-bad-create-body-in-p2)
      (fix-bad-create-particle-in-p2)
      (fix-bad-add-polygon-in-p2-body)
      (fix-pixi-class-name)
      (make-class-static "Phaser.Math")
      (make-class-static "Phaser.Utils")))

(defn ^:private public-access?
  [f]
  (not (#{"protected" "private"} (:access f))))

(defn ^:private build-export-data
  [data]
  (into {} (for [[class-name klass] data]
             (let [class-name  (:name klass)
                   functions   (filter public-access? (:functions klass))
                   constructor (:constructor klass)
                   constants   (->> (:members klass)
                                    (filter public-access?)
                                    (filter #(= "constant" (:kind %))))
                   properties  (->> (:members klass)
                                    (filter public-access?)
                                    (filter #(= "member" (:kind %))))]
               [class-name
                {:name        class-name
                 :constructor constructor
                 :constants   constants
                 :properties  properties
                 :functions   functions}]))))

(defn gen-files
  [json-resource-name]
  (let [data          (build-data (slurp (io/resource json-resource-name)))
        massaged-data (massage-data data)
        class-names   (u/export-class-names massaged-data)
        export-data   (build-export-data
                       (select-keys massaged-data class-names))]
    (remove nil?
            (conj
             (concat (pmap (fn [[class-name class-data]]
                             (fsf/build-src-file class-name class-data))
                           export-data)
                     (pmap (fn [[class-name class-data]]
                             (faf/build-accessor-file class-name class-data))
                           export-data))
             (ftaf/build-test-file export-data)
             (feaf/build-extend-all-file export-data)))))
