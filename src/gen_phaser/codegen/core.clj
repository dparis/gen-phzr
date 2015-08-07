(ns gen-phaser.codegen.core
  (:require [cheshire.core :as c]
            [cljfmt.core :as cfmt]
            [clojure.java.io :as io]
            [cuerdas.core :as str]
            [gen-phaser.codegen.constants :as cc]
            [gen-phaser.codegen.function :as cf]
            [gen-phaser.codegen.ns :as cns]))


(defn ^:private build-data
  [json]
  (let [json-data (c/parse-string json true)
        class-col (:classes json-data)]
    (into {} (for [klass class-col] [(:name klass) klass]))))

(defn ^:private public-access?
  [f]
  (not (#{"protected" "private"} (:access f))))

(defn ^:private build-form-data
  [klass]
  (let [class-name  (:name klass)
        ns-form     (cns/gen-ns class-name "phzr")
        constructor (cf/gen-constructor class-name (:constructor klass))
        constants   (cc/gen-constants class-name
                                      (->> (:members klass)
                                           (filter public-access?)
                                           (filter #(= "constant" (:kind %)))))
        functions   (map #(cf/gen-function class-name %)
                         (filter public-access? (:functions klass)))]
    {:ns          ns-form
     :constructor constructor
     :constants   constants
     :functions   functions}))

(def ^:private export-whitelist
  #{})

(defn ^:private export-class-name?
  [s]
  (or (re-find #"Phaser\." s)
      (re-find #"PIXI\." s)
      (export-whitelist s)))

(defn ^:private class-keys
  [data]
  (->> (keys data)
       (filter export-class-name?)))

(defn gen-forms
  [json-resource-name]
  (let [data        (build-data (slurp (io/resource json-resource-name)))
        export-data (select-keys data (class-keys data))]
    (into {} (for [[class-name klass] export-data]
               [class-name (build-form-data klass)]))))

(defn build-file-text
  [class-name form-data]
  (let [ns-form     (:ns form-data)
        constructor (:constructor form-data)
        constants   (:constants form-data)
        functions   (:functions form-data)]
    (str ns-form
         "\n\n\n"
         constructor
         "\n\n\n"
         (if constants
           (str constants
                "\n\n\n")
           "")
         (str/join "\n\n" functions))))


(defn ^:private print-class-summary!
  [klass]
  (println "Name: " (:name klass))
  (println "Functions: ")
  (doseq [f (sort-by :name (:functions klass))]
    (println "  * " (:name f) (if (#{"protected" "private"} (:access f))
                                " + "
                                "")))
  (println "Members: ")
  (doseq [f (sort-by :name (:members klass))]
    (println "  * " (:name f) (if (#{"protected" "private"} (:access f))
                                " + "
                                ""))))
