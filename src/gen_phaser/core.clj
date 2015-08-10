(ns gen-phaser.core
  (:require [camel-snake-kebab.core :as csk]
            [clojure.java.io :as io]
            [cuerdas.core :as str]
            [gen-phaser.codegen.core :as cg]
            [gen-phaser.util :as u])
  (:gen-class))


(defn ^:private path-touchup
  [s]
  (-> s
      (str/replace #"web\_gl" "webgl")
      (str/replace #"p\_2" "p2")))

(defn ^:private build-file-path
  [output-dir class-name]
  (let [parts      (->> (str/split class-name #"\.")
                        (remove #(= "Phaser" %))
                        (map u/name->snake))
        ns-fs-path (str/join "/" parts)]
    (str output-dir "/" ns-fs-path ".cljs")))

(defn -main
  [& args]
  (let [forms (cg/gen-forms "phaser.json")]
    (doseq [[class-name form-data] forms
            :let [path (build-file-path "out" class-name)]]
      (println "Outputting " class-name " to " path " ...")
      (io/make-parents path)
      (spit path (cg/build-file-text class-name form-data)))))
