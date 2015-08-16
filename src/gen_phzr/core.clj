(ns gen-phzr.core
  (:require [camel-snake-kebab.core :as csk]
            [cljfmt.core :as cfmt]
            [clojure.java.io :as io]
            [cuerdas.core :as str]
            [gen-phzr.codegen.core :as cg]
            [gen-phzr.util :as u])
  (:gen-class))

(defn -main
  [& args]
  (let [files (cg/gen-files "phaser.json")]
    (doseq [f files]
      (println "Writing to" (:path f))
      (io/make-parents (:path f))
      (spit (:path f) (:text f)))))
