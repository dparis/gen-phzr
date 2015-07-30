(ns gen-phaser.core
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [gen-phaser.externs :as ext])
  (:gen-class))

(defn ^:private load-phaser-json
  [resource-name]
  (json/parse-string (slurp (io/resource resource-name))))

(defn -main
  [& args]
  (let [resource-name "phaser.json"
        data          (load-phaser-json resource-name)
        exts-str      (ext/gen-exts data)]
    (spit "phaser.ext.js" exts-str)))
