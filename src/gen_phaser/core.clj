(ns gen-phaser.core
  (:require [cheshire.core :as c]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))


(defn ^:private load-resource
  [resource-name]
  (slurp (io/resource resource-name)))

(defn -main
  [& args]
  )
