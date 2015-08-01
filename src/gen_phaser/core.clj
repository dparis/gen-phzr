(ns gen-phaser.core
  (:require [cheshire.core :as c]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [gen-phaser.externs :as ext]
            [gen-phaser.preprocess :as pp])
  (:gen-class))


(defn ^:private load-resource
  [resource-name]
  (slurp (io/resource resource-name)))

;; NOTE - The jsdoc-parse binary must be on the PATH for
;;        the current JVM process
(defn ^:private jsdoc-parse-exec
  [js]
  (let [result (shell/sh "jsdoc-parse" :in js)]
    (:out result)))

(defn -main
  [& args]
  (let [js         (load-resource "phaser.js")
        clean-js   (pp/preprocess-js js)
        json       (jsdoc-parse-exec clean-js)
        clean-json (pp/preprocess-json json)
        data       (c/parse-string clean-json)
        exts-str   (ext/gen-exts data)]
    (spit "resources/phaser.clean.js" clean-js)
    (spit "resources/phaser.json" json)
    (spit "resources/phaser.clean.json" clean-json)
    (spit "phaser.ext.js" exts-str)))
