(ns gen-phaser.codegen.ns
  (:require [camel-snake-kebab.core :as csk]
            [cljfmt.core :as cfmt]
            [cuerdas.core :as str]))

(defn ^:private build-ns-path
  [class-name library-name]
  (let [parts (str/split class-name #"\.")]
    (if (= "Phaser" (first parts))
      (str library-name "." (csk/->kebab-case-string (str/join "." (rest parts))))
      (str library-name "." (csk/->kebab-case-string (str/join "." parts))))))

(def ns-template
  "(ns %s\n (:require [%s.core :refer [clj->phaser phaser->clj]]))")

(defn gen-ns
  [class-name library-name]
  (let [ns-path (build-ns-path class-name library-name)]
    (cfmt/reformat-string
     (format ns-template
             ns-path
             library-name))))
