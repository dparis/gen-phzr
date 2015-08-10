(ns gen-phaser.codegen.ns
  (:require [camel-snake-kebab.core :as csk]
            [cljfmt.core :as cfmt]
            [cuerdas.core :as str]
            [gen-phaser.util :as u]))


(def ^:private core-fn-names
  (set (map str (keys (ns-publics 'clojure.core)))))

(defn ^:private collides-with-core?
  [s]
  (core-fn-names s))

(defn ^:private build-ns-path
  [class-name]
  (let [parts (->> (str/split class-name #"\.")
                   (map u/name->kebab))]
    (if (= "phaser" (first parts))
      (str "phzr." (str/join "." (rest parts)))
      (str "phzr." (str/join "." parts)))))

(def ^:private ns-template
  "(ns %s
  (:require [phzr.impl.utils.core :refer [clj->phaser phaser->clj]]
            [phzr.impl.utils.js]
            [cljsjs.phaser])
  (:require-macros [phzr.impl.macros.protocols :as pmp])%s)")

(def ^:private excludes-template
  "\n (:refer-clojure :exclude [%s])")

(defn ^:private build-excludes
  [exclude-names]
  (if-not (empty? exclude-names)
    (format excludes-template (str/join " " exclude-names))
    ""))

(defn gen-ns
  [class-name fs]
  (let [ns-path        (build-ns-path class-name)
        fn-name-kebabs (map #(u/name->kebab (:name %)) fs)
        exclude-names  (filter collides-with-core? fn-name-kebabs)
        excludes-str   (build-excludes exclude-names)]
    (cfmt/reformat-string
     (format ns-template
             ns-path
             excludes-str))))
