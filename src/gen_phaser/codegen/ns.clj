(ns gen-phaser.codegen.ns
  (:require [camel-snake-kebab.core :as csk]
            [cljfmt.core :as cfmt]
            [cuerdas.core :as str]))

(def ^:private core-fn-names
  (set (map str (keys (ns-publics 'clojure.core)))))

(defn ^:private collides-with-core?
  [s]
  (core-fn-names s))

(defn ^:private ns-touchup
  [s]
  (-> s
      (str/replace #"web\-gl" "webgl")
      (str/replace #"p\-2" "p2")))

(defn ^:private build-ns-path
  [class-name library-name]
  (let [parts (->> (str/split class-name #"\.")
                   (map csk/->kebab-case-string)
                   (map str/lower)
                   (map ns-touchup))]
    (if (= "phaser" (first parts))
      (str library-name "." (str/join "." (rest parts)))
      (str library-name "." (str/join "." parts)))))

(def ^:private ns-template
  "(ns %s\n (:require [%s.core :refer [clj->phaser phaser->clj]])%s)")

(def ^:private excludes-template
  "\n (:refer-clojure :exclude [%s])")

(defn ^:private build-excludes
  [exclude-names]
  (if-not (empty? exclude-names)
    (format excludes-template (str/join " " exclude-names))
    ""))

(defn gen-ns
  [class-name library-name fs]
  (let [ns-path        (build-ns-path class-name library-name)
        fn-name-kebabs (map #(csk/->kebab-case-string (:name %)) fs)
        exclude-names  (filter collides-with-core? fn-name-kebabs)
        excludes-str   (build-excludes exclude-names)]
    (cfmt/reformat-string
     (format ns-template
             ns-path
             library-name
             excludes-str))))
