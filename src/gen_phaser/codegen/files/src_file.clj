(ns gen-phaser.codegen.files.src-file
  (:require [cljfmt.core :as cfmt]
            [cuerdas.core :as str]
            [gen-phaser.codegen.forms.function :as ff]
            [gen-phaser.util :as u]))

(def ^:private core-fn-names
  (into (set (map str (keys (ns-publics 'clojure.core))))
        #{"divide" "clone" "uuid"}))

(defn ^:private collides-with-core?
  [s]
  (boolean (core-fn-names s)))

(def ^:private excludes-template
  "\n (:refer-clojure :exclude [%s])")

(defn ^:private build-excludes
  [exclude-names]
  (if-not (empty? exclude-names)
    (format excludes-template (str/join " " exclude-names))
    ""))

(def ^:private ns-template
  "(ns %s
  (:require [phzr.impl.utils.core :refer [clj->phaser phaser->clj]]
            [phzr.impl.extend :as ex]
            [cljsjs.phaser])%s)")

(defn ^:private build-ns
  [class-name fs]
  (let [ns-path        (u/build-ns-path class-name)
        fn-name-kebabs (map #(u/name->kebab (:name %)) fs)
        exclude-names  (distinct (filter collides-with-core? fn-name-kebabs))
        excludes-str   (build-excludes exclude-names)]
    (cfmt/reformat-string
     (format ns-template
             ns-path
             excludes-str))))

(defn build-src-file
  [class-name class-data]
  (let [file-path       (u/build-file-path "out" class-name)
        constructor     (:constructor class-data)
        constructor-str (ff/gen-constructor class-name constructor)
        functions       (:functions class-data)
        ns-str          (build-ns class-name functions)
        function-strs   (map #(ff/gen-function class-name %) functions)]

    {:path file-path
     :text (cfmt/reformat-string
            (str ns-str
                 "\n\n\n"
                 (if constructor-str
                   (str constructor-str "\n\n\n")
                   "")
                 (str/join "\n\n" function-strs)))}))
