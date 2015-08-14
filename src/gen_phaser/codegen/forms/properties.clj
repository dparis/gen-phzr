(ns gen-phaser.codegen.forms.properties
  (:require [cljfmt.core :as cfmt]
            [cuerdas.core :as str]
            [gen-phaser.util :as u]))


(defn ^:private read-only?
  [p]
  (boolean (:readonly p)))

(defn ^:private build-keyword-name-map
  [cs]
  (into {} (for [c cs
                 :let [cn (:name c)]]
             [(keyword (u/name->kebab cn)) cn])))

(def ^:private get-properties-map-template
  "(def %s-get-properties\n  %s)")

(def ^:private set-properties-map-template
  "(def %s-set-properties\n  %s)")

(defn gen-properties
  [class-name ps]
  (when-not (empty? ps)
    (let [rw-ps           (remove read-only? ps)
          instance-arg    (u/instance-arg-name class-name)
          all-kw-name-map (build-keyword-name-map ps)
          rw-kw-name-map  (build-keyword-name-map rw-ps)]
      (cfmt/reformat-string
       (str/join "\n\n"
                 [(format get-properties-map-template
                          instance-arg
                          (-> (str all-kw-name-map)
                              (str/replace #"\, " "\n   ")))
                  (format set-properties-map-template
                          instance-arg
                          (-> (str rw-kw-name-map)
                              (str/replace #"\, " "\n   ")))])))))
