(ns gen-phaser.codegen.constants
  (:require [cljfmt.core :as cfmt]
            [cuerdas.core :as str]
            [gen-phaser.util :as u]))


(defn ^:private build-keyword-name-map
  [cs]
  (into {} (for [c cs
                 :let [cn (:name c)]]
             [(keyword (u/name->kebab cn)) cn])))

(def ^:private constants-map-template
  "(def ^:private %s-constants\n  %s)")

(defn gen-constants
  [class-name cs]
  (when-not (empty? cs)
    (let [instance-arg (u/instance-arg-name class-name)
          kw-name-map  (build-keyword-name-map cs)]
      (cfmt/reformat-string
       (format constants-map-template
               instance-arg
               (-> (str kw-name-map)
                   (str/replace #"\, " "\n   ")))))))
