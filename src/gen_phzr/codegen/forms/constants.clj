(ns gen-phzr.codegen.forms.constants
  (:require [cljfmt.core :as cfmt]
            [cuerdas.core :as str]
            [gen-phzr.util :as u]))


(defn ^:private build-keyword-name-map
  [cs]
  (into (sorted-map)
        (for [c cs
              :let [cn (:name c)]]
          [(keyword (u/name->kebab cn)) cn])))

(def ^:private constants-map-template
  "(def %s-constants\n  %s)")

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

(def ^:private const-fn-template
  "(defn const
  [k]
  (when-let [cn (get %s k)]
    (aget js/%s cn))\n)")

(defn gen-const-fn
  [class-name]
  (let [ns-path       (u/build-ns-path class-name "phzr.impl.accessors.")
        instance-arg  (u/instance-arg-name class-name)
        const-map-str (str ns-path "/" instance-arg "-constants")]
    (cfmt/reformat-string
     (format const-fn-template
             const-map-str
             class-name))))
