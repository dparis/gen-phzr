(ns gen-phaser.codegen.constants
  (:require [cuerdas.core :as str]
            [gen-phaser.util :as u]))


(defn ^:private build-keyword-name-map
  [cs]
  (into {} (for [c cs
                 :let [cn (:name c)]]
             [(keyword (u/name->kebab cn)) cn])))

(def ^:private constants-map-template
  "(def ^:private %s-constants\n  %s)")

(def ^:private constants-fn-template
  "(def get-constant
  (memoize
   (fn [k]
     (if-let [cn (clojure.core/get %s-constants k)]
       (phaser->clj (aget js/%s cn))
       (js/console.log \"Tried to access invalid constant:\" k)))))")

(defn gen-constants
  [class-name cs]
  (when-not (empty? cs)
    (let [instance-arg (u/instance-arg-name class-name)
          kw-name-map  (build-keyword-name-map cs)]
      (str/join "\n\n"
                [(format constants-map-template
                         instance-arg
                         (-> (str kw-name-map)
                             (str/replace #"\, " "\n   ")))
                 (format constants-fn-template
                         instance-arg   ; get %s-constants k
                         class-name)]))))
