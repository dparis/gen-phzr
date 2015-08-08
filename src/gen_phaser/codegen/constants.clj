(ns gen-phaser.codegen.constants
  (:require [camel-snake-kebab.core :as csk]
            [cuerdas.core :as str]))


(defn ^:private instance-arg-name
  [class-name]
  (csk/->kebab-case-string (last (str/split class-name #"\."))))

(def ^:private name-overrides
  {})

(defn ^:private name->keyword
  [s]
  (if-let [kw (get name-overrides s)]
    kw
    (csk/->kebab-case-keyword (str/lower s))))

(defn ^:private build-keyword-name-map
  [cs]
  (into {} (for [c cs
                 :let [name (:name c)]]
             [(name->keyword name) name])))

(def ^:private constants-map-template
  "(def ^:private %s-constants\n  %s)")

(def ^:private constants-fn-template
  "(def get-%s-constant
  (memoize
   (fn [k]
     (if-let [cn (clojure.core/get %s-constants k)]
       (aget js/%s cn)
       (js/console.log \"Tried to access invalid constant:\" k)))))")

(defn gen-constants
  [class-name cs]
  (when-not (empty? cs)
    (let [instance-arg (instance-arg-name class-name)
          kw-name-map  (build-keyword-name-map cs)]
      (str/join "\n\n"
                [(format constants-map-template
                         instance-arg
                         (-> (str kw-name-map)
                             (str/replace #"\, " "\n   ")))
                 (format constants-fn-template
                         instance-arg   ; get-%s-constant
                         instance-arg   ; get %s-constants k
                         class-name)]))))
