(ns gen-phaser.codegen.properties
  (:require [camel-snake-kebab.core :as csk]
            [cuerdas.core :as str]))


(defn ^:private read-only?
  [p]
  (boolean (:readonly p)))

(defn ^:private instance-arg-name
  [class-name]
  (csk/->kebab-case-string (last (str/split class-name #"\."))))

(def ^:private name-overrides
  {})

(defn ^:private name->keyword
  [s]
  (if-let [kw (get name-overrides s)]
    kw
    (csk/->kebab-case-keyword s)))

(defn ^:private build-keyword-name-map
  [cs]
  (into {} (for [c cs
                 :let [name (:name c)]]
             [(name->keyword name) name])))

(def ^:private get-properties-map-template
  "(def ^:private %s-get-properties\n  %s)")

(def ^:private set-properties-map-template
  "(def ^:private %s-set-properties\n  %s)")

(def ^:private get-properties-fn-template
  "(defn get-property
  [%s-obj k]
    (if-let [pn (clojure.core/get %s-get-properties k)]
      (phaser->clj (aget %s-obj pn))
      (js/console.log \"Tried to access invalid property:\" k)))")

(def ^:private set-properties-fn-template
  "(defn set-property!
  [%s-obj k v]
    (if-let [pn (clojure.core/get %s-set-properties k)]
      (aset %s-obj pn (clj->phaser v))
      (js/console.log \"Tried to access invalid property:\" k)))")


(defn gen-properties
  [class-name ps]
  (when-not (empty? ps)
    (let [rw-ps           (remove read-only? ps)
          instance-arg    (instance-arg-name class-name)
          all-kw-name-map (build-keyword-name-map ps)
          rw-kw-name-map  (build-keyword-name-map rw-ps)]
      (str/join "\n\n"
                [(format get-properties-map-template
                         instance-arg
                         (-> (str all-kw-name-map)
                             (str/replace #"\, " "\n   ")))
                 (format set-properties-map-template
                         instance-arg
                         (-> (str rw-kw-name-map)
                             (str/replace #"\, " "\n   ")))
                 (format get-properties-fn-template
                         instance-arg   ; get-%s-property
                         instance-arg   ; first arg
                         instance-arg   ; get %s-get-properties k
                         instance-arg)  ; aget %s name
                 (format set-properties-fn-template
                         instance-arg   ; get-%s-property
                         instance-arg   ; first arg
                         instance-arg   ; get %s-set-properties k
                         instance-arg)])))) ; aset %s name
