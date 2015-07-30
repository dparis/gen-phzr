(ns gen-phaser.core
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn ^:private load-phaser-json
  [resource-name]
  (json/parse-string (slurp (io/resource resource-name))))

(defn ^:private public?
  [node]
  (not (#{"protected" "private"} (get node "access"))))

(defn ^:private public-access
  [data]
  (filter public? data))

(defn ^:private exposed-constructors
  [data]
  (let [class-set (into #{} (map #(get % "memberof")
                                 (public-access data)))]
    (flatten
     (for [c class-set]
       (filter #(and (= c (get % "longname"))
                     (= "constructor" (get % "kind"))
                     (public? %)
                     (not (#{"Phaser.Device"} (get % "longname"))))
               data)))))

(defn ^:private instance-property?
  [node]
  (and (= "member" (get node "kind"))
       (= "instance" (get node "scope"))
       (public? node)))

(defn ^:private inst-props-for-constructor
  [data constructor]
  (let [c-name (get constructor "longname")]
    (->> (filterv #(= c-name (get % "memberof")) data)
         (filterv instance-property?))))

(defn ^:private instance-method?
  [node]
  (and (= "function" (get node "kind"))
       (= "instance" (get node "scope"))
       (public? node)))

(defn ^:private inst-methods-for-constructor
  [data constructor]
  (let [c-name (get constructor "longname")]
    (->> (filterv #(= c-name (get % "memberof")) data)
         (filterv instance-method?))))

(defn ^:private build-exts
  [data]
  (let [constructors (exposed-constructors data)]
    (into {} (for [c constructors
                   :let [ip (inst-props-for-constructor data c)
                         im (inst-methods-for-constructor data c)]]
               [c {:instance-properties ip
                   :instance-methods    im}]))))

(defn ^:private format-constructor
  [constructor]
  (let [cl-name (get constructor "longname")
        params  (get constructor "params")]
    (format "var %s = function(%s) {}"
            cl-name
            (str/join ", " (map #(get % "name") params)))))

(defn ^:private format-inst-property
  [constructor inst-property]
  (let [cl-name (get constructor "longname")
        ip-name (get inst-property "name")]
    (format "%s.prototype.%s = {}" cl-name ip-name)))

(defn ^:private format-inst-method
  [constructor inst-method]
  (let [cl-name (get constructor "longname")
        im-name (get inst-method "name")
        params  (get inst-method "params")]
    (format "%s.prototype.%s = function(%s) {}"
            cl-name
            im-name
            (str/join ", " (map #(get % "name") params)))))

(defn ^:private format-ext
  [[c ext]]
  (let [c-str   (format-constructor c)
        ip-strs (map #(format-inst-property c %) (:instance-properties ext))
        im-strs (map #(format-inst-method c %) (:instance-methods ext))]
    (format "%s\n\n%s\n\n%s"
            c-str
            (str/join "\n" (sort ip-strs))
            (str/join "\n" (sort im-strs)))))

(defn ^:private format-exts
  [exts]
  (->> (map format-ext exts)
       (str/join "\n\n")))

(defn ^:private gen-exts
  [data]
  (let [exts (build-exts data)]
    (format-exts exts)))

(defn -main
  [& args]
  (let [resource-name "phaser.json"
        data          (load-phaser-json resource-name)
        exts-str      (gen-exts data)]
    (spit "phaser.ext.js" exts-str)))
