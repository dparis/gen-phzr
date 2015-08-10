(ns gen-phaser.util
  (:require [camel-snake-kebab.core :as csk]
            [cuerdas.core :as str]))

(defn ^:private kebab-touchup
  [s]
  (-> s
      (str/replace #"web\-gl" "webgl")
      (str/replace #"p\-2js" "p2-js")
      (str/replace #"p\-2" "p2")
      (str/replace #"palette\-c\-64" "palette-c64")
      (str/replace #"box\-2\-d" "box-2d")))

(defn name->kebab
  [s]
  (-> s
      (csk/->kebab-case-string)
      (str/lower)
      (kebab-touchup)))

(defn ^:private snake-touchup
  [s]
  (-> s
      (str/replace #"web\_gl" "webgl")
      (str/replace #"p\_2" "p2")))

(defn name->snake
  [s]
  (-> s
      (csk/->snake_case_string)
      (str/lower)
      (snake-touchup)))

(defn instance-arg-name
  [class-name]
  (name->kebab (last (str/split class-name #"\."))))
