(ns gen-phaser.codegen.extend
  (:require [cljfmt.core :as cfmt]
            [gen-phaser.util :as u]))

(defn ^:private set-properties?
  [ps]
  (not (empty? (remove #(boolean (:readonly %)) ps))))

(defn gen-extend
  [class-name constants properties]
  (let [])
  (cfmt/reformat-string
   (format "(ex/extend-phaser-class js/%s\n%s\n%s\n%s)"
           class-name
           (if properties
             (str (u/instance-arg-name class-name) "-get-properties")
             "nil")
           (if (set-properties? properties)
             (str (u/instance-arg-name class-name) "-set-properties")
             "nil")
           (if constants
             (str (u/instance-arg-name class-name) "-constants")
             "nil"))))
