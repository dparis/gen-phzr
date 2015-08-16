(ns gen-phzr.codegen.forms.extend
  (:require [cljfmt.core :as cfmt]
            [gen-phzr.util :as u]))

(defn ^:private set-properties?
  [ps]
  (not (empty? (remove #(boolean (:readonly %)) ps))))

(defn gen-extend
  [class-name constants properties]
  (let [ns-path    (u/build-ns-path class-name "phzr.impl.accessors.")
        inst-name  (u/instance-arg-name class-name)]
    (cfmt/reformat-string
     (format "(ex/extend-phaser-class\njs/%s\n%s\n%s\n%s)"
             class-name
             (if properties
               (str ns-path "/" inst-name "-get-properties")
               "nil")
             (if (set-properties? properties)
               (str ns-path "/" inst-name "-set-properties")
               "nil")
             (if constants
               (str ns-path "/" inst-name "-constants")
               "nil")))))
