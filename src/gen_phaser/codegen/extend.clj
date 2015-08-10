(ns gen-phaser.codegen.extend
  (:require [cljfmt.core :as cfmt]))

(defn gen-extend
  [class-name]
  (cfmt/reformat-string
   (format "(pmp/extend-phaser %s)"
           class-name)))
