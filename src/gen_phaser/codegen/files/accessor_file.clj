(ns gen-phaser.codegen.files.accessor-file
  (:require [cljfmt.core :as cfmt]
            [gen-phaser.codegen.forms.constants :as fc]
            [gen-phaser.codegen.forms.properties :as fp]
            [gen-phaser.util :as u]))

(defn ^:private build-ns
  [class-name]
  (let [ns-path (u/build-ns-path class-name "phzr.impl.accessors.")]
    (format "(ns %s)" ns-path)))

(defn build-accessor-file
  [class-name class-data]
  (let [file-path      (u/build-file-path "out/impl/accessors" class-name)
        ns-form        (build-ns class-name)
        constants      (:constants class-data)
        constants-str  (fc/gen-constants class-name constants)
        properties     (:properties class-data)
        properties-str (fp/gen-properties class-name properties)]
    (when-not (and (empty? constants) (empty? properties))
      {:path file-path
       :text (cfmt/reformat-string
              (str ns-form
                   "\n\n\n"
                   constants-str
                   "\n\n\n"
                   properties-str))})))
