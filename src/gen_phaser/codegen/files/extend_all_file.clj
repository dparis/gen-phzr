(ns gen-phaser.codegen.files.extend-all-file
  (:require [cljfmt.core :as cfmt]
            [cuerdas.core :as str]
            [gen-phaser.codegen.forms.constants :as fc]
            [gen-phaser.codegen.forms.extend :as fe]
            [gen-phaser.codegen.forms.properties :as fp]
            [gen-phaser.util :as u]))


(def ^:private extend-blacklist
  (conj u/raw-phaser-objs))

(defn ^:private extend-class-names
  [export-data]
  (let [class-names (keys export-data)]
    (remove extend-blacklist class-names)))

(defn ^:private require-accessors
  [class-name]
  (let [ns-path (u/build-ns-path class-name "phzr.impl.accessors.")]
    (format "[%s]" ns-path)))

(defn ^:private build-require
  [extend-data]
  (let [ra-strs (for [[class-name class-data] extend-data
                      :when (or (seq (:constants class-data))
                                (seq (:properties class-data)))]
                  (require-accessors class-name))]
    (format "(:require [cljsjs.phaser]\n[phzr.impl.extend.core :as ex]\n%s)"
            (str/join "\n" (sort ra-strs)))))

(defn ^:private build-extend-call
  [class-name class-data]
  (let [constants     (:constants class-data)
        constants-str (fc/gen-constants class-name constants)
        properties    (:properties class-data)
        property-strs (fp/gen-properties class-name properties)]
    (fe/gen-extend class-name constants-str property-strs)))

(defn build-extend-all-file
  [export-data]
  (let [class-names      (extend-class-names export-data)
        extend-data      (select-keys export-data class-names)
        require-str      (build-require extend-data)
        extend-call-strs (map (fn [[class-name class-data]]
                                (build-extend-call class-name class-data))
                              extend-data)]
    {:path "out/impl/extend.cljs"
     :text (cfmt/reformat-string
            (format "(ns phzr.impl.extend\n%s)\n\n\n%s"
                    require-str
                    (str/join "\n\n" (sort extend-call-strs))))}))
