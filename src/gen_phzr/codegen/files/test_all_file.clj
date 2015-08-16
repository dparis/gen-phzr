(ns gen-phzr.codegen.files.test-all-file
  (:require [cljfmt.core :as cfmt]
            [cuerdas.core :as str]
            [gen-phzr.util :as u]))

(defn build-test-file
  [export-data]
  (let [class-names (keys export-data)
        ns-paths    (sort (map u/build-ns-path class-names))]
    {:path "out/impl/test_all.cljs"
     :text (cfmt/reformat-string
            (format "(ns phzr.impl.test-all\n  (:require %s))"
                    (str/join "\n" (map #(format "[%s]" %) ns-paths))))}))
