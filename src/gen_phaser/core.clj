(ns gen-phaser.core
  (:require [camel-snake-kebab.core :as csk]
            [cljfmt.core :as cfmt]
            [clojure.java.io :as io]
            [cuerdas.core :as str]
            [gen-phaser.codegen.core :as cg]
            [gen-phaser.codegen.ns :as cns]
            [gen-phaser.util :as u])
  (:gen-class))


(defn ^:private path-touchup
  [s]
  (-> s
      (str/replace #"web\_gl" "webgl")
      (str/replace #"p\_2" "p2")))

(defn ^:private build-file-path
  [output-dir class-name]
  (let [parts      (->> (str/split class-name #"\.")
                        (remove #(= "Phaser" %))
                        (map u/name->snake))
        ns-fs-path (str/join "/" parts)]
    (str output-dir "/" ns-fs-path ".cljs")))

(defn ^:private write-generated-files!
  [forms]
  (doseq [[class-name form-data] forms
          :let [path (build-file-path "out" class-name)]]
    (println "Outputting " class-name " to " path " ...")
    (io/make-parents path)
    (spit path (cg/build-file-text class-name form-data))))

(defn ^:private build-test-file
  [forms]
  (let [class-names (u/export-class-names forms)
        ns-paths    (sort (map cns/build-ns-path class-names))]
    (cfmt/reformat-string
     (format "(ns phzr.impl.test-all\n  (:require %s))"
             (str/join "\n" (map #(format "[%s]" %) ns-paths))))))

(defn ^:private write-test-file!
  [forms]
  (let [test-file-str (build-test-file forms)
        path          "out/impl/test_all.cljs"]
    (io/make-parents path)
    (spit path test-file-str)))

(defn -main
  [& args]
  (let [forms (cg/gen-forms "phaser.json")]
    (write-generated-files! forms)
    (write-test-file! forms)))
