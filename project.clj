(defproject gen-phzr "1.0.0"
  :description "Clojure app to generate phzr code from Phaser javascript source"
  :url "http://"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [cheshire "5.5.0"]
                 [funcool/cuerdas "0.5.0"]
                 [camel-snake-kebab "0.3.2"]
                 [cljfmt "0.3.0"]]
  :main ^:skip-aot gen-phzr.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
