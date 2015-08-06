(defproject gen-phaser "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [cheshire "5.5.0"]
                 [funcool/cuerdas "0.5.0"]
                 [camel-snake-kebab "0.3.2"]
                 [cljfmt "0.3.0"]]
  :main ^:skip-aot gen-phaser.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
