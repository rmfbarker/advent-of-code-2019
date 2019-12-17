(defproject advent-of-code-2019 ""
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.async "0.6.532"]]

  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :test-paths ["src" "test"]
  :plugins [[lein-auto "0.1.3"]])
