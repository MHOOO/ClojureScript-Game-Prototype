(defproject one "1.0.0-SNAPSHOT"
  :description "Getting Started with ClojureScript."
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [ring "1.0.1"]
                 [compojure "0.6.4"]
                 [enlive "1.0.0"]
                 [clj-stacktrace "0.2.4"]]
  :dev-dependencies [[jline "0.9.94"]
                     [marginalia "0.7.0-SNAPSHOT"]
                     [lein-marginalia "0.7.0-SNAPSHOT"]]
  :exclusions [clj-stacktrace] ;; ensure we exclude this from other
                               ;; dependencies, so we can pull a newer
                               ;; version that works with swank
  :disable-deps-clean true
  :main one.sample.prod-server
  :source-path "src/app/clj")
