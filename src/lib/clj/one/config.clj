(ns ^{:doc "Convenience functions for working with configuration data."}
  one.config)

(defn cljs-build-opts
  "Given a configuration map, return output directory options."
  [config]
  {:output-to (str (:js config) "/" (:dev-js-file-name config))
   :output-dir (str (:js config) "/out")
   :externs ["public/js/caat-externs.js"]})

(defn production-js
  "Given a configuration map, return the path to the production
  Javascript file."
  [config]
  (str (:js config) "/" (:prod-js-file-name config)))
