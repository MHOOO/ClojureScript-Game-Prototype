(ns one.sample.snippets
  "Macros for including HTML snippets in the ClojureScript application
  at compile time."
  (:use [one.templates :only (render)])
  (:require [net.cgrand.enlive-html :as html]))

(defn- snippet [file id]
  (render (html/select (html/html-resource file) id)))

(defmacro snippets
  "Expands to a map of HTML snippets which are extracted from the
  design templates."
  []
  {:form (snippet "form.html" [:div#form])
   :greeting (snippet "greeting.html" [:div#greeting])
   :game (snippet "game.html" [:div#game])})


(defmacro wrap
  [target-sym arglist & body]
  `(let [wrapped# ~target-sym]
     (set!
      ~target-sym
      ;; unbound wrapper function
      ;; will not have an instance as its first argument
      (fn [& args#]
        (let [~arglist (concat
                        ;; given arguments to wrapper
                        args#
                        ;; wrapped function
                        [(fn [inst# & args2#]
                           (.apply
                            wrapped#
                            inst# (~'clj->js args2#)))])]
          ~@body))
      ))
  )
