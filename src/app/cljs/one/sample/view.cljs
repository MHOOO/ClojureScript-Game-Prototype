(ns ^{:doc "Render the views for the application."}
  one.sample.view
  (:use [domina :only (set-html! set-styles! styles by-id set-style!
                       by-class value set-value! set-text! nodes single-node)]
        [domina.xpath :only (xpath)]
        [one.browser.animation :only (play)])
  (:require-macros [one.sample.snippets :as snippets])
  (:require [goog.events.KeyCodes :as key-codes]
            [goog.events.KeyHandler :as key-handler]
            [clojure.browser.event :as event]
            [one.dispatch :as dispatch]
            [one.sample.animation :as fx]))

(def ^{:doc "A map which contains chunks of HTML which may be used
  when rendering views."}
  snippets (snippets/snippets))

(defmulti render
  "Accepts a map which represents the current state of the application
  and renders a view based on the value of the `:state` key."
  :state)

(defmethod render :init [_]
  (fx/initialize-views (:game snippets))
  ;; (fx/initialize-views (:game snippets))
  ;; (add-input-event-listeners "name-input")
  ;; (event/listen (by-id "greet-button")
  ;;               "click"
  ;;               #(dispatch/fire :greeting
  ;;                               {:name (value (by-id "name-input"))}))
  )

(dispatch/react-to #{:state-change} (fn [_ m] (render m)))
