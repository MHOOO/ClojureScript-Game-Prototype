(ns ^{:doc "Defines animations which are used in the sample
  application."}
  one.sample.animation
  (:use [one.core :only (start)]
        [one.browser.animation :only (bind parallel serial play play-animation)]
        [domina :only (by-id set-html! set-styles! destroy-children! append!)]
        [domina.xpath :only (xpath)])
  (:require [goog.dom.forms :as gforms]
            [goog.style :as style]
            [one.logging :as log]))

(def form "//div[@id='form']")
(def cloud "//div[@id='greeting']")
(def label "//label[@id='name-input-label']/span")

(def ^:private
  form-in {:effect :fade :start 0 :end 1 :time 800})

(def logger (log/get-logger "animation"))

(defn jsArr
  "Recursively converts a sequential object into a JavaScript array"
  [seq]
  (.array (vec (map #(if (sequential? %) (jsArr %) %)
                    seq))))

(defn jsObj
  "Convert a clojure map into a JavaScript object"
  [obj]
  (.strobj (into {} (map (fn [[k v]]
                           (let [k (if (keyword? k) (name k) k)
                                 v (if (keyword? v) (name v) v)]
                             (if (map? v)
                               [k (jsObj v)]
                               [k v])))
                         obj))))

(defn clj->js
  "Recursively transforms ClojureScript maps into Javascript objects,
   other ClojureScript colls into JavaScript arrays, and ClojureScript
   keywords into JavaScript strings."
  [x]
  (cond
   (string? x) x
   (keyword? x) (name x)
   (map? x) (.strobj (reduce (fn [m [k v]]
                               (assoc m (clj->js k) (clj->js v))) {} x))
   (coll? x) (apply array (map clj->js x))
   :else x))

(defn gen-shaky-behavior [target]
  (let [container (CAAT/ContainerBehavior.)
        total-time 1500.0
        rotation-strenght (/ (* 2 Math/PI) 140)]
    (.addBehavior
     container
     (doto (CAAT/RotateBehavior.)
       (.setFrameTime 0 (/ total-time 4))
       (.setValues 0 (- rotation-strenght))))
    (.addBehavior
     container
     (doto (CAAT/RotateBehavior.)
       (.setFrameTime (/ total-time 4) (* 2 (/ total-time 4)))
       (.setValues (- rotation-strenght) 0)))
    (.addBehavior
     container
     (doto (CAAT/RotateBehavior.)
       (.setFrameTime (* 2 (/ total-time 4)) (* 3 (/ total-time 4)))
       (.setValues 0 (+ rotation-strenght))))
    (.addBehavior
     container
     (doto (CAAT/RotateBehavior.)
       (.setFrameTime (* 3 (/ total-time 4)) (* 4 (/ total-time 4)))
       (.setValues (+ rotation-strenght) 0)))
    (.setCycle container true)
    (.setFrameTime container (.time target) total-time)
    container))

(defn gen-move-behavior
  "Generate & return a move behavior for the target. Still must be added by calling .addBehavior.
   Examples: 
   (gen-move-behavior hat :to {:up 30})
   (gen-move-behavior hat :from {:up 30} :to {:down 30})
   (gen-move-behavior hat :from {:down 30})

   (gen-move-behavior hat :from [0 0] :to [50 50]"
  [target & {from :from 
             to :to
             time :time :or {time 120}}]
  (let [nr (fn [n] (or n 0))
        [to-x to-y] (if (vector? to) to)
        [from-x from-y] (if (vector? from) from)]
    ;; (log/info logger
    ;;           (str (+ (nr (:right from)) (- (nr (:left from)))) " " (+ (nr (:down from)) (- (nr (:up from)))) " -> "
    ;;                (+ (nr (:right to)) (- (nr (:left to)))) " " (+ (nr (:down to)) (- (nr (:up to))))))
    (doto (CAAT/PathBehavior.)
      (.setFrameTime (.time target) time)
      (.setPath
       (doto (CAAT/Path.)
         (.setLinear (or from-x (+ (nr (:right from)) (- (nr (:left from)))))
                     (or from-y (+ (nr (:down from)) (- (nr (:up from)))))
                     (or to-x (+ (nr (:right to)) (- (nr (:left to)))))
                     (or to-y (+ (nr (:down to)) (- (nr (:up to)))))))))))


(defn gen-basket
  "Generate & return the interactive basket."
  [director]
  (let [scale-factor 1.02
        basket-sprite (.initialize (CAAT/SpriteImage.) (.getImage director "basket") 1 2)
        basket-head (.initialize (CAAT/SpriteImage.) (.getImage director "basket-head") 1 1)
        basket-container (doto (CAAT/ActorContainer.) 
                           (.setBackgroundImage basket-sprite)
                           (.setLocation 490 320)
                           (.setFillStyle "#ff3fff"))
        basket-body (doto (CAAT/Actor.)
                      (.setBackgroundImage basket-sprite) 
                      (.setSpriteIndex 1))
        basket-head (doto (CAAT/Actor.)
                      (.setBackgroundImage basket-head)
                      (.setLocation 0 0)
                      (.enableEvents false))]
    (.addChild basket-container basket-body)
    (.addChild basket-container basket-head)
    (set! basket-container.isOpened false)
    (set! basket-container.doOpen
          (fn []
            (when (not this.isOpened)
              (log/info logger "Opening basket")
              (doto basket-head
                (.addBehavior (gen-shaky-behavior basket-head))
                (.addBehavior (gen-move-behavior basket-head :to {:up 30})))
              (.addBehavior basket-body
                            (doto (CAAT/ScaleBehavior.)
                              (.setFrameTime (.time basket-body) 120)
                              (.setValues 1 scale-factor 1 scale-factor)))
              (set! this.isOpened true))))

    (set! basket-container.doClose
          (fn []
            (when this.isOpened
              (log/info logger "Closing basket")
              (doto basket-head
                (. (emptyBehaviorList))
                (.setRotation 0)
                (.addBehavior (gen-move-behavior basket-head :from {:up 30})))
              (.addBehavior basket-body
                            (doto (CAAT/ScaleBehavior.)
                              (.setFrameTime (.time basket-body) 120)
                              (.setValues scale-factor 1 scale-factor 1)))
              (set! this.isOpened false))))
    ;; make sure the sprite changes when the mouse hovers over the basket
    (set! basket-body.mouseEnter (fn [mouseEvent] (. basket-container (doOpen))))
    (set! basket-body.mouseExit (fn [mouseEvent] (. basket-container (doClose))))
    basket-container))

(defn throw-into-basket [scene basket target] 
  (let [total-animation-time 400
        source (.modelToView target (CAAT/Point. 0 0))
        destination (.modelToView basket (CAAT/Point. 0 0))
        remove-timer (.createTimer scene
                                   (.time target)
                                   (* 1.2 total-animation-time)
                                   (fn [scene-time timer-time timetask]
                                     (.setExpired target scene-time))
                                   (fn []) (fn []))]
    (log/info logger (str "Src: " source ";Dest: " destination))
    (doto target
      (.enableEvents false)
      (.addBehavior (gen-move-behavior target :from [(.x source) (.y source)] :to [(.x destination) (.y destination)]))
      (.addBehavior (doto (CAAT/ScaleBehavior.)
                      (.setFrameTime (.time target) total-animation-time)
                      (.setValues 1 0.1 1 0.1)))
      (.addBehavior (doto (CAAT/RotateBehavior.)
                      (.setFrameTime (.time target) total-animation-time)
                      (.setValues 0 (* 2 Math/PI))) 0 0)
      )))

(defn draw-bubble [ctx x y w h radius]
  (let [r (+ x w)
        b (+ y h)]
    (set! ctx.strokeStyle "black")
    (set! ctx.lineWidth "2")
    (. ctx (beginPath))
    (.moveTo ctx (+ x radius) y)
    (.lineTo ctx (+ x (/ radius 2)), (- y 10))
    (.lineTo ctx (+ x (* 2 radius)), y)
    (.lineTo ctx (- r radius), y)
    (.quadraticCurveTo ctx r, y, r, (+ y radius))
    (.lineTo ctx r, (+ y h (- radius)))
    (.quadraticCurveTo ctx r, b, (- r radius), b)
    (.lineTo ctx (+ x radius), b)
    (.quadraticCurveTo ctx x, b, x, (- b radius))
    (.lineTo ctx x, (+ y radius))
    (.quadraticCurveTo ctx x, y, (+ x radius), y)
    (. ctx (stroke))
    (set! ctx.fillStyle "#AABBAA")
    (. ctx (fill))
    ))

(defn gen-bubble [director scene
                  text & {:keys [x y w h radius text-size time] :or {text-size 20 x 0 y 0 w nil h nil radius 10
                                                                     time nil}}]
  (log/info logger (str "Creating bubble with text: " text))
  (let [actor-container (doto (CAAT/ActorContainer.)
                          ;; (.setFillStyle "#FFFFFF")
                          ) 
        text (doto (CAAT/TextActor.)
               (.setFont (str text-size "px sans-serif")) 
               (.setText text)
               (. (create))
               (.calcTextSize director)
               (.setFillStyle "black")) 
        w (or w (+ text.textWidth 30))
        h (or h (+ text.textHeight 30))
        bubble (doto (CAAT/Actor.)
                 (.setLocation 0 0)
                 (.enableEvents false))]
    (.setBounds actor-container x y w h)
    (when time
      (.createTimer scene
                    (.time scene)
                    time
                    (fn [scene-time timer-time timetask] 
                      (.setExpired actor-container scene-time))
                    (fn []) (fn [])))
    (set! bubble.paint
          (fn [director time]
            (draw-bubble director.ctx 0 0 w h radius))) 
    (doto actor-container
      (.addChild bubble)
      (.addChild text))
    (.centerAt text (/ w 2) (/ h 2))

    actor-container))

(def *positive-feedbacks* ["Super!" "Genau!" "Ja, sehr richtig!" "Ja, weiter so!"])

(defn initialize-views
  "Accepts the form and greeting view HTML and adds them to the
  page. Animates the form sliding in from above. This function must be
  run before any other view functions. It may be called from any state
  to reset the UI."
  [game-html form-html]
  (let [content (xpath "//div[@id='content']")]
    (log/info logger "Initializing View")
    (log/info logger (pr-str image-data)) 
    (destroy-children! content)
    (set-html! content game-html)
    ;; a caat test
    ;; PRELOAD IMAGES & RUN MAIN FN
    (log/info logger "Loading images...")
    (CAAT.modules.initialization/init
     800
     500
     "game-holder"
     (clj->js [{:id "room" :url "images/room.png"}
               {:id "basket" :url "images/basket.png"}
               {:id "basket-head" :url "images/basket - head.png"}])
     (fn [director]
       (log/info logger " Images loaded. Creating scene.")
       (let [scene (. director (createScene))
             background (doto (CAAT/Actor.) 
                          (.setBackgroundImage (.getImage director "room"))) 
             basket (gen-basket director)
             ball (doto (CAAT/ShapeActor.)
                    (.setSize 50 50)
                    (.setFillStyle "#ffffff")
                    (.enableDrag true))] 
         (log/info logger " Displaying scene")

         ;; make sure to pass mouse events to the basket
         (set! ball.__mouseDrag ball.mouseDrag)
         (set! ball.mouseDrag
               (fn [event]
                 (let [point-in-basket (.modelToModel ball (.point event) basket)]
                   (if (.contains basket (.x point-in-basket) (.y point-in-basket))
                     (when (. basket (doOpen))
                       (throw-into-basket scene basket ball)
                       (.addChildDelayed scene (gen-bubble director scene (rand-nth *positive-feedbacks*) :x 20 :y 20 :time 2500)))
                     (. basket (doClose))))
                 (.__mouseDrag ball event)))
         
         (.addChild scene background)
         (.addChild scene basket)
         (.addChild scene ball)
         ;; (.addChild scene (gen-bubble director scene (rand-nth *positive-feedbacks*) :x 50 :y 20))
          
         ;; (.setScene director 0)
         
         ;; (.loop director 1)
         )))
    
    
    ;; (set-styles! (xpath cloud) {:opacity "0" :display "none" :margin-top "-500px"})
    ;; (set-styles! (by-id "greet-button") {:opacity "0.2" :disabled true})
    ;; (play form form-in {:after #(.focus (by-id "name-input") ())})
    ))

(comment ;; Try it

  (initialize-views (:form one.sample.view/snippets)
                    (:greeting one.sample.view/snippets))
  
  )

(defn label-move-up
  "Move the passed input field label above the input field. Run when
  the field gets focus and is empty."
  [label]
  (play label [{:effect :color :end "#53607b" :time 200}
               {:effect :slide :up 40 :time 200}]))

(defn label-fade-out
  "Make the passed input field label invisible. Run when the input
  field loses focus and contains a valid input value."
  [label]
  (play label {:effect :fade :start 1 :end 0 :time 200}))

(def move-down [{:effect :fade :end 1 :time 200}
                {:effect :color :end "#BBC4D7" :time 200}
                {:effect :slide :down 40 :time 200}])

(def fade-in {:effect :fade :end 1 :time 400})

(def fade-out {:effect :fade :start 1 :end 0 :time 400})

(defn label-move-down
  "Make the passed input field label visible and move it down into the
  input field. Run when an input field loses focus and is empty."
  [label]
  (play label move-down))

(comment ;; Examples of label effects.
  
  (label-move-up label)
  (label-fade-out label)
  (label-move-down label)
  )

(defn show-greeting
  "Move the form out of view and the greeting into view. Run when the
  submit button is clicked and the form has valid input."
  []
  (let [e {:effect :fade :start 1 :end 0 :time 500}]
    (play-animation (parallel (bind form e)
                              (bind label e) ; Since the label won't fade in IE
                              (bind cloud
                                    {:effect :color :time 500} ; Dummy animation for delay purposes
                                    {:effect :fade-in-and-show :time 600}))
                    {:before #(gforms/setDisabled (by-id "name-input") true)
                     ;; We need this next one because IE8 won't hide the button
                     :after #(set-styles! (by-id "greet-button") {:display "none"})})))

(defn show-form
  "Move the greeting cloud out of view and show the form. Run when the
  back button is clicked from the greeting view."
  []
  (play-animation (serial (parallel (bind cloud {:effect :fade-out-and-hide :time 500})
                                    (bind form
                                          {:effect :color :time 300} ; Dummy animation for delay purposes
                                          form-in)
                                    (bind label fade-in move-down)))
                  {;; Because IE8 won't hide the button, we need to
                   ;; toggle it between displaying inline and none
                   :before #(set-styles! (by-id "greet-button") {:display "inline"})
                   :after #(do
                             (gforms/setDisabled (by-id "name-input") false)
                             (.focus (by-id "name-input") ()))}))

(comment ;; Switch between greeting and form views

  (label-move-up label)
  (show-greeting)
  (show-form)
  )

(defn disable-button
  "Accepts an element id for a button and disables it. Fades the
  button to 0.2 opacity."
  [id]
  (let [button (by-id id)]
    (gforms/setDisabled button true)
    (play button {:effect :fade :end 0.2 :time 400})))

(defn enable-button
  "Accepts an element id for a button and enables it. Fades the button
  to an opactiy of 1."
  [id]
  (let [button (by-id id)]
    (gforms/setDisabled button false)
    (play button fade-in)))

(comment ;; Examples of all effects

  (initialize-views (:form one.sample.view/snippets)
                    (:greeting one.sample.view/snippets))
  (label-move-up label)
  (label-fade-out label)
  (show-greeting)
  (show-form)

  (disable-button "greet-button")
  (enable-button "greet-button")
  )
