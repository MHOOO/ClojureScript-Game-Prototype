(ns ^{:doc "Defines animations which are used in the sample
  application."}
  one.sample.animation
  (:use [one.core :only (start)]
        [one.browser.animation :only (bind parallel serial play play-animation)]
        [domina :only (by-id set-html! set-styles! destroy-children! append!)]
        [domina.xpath :only (xpath)])
  (:require-macros [one.sample.snippets :as m])
  (:require [goog.dom.forms :as gforms]
            [goog.style :as style]
            [one.logging :as log]))

(def ^:private
  form-in {:effect :fade :start 0 :end 1 :time 800})

(def logger (log/get-logger "animation"))

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

(defprotocol AXYProvider
  (x-of [t])
  (y-of [t]))

(extend-protocol AXYProvider
  cljs.core.Vector
  (x-of [t] (first t))
  (y-of [t] (second t))
  CAAT.Point
  (x-of [t] (.x t))
  (y-of [t] (.y t)))


(defn gen-shaky-behavior
  "Generate & return a rotation behavior that will rotate a target left & right repeatedly."
  [target]
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
   (gen-move-behavior :to {:up 30})
   (gen-move-behavior :from {:up 30} :to {:down 30})
   (gen-move-behavior :from {:down 30})

   (gen-move-behavior :from [0 0] :to [50 50] :time 120)
   (gen-move-behavior :from [50 50] :to [0 0] :time [120 120])"
  [& {:keys [from to time auto-rotate?]
      :or {time 120 auto-rotate? false}}]
  (let [nr (fn [n] (or n 0))
        [to-x to-y] (if (vector? to) to)
        [from-x from-y] (if (vector? from) from)
        [start-time time] (if (vector? time) time [0 time])]
    ;; (log/info logger
    ;;           (str (+ (nr (:right from)) (- (nr (:left from)))) " " (+ (nr (:down from)) (- (nr (:up from)))) " -> "
    ;;                (+ (nr (:right to)) (- (nr (:left to)))) " " (+ (nr (:down to)) (- (nr (:up to))))))
    ;; (log/info logger (str start-time " " time))
    (doto (CAAT/PathBehavior.)
      (.setFrameTime start-time time)
      (.setAutoRotate (if auto-rotate? true false) (if (fn? auto-rotate?) auto-rotate? nil))
      (.setPath
       (doto (CAAT/Path.)
         (.setLinear (or from-x (+ (nr (:right from)) (- (nr (:left from)))))
                     (or from-y (+ (nr (:down from)) (- (nr (:up from)))))
                     (or to-x (+ (nr (:right to)) (- (nr (:left to)))))
                     (or to-y (+ (nr (:down to)) (- (nr (:up to)))))))))))

(defn deg->rad [deg]
  (* (* 2 Math/PI) (/ deg 360)))

(defn gen-rotate-behavior [& {:keys [from to time rotation-anchor]
                              :or {from 0 to 0 time 120 rotation-anchor [0.5 0.5]}}]
  (let [[start-time time] (if (vector? time) time [0 time])]
    (doto (CAAT/RotateBehavior.)
      (.setFrameTime start-time time) 
      (.setValues (deg->rad from)
                  (deg->rad to)
                  (x-of rotation-anchor)
                  (y-of rotation-anchor)
                  ))))

(defn gen-parallel-behavior [& args]
  (let [container (doto (CAAT/ContainerBehavior.)
                    (.setFrameTime 0 Number.MAX_VALUE))] 
    (doseq [spec args]
      (let [bh (gen-behavior spec)] 
        (.setFrameTime bh 0 (. bh (getDuration))) 
        (.addBehavior container bh)))
    container))

(def kw->behavior-fn {:move (fn [arg] (apply gen-move-behavior arg))
                      :rotate (fn [arg] (apply gen-rotate-behavior arg))
                      :parallel gen-parallel-behavior})

(defprotocol AAnimationSpec
  (gen-behavior [s]))

(extend-protocol AAnimationSpec
  CAAT.Behavior
  (gen-behavior [s] s)
  cljs.core.Vector
  (gen-behavior [v]
                (let [[kind & args] v] 
                  (apply (kind kw->behavior-fn) args))))

(defn animation-specs->behavior
  "Given a TARGET and any number of animation SPECS, create & return a
ContainerBehavior which encompasses all the animations. The animations
will be executed syncronously. Any starting-time set on a spec will be
overriden, so that there are no waits between animations (unless
explicitly specified using a wait-spec)."
  ([target & specs]
     (let [container (doto (CAAT/ContainerBehavior.)
                       (.setFrameTime (.time target) Number.MAX_VALUE))]
       (loop [[spec & more] specs
              t 0
              last-caat-behavior nil]
         (when (not (nil? spec))
           (if (fn? spec)
             (do (when last-caat-behavior
                   ;; (log/info logger (str "Adding listener fn: " spec " to " last-caat-behavior))
                   (.addListener
                    last-caat-behavior
                    (clj->js
                     {:behaviorExpired
                      (let [callback-fn spec]
                        (fn [bh t a]
                          ;; (log/info logger (pr-str "Calling expiration fn" spec))
                          (callback-fn t)))})))
                 (recur more t last-caat-behavior))
             (let [bh (gen-behavior spec)
                   dur (. bh (getDuration))
                   last-caat-behavior (if (instance? CAAT.Behavior bh) bh last-caat-behavior)] 
               (.setFrameTime bh t dur) 
               (.addBehavior container bh)
               (recur more
                      (+ t dur)
                      last-caat-behavior)))))
       container)))

(defn animate
  "Animate a target. Besides the target, takes any number of behavior specs.
   For supported specs, take a look at the KW->BEHAVIOR-FN map.
  Example:
    (animate target [:move [:to {:up md :left 5} :time 200]])"
  [target & animations]
  (.addBehavior target (apply animation-specs->behavior target animations)))

(defn basket-head-munch! [basket-head & {:keys [on-finish] :or {on-finish (fn [])}}]
  (let [md 10]
    (animate basket-head 
             [:move [:to {:up md :left 5} :time 200]]
             [:move [:from {:up md :left 5} :time 300]]
             [:move [:to {:up md :left (- 5)} :time 150]]
             [:move [:from {:up md :left (- 5)} :time 300]]
             [:move [:to {:up (/ md 2) :left 5} :time 200]]
             [:move [:from {:up (/ md 2) :left 5} :time 100]]
             [:move [:to {:up (/ md 2) :left (- 5)} :time 150]]
             [:move [:from {:up (/ md 2) :left (- 5)} :time 100]]
             (fn [t] (on-finish)))))

(defn basket-head-talk! [basket-head]
  (let [md 10
        tpa 90]
   (apply
    animate basket-head
            (apply
             concat
             (repeat
              2
              [[:move [:to {:up md :left 5} :time tpa]]
               [:move [:from {:up md :left 5} :time tpa]]
               [:move [:to {:up md :left (- 5)} :time tpa]]
               [:move [:from {:up md :left (- 5)} :time tpa]]])))))

(defn basket-body-shake! [basket-body]
  (animate basket-body
           [:rotate [:to 5 :time 80]]
           [:rotate [:from 5 :time 80]]
           [:rotate [:to -5 :time 80]]
           [:rotate [:from -5 :time 80]]))

(defn gen-basket
  "Generate & return the interactive basket.
   The basket will have doOpen & doClose methods to open & close
  it. Also it will open & close when hovered."
  [director & {:keys [x y] :or {x 490 y 320}}]
  (let [scale-factor 1.02
        basket-sprite (.initialize (CAAT/SpriteImage.) (.getImage director "basket") 1 2)
        basket-head (.initialize (CAAT/SpriteImage.) (.getImage director "basket-head") 1 1)
        basket-container (doto (CAAT/ActorContainer.) 
                           (.setBackgroundImage basket-sprite)
                           (.setLocation x y)
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
    (set! basket-container.doMunch
          (fn [& {:keys [on-finish] :or {on-finish (fn [])}}]
            (basket-head-munch! basket-head :on-finish on-finish)))
    (set! basket-container.doTalk
          (fn []
            (basket-head-talk! basket-head)))
    (set! basket-container.doOpen
          (fn [& {:keys [on-finish] :or {on-finish (fn [])}}]
            (when (not this.isOpened)
              (log/info logger "Opening basket")
              (animate basket-head (gen-shaky-behavior basket-head))
              (animate basket-head
                       (gen-move-behavior :to {:up 30})
                       (fn [t] (on-finish)))
              (.addBehavior basket-body
                            (doto (CAAT/ScaleBehavior.)
                              (.setFrameTime (.time basket-body) 120)
                              (.setValues 1 scale-factor 1 scale-factor)))
              (set! this.isOpened true))))

    (set! basket-container.doClose
          (fn [& {:keys [on-finish] :or {on-finish (fn [])}}]
            (when this.isOpened
              ;; (log/info logger "Closing basket")
              (doto basket-head
                (. (emptyBehaviorList))
                (.setRotation 0)
                (animate [:move [:from {:up 30}]]
                         (fn [t] (on-finish))))
              
              (.addBehavior basket-body
                            (doto (CAAT/ScaleBehavior.)
                              (.setFrameTime (.time basket-body) 120)
                              (.setValues scale-factor 1 scale-factor 1)))
              (set! this.isOpened false))))
    ;; make sure the sprite changes when the mouse hovers over the basket
    (set! basket-body.mouseEnter (fn [mouseEvent]
                                   ;; (. basket-container (doMunch))
                                   (. basket-container (doOpen))
                                   ))
    (set! basket-body.mouseExit (fn [mouseEvent] (. basket-container (doClose))))
    basket-container)) 

(defn throw-into-basket
  "Animate a TARGET by moving it into the BASKET."
  [director scene basket target counter] 
  (let [total-animation-time 400
        source (.modelToView target (CAAT/Point. 0 0))
        destination (.modelToView basket (CAAT/Point. 0 0))
        remove-timer (.createTimer scene
                                   (.time target)
                                   (* 1.2 total-animation-time)
                                   (fn [scene-time timer-time timetask]
                                     (.setExpired target scene-time))
                                   (fn []) (fn []))]
    ;; (log/info logger (str "Src: " source ";Dest: " destination))
    (let [body (.getChildAt basket 0)
          head (.getChildAt basket 1)]
      (basket-body-shake! body)
      (.enableEvents basket false)
      (set! basket.eating true)
      (. basket
         (doOpen
          :on-finish
          (fn []
            (. basket
               (doClose
                :on-finish
                (fn []
                  (.audioPlay director "munching")
                  (. basket
                     (doMunch
                      :on-finish
                      (fn []
                        (. counter (inc))
                        (.audioPlay director "chime")
                        ;; TODO: have the basket do the talking after it has opened up
                        (basket-head-talk! head)
                        (.enableEvents basket true)
                        (set! basket.eating false)
                        (message! director scene
                                  (rand-nth *positive-feedbacks*)
                                  :x 510 :y 290
                                  :duration 1500)))))))))) 
      
      )
    (set! target.trashed true)
    (doto target
      (.enableDrag false)
      (.enableEvents false)
      (.addBehavior (gen-move-behavior :from [(.x source) (.y source)] :to [(.x destination) (.y destination)]))
      (.addBehavior (doto (CAAT/ScaleBehavior.)
                      (.setFrameTime (.time target) total-animation-time)
                      (.setValues 1 0.1 1 0.1)))
      (.addBehavior (doto (CAAT/RotateBehavior.)
                      (.setFrameTime (.time target) total-animation-time)
                      (.setValues 0 (* 2 Math/PI))) 0 0))))

(defn draw-bubble
  "Draw a speech bubble. With given width, height & corner radius."
  [ctx x y w h radius bg-color]
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
    (set! ctx.fillStyle bg-color)
    (. ctx (fill))
    ))

(defn gen-bubble
  "Generate a speech bubble with some text in it. Must be passed
  director, scene & text in that order. The text will be centered
  inside the bubble. Other args are (as keywords):
  :x - x position of bubble
  :y - y position of bubble
  :w - width of bubble. Will be computed based on :text-size if not given.
  :h - height of bubble. Will be computed based on :text-size if not given.
  :radius - radius of the corners. Higher means more round.
  :text-size - Text size in pixels.
  :time - Time in ms after which the bubble should expire."
  [director scene text &
   {:keys [x y w h radius text-size time bg-color]
    :or {text-size 20 x 0 y 0 w
         nil h nil radius 10 time nil
         bg-color "#FFFFFF"}}]
  (log/info logger (str "Creating bubble with text: " text))
  (let [actor-container (doto (CAAT/ActorContainer.)
                          ;; (.setFillStyle "#FFFFFF")
                          (.enableEvents false)
                          ) 
        text (doto (CAAT/TextActor.)
               (.setFont (str text-size "px Comic Sans MS")) 
               (.setText text)
               (. (create))
               (.calcTextSize director)
               (.setFillStyle "black")) 
        w (or w (+ text.textWidth 30))
        h (or h (+ text.textHeight 30))
        bubble (doto (CAAT/Actor.)
                 (.setLocation 0 0)
                 (.enableEvents false))
        x (if (fn? x) (x w h) x)
        y (if (fn? y) (y w h) y)]
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
            (draw-bubble director.ctx 0 0 w h radius bg-color))) 
    (doto actor-container
      (.addChild bubble)
      (.addChild text))
    (.centerAt text (/ w 2) (/ h 2))

    actor-container))

(def *positive-feedbacks* ["Super!" "Genau!" "Ja, sehr richtig!" "Ja, weiter so!"])

(defprotocol Container
  (add!* [c obj]))

(defn add! [c obj & {:keys [position] :or {position nil}}]
  (when position
    (.setLocation obj (x-of position) (y-of position)))
  (add!* c obj))

(extend-protocol Container
  CAAT.ActorContainer
  (add!* [c obj] (.addChildDelayed c obj)))

(let [current {:object nil}]
 (defn message!
   "Print out a message using a speech bubble. The message will stay
   for 2.5 seconds until it vanishes. A second call will remove any
   currently displayed message first."
   [director scene msg & {:keys [x y duration]
                          :or {duration 2500
                               x 20 y (fn [w h]
                                        (- (.height director) h 20))}}]
   (when current.object
     (log/info logger (str "Expiring old message bubble: " current.object))
     (.setExpired current.object (.time scene)))
   (let [new-message (gen-bubble director scene msg :x x :y y :time duration)]
     (add! scene new-message)
     (set! current.object new-message))))

(defn pulsate! [target & {:keys [strength count total-time] :or {strength 1.1 count 1 total-time 200}}]
  (let [time-offset (/ total-time count)
        [strength-start strength-end] (if (vector? strength) strength [1 strength])]
    (dotimes [i count]
      (.addBehavior
       target
       (doto (CAAT/ScaleBehavior.)
         (.setFrameTime (+ (.time target) (* i time-offset)) time-offset)
         (.setValues strength-start strength-end strength-start strength-end)
         (.setPingPong true))))))

(defn gen-count-actor [total-count]
  (let [data {}
        actor (doto (CAAT/TextActor.) 
                (.setFont "30px Comic Sans Ms")
                (.setFillStyle "orange")
                (.setOutline true)
                (.setOutlineColor "black")
                (.setLocation 640 90))]
    (set! actor.mouseEnter
          (fn [e] (pulsate! actor)))
    (set! actor.inc
          (fn []
            (set! data.count (+ (or data.count -1) 1)) 
            (.setText actor (str data.count "/" total-count))
            (pulsate! actor :count 2 :strength 1.3)))
    (. actor (inc))
    actor))

(defn make-draggable-into-basket
  "Given a target, make it draggable in such a way that it can be
  dragged ontop of the basket."
  [target director scene basket counter]
  (set! target.__mouseDrag target.mouseDrag)
  (set! target.mouseDrag
        (fn [event]
          (let [point-in-basket (.modelToModel target (.point event) basket)]
            (if (.contains basket (.x point-in-basket) (.y point-in-basket))
              (when (and (not (.isOpened basket)) (not (.eating basket)))
                (throw-into-basket director scene basket target counter))
              ;; (. basket (doClose))
              ))
          (.__mouseDrag target event)))
  (m/wrap target.mouseDown [e oldf]
          (.audioPlay director "flopp")
          (pulsate! target :strength [1.1 1.4]))
  (m/wrap target.mouseUp [e oldf]
          (.audioPlay director "doppp"))
  (let [oldf target.mouseEnter]
    (set! target.mouseEnter
          (fn [e] 
            (.addBehavior
             target
             (doto (CAAT/ScaleBehavior.)
               (.setFrameTime (.time scene) 250)
               (.setValues 1 1.1 1 1.1))) 
            (oldf target e)))) 
  (let [oldf target.mouseExit]
    (set! target.mouseExit
          (fn [e]
            (when (not target.trashed)
              (.addBehavior
               target
               (doto (CAAT/ScaleBehavior.)
                 (.setFrameTime (.time scene) 250)
                 (.setValues 1.1 1 1.1 1))))
            (oldf target e))))
  target)

(defn gen-teddy [director]
  (log/info logger (str "Creating teddy"))
  (let [teddy-image (.getImage director "teddy") 
        img (doto (CAAT/SpriteImage.)
              (.initialize  teddy-image 1 3)
              (.setSpriteIndex 0)
              ;; (.setAnimationImageIndex (clj->js [0 1 0 2]))
              (.setChangeFPS 150)
              ) 
        actor (doto (CAAT/Actor.)
                (.setBackgroundImage img)
                (.enableDrag true))]
    (let [oldf actor.mouseEnter]
      (set! actor.mouseEnter
            (fn [e] 
              (.setAnimationImageIndex img (clj->js [0 1 0 2]))
              (oldf actor e))))
    (let [oldf actor.mouseExit]
      (set! actor.mouseExit
            (fn [e]
              (.setAnimationImageIndex img (clj->js [0]))
              (oldf actor e))))
    actor))

(defn initialize-views
  "Accepts the form and greeting view HTML and adds them to the
  page. Animates the form sliding in from above. This function must be
  run before any other view functions. It may be called from any state
  to reset the UI."
  [game-html]
  (let [content (xpath "//div[@id='content']")]
    (log/info logger "Initializing View")
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
               {:id "basket-head" :url "images/basket - head.png"}
               {:id "teddy" :url "images/teddy_wave_animation_scaled.png"}])
     (fn [director]
       (log/info logger " Images loaded. Creating scene.")
       (doto director
         (.addAudio "chime" "sounds/22267__zeuss__the-chime.wav")
         (.addAudio "kloing" "sounds/26875__cfork__cf-fx-batch-jingle-glock-n-kloing.wav")
         (.addAudio "click" "sounds/39562__the-bizniss__mouse-click.wav")
         (.addAudio "splat" "sounds/42960__freqman__splat-10.wav")
         (.addAudio "doppp" "sounds/8001__cfork__cf-fx-doppp-01.wav")
         (.addAudio "flopp" "sounds/8006__cfork__cf-fx-flopp-03-dry.wav")
         (.addAudio "munching" "sounds/27877__inequation__munching.wav"))
       (let [scene (. director (createScene))
             background (doto (CAAT/Actor.) 
                          (.setBackgroundImage (.getImage director "room")))
             object-count 7
             counter (gen-count-actor object-count)
             basket (gen-basket director :x 440 :y 240)
             objects (map (fn [index object]
                            ;; make sure to pass mouse events to the basket
                            (doto object
                              (.setLocation (+ (rand-int 30) (* 80 index) 100) (+ (rand-int 60) 370)) 
                              (make-draggable-into-basket director scene basket counter)))
                          (range object-count)
                          (take 7 (repeatedly (partial gen-teddy director))))] 
         (log/info logger " Displaying scene")
         (add! scene background)
         (add! scene basket)
         (add! scene counter) 
         (doseq [object objects]
          (add! scene object)))))))
