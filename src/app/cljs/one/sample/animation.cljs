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

(defn gen-scale-behavior [& {:keys [from to time]
                             :or {from 0 to 0 time 120}}]
  (let [[start-time time] (if (vector? time) time [0 time])]
    (doto (CAAT/ScaleBehavior.)
      (.setFrameTime start-time time) 
      (.setValues from to from to))))

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
                      :scale (fn [arg] (apply gen-scale-behavior arg))
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
    (listen basket-body :mouse-enter (fn [mouseEvent] (. basket-container (doOpen))))
    (listen basket-body :mouse-exit (fn [mouseEvent] (. basket-container (doClose))))
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
    (. bubble (cacheAsBitmap))
    (doto actor-container
      (.addChild bubble)
      (.addChild text))
    (.centerAt text (/ w 2) (/ h 2))

    actor-container))

(def *positive-feedbacks* ["Super!" "Genau!" "Ja, sehr richtig!" "Ja, weiter so!" "Mmm, lecker!" "Gut gemacht!"])

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

(defn gen-count-actor [total-count & {:keys [on-total-reached] :or {on-total-reached (fn [c])}}]
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
            (pulsate! actor :count 2 :strength 1.3)
            (when (== data.count total-count)
              (on-total-reached total-count))))
    (. actor (inc))
    actor))


(defn make-draggable!
  "Given a TARGET, make it draggable. This will also add a pulsate
  effect on the target as the mouse hovers over it."
  [target scene]
  (let [director (director-of scene)]
    (m/wrap target.mouseDown [e oldf]
            (.audioPlay director "flopp")
            (pulsate! target :strength [1.1 1.4]))
    (m/wrap target.mouseUp [e oldf]
            (.audioPlay director "doppp"))
    (.setDiscardable target true)
    (listen target
            :mouse-enter
            (fn [e oldf] 
              (.addBehavior
               target
               (doto (CAAT/ScaleBehavior.)
                 (.setFrameTime (.time scene) 250)
                 (.setValues 1 1.1 1 1.1))) 
              (oldf target e)))
    (listen target :mouse-exit
            (fn [e oldf]
              (when (not target.trashed)
                (.addBehavior
                 target
                 (doto (CAAT/ScaleBehavior.)
                   (.setFrameTime (.time scene) 250)
                   (.setValues 1.1 1 1.1 1))))
              (oldf target e)))))

(defn make-draggable-into
  "Given a source, make it draggable in such a way that it can be
  dragged ontop of the destination. Return the source."
  [source destination scene action-fn & {:keys [valid-fn] :or {valid-fn (constantly true)}}]
  (let [shot? (atom false)]
   (listen source
           :mouse-drag
           (fn [event oldf]
             (when (not @shot?)
               (let [point-in-destination (.modelToModel source (.point event) destination)]
                 (when (and (.contains destination (.x point-in-destination) (.y point-in-destination)) (valid-fn))
                   (action-fn)
                   (reset! shot? true))))
             (oldf source event))))
  (make-draggable! source scene)
  source)

(defn image?
  "Returns true iff the argument is an HTMLImageElement,
  HTMLCanvasElement or a CAAT.SpriteImage."
  [img]
  (cond (= (.nodeName img) "IMG") true
        (= (.nodeName img) "CANVAS") true
        (instance? CAAT.SpriteImage img) true))

(defn ->sprite [img director & {:keys [rows cols] :or {rows 1 cols 1}}]
  ;; (log/info logger "->sprite")
  (assert (director? director) "Second arg must be a director instance.")
  (cond (string? img) (->image img director)
        (= (.nodeName img) "IMG") (.initialize (CAAT/SpriteImage.) img rows cols)
        (= (.nodeName img) "CANVAS")  (.initialize (CAAT/SpriteImage.) img rows cols)
        (instance? CAAT.SpriteImage img) img))

(defn ->image [value director & {:keys [thumbnail] :or {thumbnail nil}}]
  ;; (log/info logger (str "->image " value))
  (assert (director? director) "first arg must be a director.")
  (cond (string? value)
        (let [image (.getImage director value)
              _ (assert (not (nil? image)) "Could not find image.")
              image (if (vector? thumbnail)
                      (CAAT.modules.ImageUtil/createThumb image (first thumbnail) (second thumbnail) true)
                      image)]
          image)
        (image? value) value))


(defn draw-star [context & {:keys [radius color] :or {radius 80 color "green"}}]
  ;; (set! context.strokeStyle "black")
  ;; (set! context.lineWidth 2)
  (set! context.fillStyle color)
  (set! context.shadowColor color)
  (set! context.shadowBlur (/ radius 3))
  (. context (beginPath))
  (. context (moveTo 0 radius))
  (dotimes [n 10]
    (let [radius (if (== (mod n 2) 0) radius (* 0.5 radius))
          x (* radius (Math/sin (/ (* n 2 Math/PI) 10)))
          y (* radius (Math/cos (/ (* n 2 Math/PI) 10)))]
     (.lineTo context x y)))
  (. context (closePath))
  (. context (fill)) 
  ;; (. context (stroke))
  )

(defn gen-star [& {:keys [radius color] :or {radius 80 color "green"}}]
  (let [actor (doto (CAAT/Actor.)
                (.setBounds 0 0 radius radius))]
    (m/wrap actor.paint [director time oldf]
          (draw-star director.ctx :radius radius :color color))
    ;; (. actor (cacheAsBitmap))
    ;; (.enableDrag actor true)
    actor))

(defn actor? [s]
  (instance? CAAT.Actor s))

(defn scene? [s]
  (instance? CAAT.Scene s))

(defn director? [d]
  (instance? CAAT.Director d))

(defn listen
  "Listen on events for a CAAT.Actor. Possible events are:
   - :mouse-enter
   - :mouse-exit
   - :mouse-drag

   The handler function will be passed one additional parameter, which
   is the old handler that was active. The old handler must be called
   with the instance as its first argument, besides any other
   arguments."
  ([target event-name handler]
     (assert (actor? target))
     (assert (keyword? event-name))
     (assert (fn? handler))
     (cond (= event-name :mouse-enter)
           (m/wrap target.mouseEnter [e oldf]
                   (handler e oldf))
           (= event-name :mouse-exit)
           (m/wrap target.mouseExit [e oldf]
                   (handler e oldf))
           (= event-name :mouse-drag)
           (m/wrap target.mouseDrag [e oldf]
                   (handler e oldf))))
  ([target e1 h1 e2 h2 & more]
     (listen target e1 h1)
     (listen target e2 h2)
     (when more
       (apply listen target more))))

(defn gen-actor [scene & {:keys [image draggable? fill-style position size sprite-index]
                          :or {draggable? false}}]
  (assert (scene? scene) "First arg must be a scene.")
  (when (and image fill-style)
   (log/info logger (str "WARNING: both :image & :fill-style set. No image will be visible.")))
  (when (and fill-style (not size) (not image))
   (log/info logger (str "WARNING: :fill-style set, but no size specified. Nothing will be visible.")))
  (let [actor (if fill-style (CAAT/ShapeActor.) (CAAT/Actor.))]
    (when image
      ;; (log/info logger (pr-str "Image" "; tagName=" (.tagName image) "; nodeName=" (.nodeName image) "; image=" image "; ->image" (->image image (director-of scene))))
      (.setBackgroundImage actor (->image image (director-of scene))))
    (when size
      (.setSize actor (x-of size) (y-of size)))
    (when fill-style
      (.setFillStyle actor fill-style))
    (when position
      (.setLocation (x-of position) (y-of position)))
    (when (not (false? draggable?))
      (.enableDrag actor true))
    (when sprite-index
      (.setSpriteIndex actor sprite-index))
    actor))

(defn gen-animated-actor
  [scene & {:keys [image animation-indices default-index key-frames frame-time draggable? thumbnail]
               :or {image nil animation-indices [0] default-index 0 key-frames nil frame-time 150 draggable? false thumbnail nil}}]
  (assert (scene? scene) "First arg must be a scene instance.")
  (log/info logger (str "Creating actor"))
  (let [image (->image image (director-of scene) :thumbnail thumbnail) 
        img (doto (CAAT/SpriteImage.)
              (.initialize image 1 (or key-frames (+ 1 (apply max animation-indices)))) 
              (.setAnimationImageIndex (clj->js [default-index]))
              (.setChangeFPS frame-time)) 
        actor (gen-actor scene :image img :draggable? true)]
    (listen actor
            :mouse-enter
            (fn [e oldf] 
              (.setAnimationImageIndex img (clj->js animation-indices))
              (oldf actor e))
            :mouse-exit
            (fn [e oldf]
              (.setAnimationImageIndex img (clj->js [default-index]))
              (oldf actor e)))
    actor))


(defn fade! [target director & {:keys [start-time duration]
                                :or {start-time nil duration 2000}}]
  (doto target
    (.addBehavior (doto (CAAT/AlphaBehavior.)
                    (.setFrameTime (or start-time (.time director)) duration)
                    (.setValues 1 0)))))

(let [auto-expire-listener
      (clj->js
       {:behaviorExpired
        (fn [bh t a] 
          (.setExpired a true))})]
  (defn fall! [target director & {:keys [duration] :or {duration 2000}}]
    (let [cpX (+ (.x target) (- (rand-int 200) 100))
          cpY (+ (.y target) (- (rand-int 200) 100))
          nX (+ (.x target) (* 2 (- cpX (.x target))))
          nY (+ (.height director) 10)]
      (doto target
        (.setDiscardable true)
        (.addBehavior (doto (CAAT/PathBehavior.)
                        (.setFrameTime (.time director) duration)
                        (.setPath (doto (CAAT/Path.)
                                    (.beginPath (.x target) (.y target))
                                    (.addQuadricTo cpX cpY nX nY)
                                    (. (endPath))))
                        (.addListener auto-expire-listener)))))))

(defn director-of [what]
  (cond (instance? CAAT.Scene what) (.parent what)
        (instance? CAAT.ActorContainer what) (director-of (.parent what))
        (instance? CAAT.Actor what) (director-of (.parent what))
        true nil))

(defn scene-of [what]
  (cond (instance? CAAT.Scene what) what
        (instance? CAAT.ActorContainer what) (scene-of (.parent what))
        (instance? CAAT.Actor what) (scene-of (.parent what))
        true nil))

(defn repeatedly-create-stars [target scene &
                               {:keys [start-time start-position batch-count repeat-interval on-create]
                                :as kw
                                :or {start-time 0 batch-count 30 start-position [400 50] repeat-interval 2000 on-create (fn [t])}}]
  (let [director (director-of scene)] 
    (.createTimer
     scene
     (.time scene)
     start-time
     (fn [scene-time timer-time timetask]
       (on-create scene-time)
       (dotimes [x batch-count]
         (let [rotation-direction (if (< (Math/random) 0.5) -1 1)
               star (doto (gen-star :radius (rand-nth (map (comp dec dec dec) [12 14 16 18 20]))
                                    :color (rand-nth ["yellow" "chartreuse" "orange" "red" "powderblue" "magenta" "cyan"])) 
                      (.enableEvents false)
                      (.setLocation (+ (x-of start-position) (* x 10)) (+ (y-of start-position) (rand-int 30)))
                      ;; (. (cacheAsBitmap))
                      (.addBehavior
                       (gen-rotate-behavior :from 0 :to (* rotation-direction (+ 180 (* (* (rand-int 2) -1) (rand-int 180))))
                                            :time [scene-time 3000]))
                      ;; TODO: why is the following not working? (rotation stops after some seconds)
                      ;; (animate [:rotate [:from 0 :to (+ 180 (* (* (rand-int 2) -1) (rand-int 180)))
                      ;;                    :time [scene-time 3000]]])
                      (fall! director :duration (+ 2000 (rand-int 1000)))
                      (.addBehavior (doto (CAAT/AlphaBehavior.)
                                      (.setFrameTime scene-time 500)
                                      (.setValues 0 1))
                                    )
                      (fade! director
                             :duration (+ 3000 (rand-int 1000))
                             :start-time (+ scene-time 500)))]
           (add! target star)
           ;; randomize the star z
           (.setZOrder target star (- (rand-int batch-count) (/ batch-count 2))) 
           ))
       (repeatedly-create-stars target scene
              :start-time repeat-interval
              :start-position start-position
              :batch-count batch-count
              :repeat-interval repeat-interval
              :on-create on-create)
       )
     (fn []) (fn []))))

(defn gen-winning-screen [scene]
  (let [director (director-of scene) 
        mask (doto (CAAT/ShapeActor.)
               (.setShape CAAT.ShapeActor.prototype.SHAPE_RECTANGLE) 
               (.setBounds 0 0 (.width director) (.height director)) 
               (.setFillStyle "#000000")
               (.setAlpha 0.7))
        text (doto (CAAT/TextActor.)
               (.setText (rand-nth ["PRIMA!" "SUPER!" "TOLL!"]))
               (.setFont "90px Comic Sans Ms")
               (.setFillStyle "orange")
               (.setOutline true)
               (.setOutlineColor "black")
               (.calcTextSize director)
               (.centerAt (/ (.width director) 2) (/ (.height director) 2)))
        subtext (doto (CAAT/TextActor.)
                  (.setText "Du hast ein Lied freigespielt.")
                  (.setFont "30px Comic Sans Ms")
                  (.setFillStyle "yellow")
                  (.setAlpha 0.7)
                  (.setOutline true)
                  (.setOutlineColor "black")
                  (.calcTextSize director)
                  (.centerAt (/ (.width director) 2) (/ (.height director) 2)))
        song-subtext
        (doto (CAAT/TextActor.)
          (.setText "Name: The Sound of Music 3 - Author: larrylarrybb")
          (.setFont "20px Comic Sans Ms")
          (.setFillStyle "red")
          (.setAlpha 0.7)
          (.setOutline true)
          (.setOutlineColor "black")
          (.calcTextSize director)
          (.centerAt (/ (.width director) 2) (/ (.height director) 2)))
        song (.getAudio (. director (getAudioManager)) "song")
        song-supported? (and song (not (not (and song.canPlayType (.replace (.canPlayType song "audio/mpeg;") #"no" "")))))
        song-playing? (atom false)
        play-icon (let [img (->image "play" director :thumbnail [35 35])
                        img2 (->image "stop" director :thumbnail [35 35])
                        actor (doto (CAAT/Actor.)
                                (.setBackgroundImage img))
                        audio-loop (atom nil)] 
                    (set! actor.mouseClick
                          (fn [e]
                            (if (not @song-playing?)
                              (do (reset! song-playing? true)
                                  (log/info logger (pr-str "Playing song" song))
                                  (reset! audio-loop (.audioLoop director "song"))
                                  (.setBackgroundImage actor img2))
                              (do (reset! song-playing? false)
                                  (log/info logger "Stopping song playback")
                                  (. @audio-loop (pause))
                                  (reset! audio-loop nil)
                                  (.setBackgroundImage actor img)))))
                    actor)
        container (doto (CAAT/ActorContainer.)
                    (.setBounds 0 0 (.width director) (.height director))
                    (add! mask)
                    (add! text) 
                    ;; (.setZOrder text 8)
                    ;; (.enableEvents true)
                    )]
    (if song-supported?
      (doto container
        (add! play-icon)
        (add! subtext) 
        (add! song-subtext)))
    ;; put the subtext beneath the main text
    (.setLocation subtext (.x subtext) (+ (.y text) (.textHeight text) 10))
    (.setLocation song-subtext (.x subtext) (+ (.y text) 220 10))
    (.centerAt play-icon (/ (.width director) 2) (+ 140 (/ (.height director) 2)))
    ;; (.setLocation play-icon (+ (.x subtext) (.textWidth subtext)) (+ (.y subtext) 5))
    (m/wrap text.paint [director time default]
            (let [ctx (.ctx director)]
              (set! ctx.shadowColor "yellow")
              (set! ctx.shadowBlur 20)
              (default text director time)))
    (let [animate-text
          (fn animate-text []
            (let [r1 (+ 1 (rand-int 4))
                  r2 (- (+ 1 (rand-int 4)))]
              (animate text
                       [:rotate [:from 0 :to r1 :time 1000]]
                       [:rotate [:from r1 :to r2 :time 1000]]
                       [:rotate [:from r2 :to 0 :time 1000]]
                       (fn [t] (animate-text)))))
          animate-text2
          (fn animate-text []
            (animate text
                     [:scale [:from 1 :to 1.05 :time 1000]]
                     [:scale [:from 1.05 :to 1 :time 1000]]
                     (fn [t] (animate-text))))]
      (animate-text)
      (animate-text2)) 
    (dotimes [i 11]
      (repeatedly-create-stars
       container scene
       :start-time (* i 100)
       :repeat-interval 2000
       :batch-count 5
       :start-position [(* 60 (+ 1 i))
                        (+ 50 (* (mod i 2) 50))]
       ;; play a sound which gets gradually more silent
       :on-create (if (== i 0)
                    (let [volume (atom 1)]
                      (fn [t]
                        (when (not @song-playing?)
                         (let [sound (.getAudio (. director (getAudioManager)) "chime")]
                           (.audioPlay director "chime")
                           (set! sound.volume @volume)
                           (reset! volume (max 0.2 (- @volume 0.2))))))) (fn [t]))
       ))
    container))

(defn gen-bookshelf [scene]
  (let [container (doto (CAAT/ActorContainer.)
                    (.setBounds 110 135 180 60)
                    ;; (.setFillStyle "green") ;; set this to see the area where a book can be placed
                    )
        img (-> "books"
              (->image (director-of scene) :thumbnail [60 60])
              (->sprite (director-of scene) :cols 3)) 
        books (gen-actor scene :image img :sprite-index 0)]
    (set! container.inc-book-count
          (fn []
            (.setSpriteIndex books (mod (+ (.spriteIndex img) 1) 3))))
    (doto container
      (add! books :position [10 13]))))
 
(defn ^:export initialize-views
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
               {:id "teddy" :url "images/teddy_wave_animation_scaled.png"}
               {:id "book" :url "images/book.png"}
               {:id "books" :url "images/books.png"}
               {:id "play" :url "images/1194999000504424746player_play.svg.thumb.png"}
               {:id "stop" :url "images/1194999023691726266player_stop.svg.thumb.png"}
               {:id "blocks" :url "images/1194989520603923742unit_blocks_lion_kimbro_01.svg.thumb.png"}
               {:id "car" :url "images/1245691880329073007ronaldrhouston_sport_car.svg.thumb.png"}
               {:id "football" :url "images/football-th.png"}
               ])
     (fn [director]
       (log/info logger " Images loaded. Creating scene.")
       (doto director
         (.addAudio "chime" "sounds/22267__zeuss__the-chime.ogg" ;; "sounds/22267__zeuss__the-chime.mp3"
                    ) 
         (.addAudio "doppp" "sounds/8001__cfork__cf-fx-doppp-01.ogg" ;; "sounds/8001__cfork__cf-fx-doppp-01.wav"
                    )
         (.addAudio "flopp" "sounds/8006__cfork__cf-fx-flopp-03-dry.ogg" ;; "sounds/8006__cfork__cf-fx-flopp-03-dry.wav"
                    )
         (.addAudio "munching" "sounds/27877__inequation__munching.ogg" ;; "sounds/27877__inequation__munching.wav"
                    )
         (.addAudio "song" "sounds/448614_Happiness_Alone.ogg"))
       (let [scene (. director (createScene))
             background (doto (CAAT/Actor.) 
                          (.setBackgroundImage (.getImage director "room")))
             basket (gen-basket director :x 440 :y 240)
             bookshelf (gen-bookshelf scene)
             counter (gen-count-actor 6 :on-total-reached (fn [e] (add! scene (gen-winning-screen scene))))
             basket-objects (map
                             #(make-draggable-into % basket scene 
                                                   (fn [] (throw-into-basket director scene basket % counter))
                               :valid-fn (fn [] (and (not (.isOpened basket)) (not (.eating basket)))))
                             [(gen-animated-actor scene :image "football" :animation-indices [0] :draggable? true :thumbnail [60 60])
                              (gen-animated-actor scene :image "car" :animation-indices [0] :draggable? true :thumbnail [60 60])
                              (gen-animated-actor scene :image "blocks" :animation-indices [0] :draggable? true)
                              (gen-animated-actor scene :image "teddy" :animation-indices [0 1 0 2] :draggable? true)])
             bookshelf-objects (map
                                #(make-draggable-into % bookshelf scene 
                                  (fn []
                                    (.setExpired % true)
                                    (. bookshelf (inc-book-count))
                                    (. counter (inc))))
                                [(gen-animated-actor scene :image "book" :animation-indices [2] :default-index 2 :key-frames 3 :draggable? true)
                                 (gen-animated-actor scene :image "book" :animation-indices [0] :default-index 0 :key-frames 3 :draggable? true)])
             objects (concat basket-objects bookshelf-objects)] 
         (log/info logger " Displaying scene")
         (add! scene background)
         (add! scene basket)
         (add! scene counter) 
         (add! scene bookshelf)
         
         (dotimes [index (count objects)]
           (let [object (nth objects index)]
             (add!
              scene
              object
              :position [(+ (rand-int 30) (* (/ 600 (count objects)) index) 100)
                         (+ (rand-int 50) 360)])))
         ;; (add! scene (gen-winning-screen scene))
         )))))
