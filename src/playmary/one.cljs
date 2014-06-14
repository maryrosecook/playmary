(ns playmary.one
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [goog.dom :as dom]
            [goog.events :as events]
            [cljs.core.async :as async :refer [<! >! put! chan timeout sliding-buffer close!
                                               mapcat< onto-chan closed?]]
            [playmary.util :as util]
            [playmary.scales :as scales]))

(def timbre js/T)

(def colors [{:note "#676767" :light "#6DA0CB" :dark "#000000"}
             {:note "#929292" :light "#A76AB9" :dark "#1E1E1E"}
             {:note "#B9B9B9" :light "#BB67A2" :dark "#3D3D3D"}
             {:note "#DCDCDC" :light "#C55D83" :dark "#5C5C5C"}
             {:note "#FFFFFF" :light "#D35E4C" :dark "#7A7A7A"}
             {:note "#000000" :light "#E18C43" :dark "#999999"}
             {:note "#393939" :light "#E1B040" :dark "#B9B9B9"}])

(defn prevent-scrolling
  []
  (set! (.-ontouchmove js/document) (fn [e] (.preventDefault e))))

(defn set-up-web-audio-on-first-touch
  []
  (set! (.-ontouchstart js/document)
        (fn []
          (-> "sin" timbre .play .pause)
          (set! (.-ontouchstart js/document) nil))))

(defn piano-key-width
  [{piano-keys :piano-keys w :w}]
  (.round js/Math (/ w (count piano-keys))))

(defn t->px
  [{start :start px-per-ms :px-per-ms} t]
  (* px-per-ms (- t start)))

(defn note-rect
  [{on :on off :off freq :freq :as note}
   {px-per-ms :px-per-ms playhead :playhead :as instrument}]
  (let [piano-key-w (piano-key-width instrument)]
    {:x (* piano-key-w (get-in instrument [:piano-keys freq :n]))
     :y (+ (t->px instrument on)
           (/ (-> instrument :h) 2))
     :w piano-key-w
     :h (- (t->px instrument (or off playhead))
           (t->px instrument on))}))

(defn screen-rect
  [{w :w h :h playhead :playhead :as instrument}]
  {:x 0 :y (t->px instrument playhead) :w w :h h})

(defn draw-note
  [draw-ctx instrument note]
  (let [{x :x y :y w :w h :h} (note-rect note instrument)]
    (set! (.-fillStyle draw-ctx) "white")
    (.fillRect draw-ctx x y w h)))

(defn colliding?
  [r1 r2]
  (not (or (< (+ (:x r1) (:w r1)) (:x r2))
           (< (+ (:y r1) (:h r1)) (:y r2))
           (> (:x r1) (+ (:x r2) (:w r2)))
           (> (:y r1) (+ (:y r2) (:h r2))))))

(defn on-screen?
  [instrument note]
  (colliding? (note-rect note instrument)
              (screen-rect instrument)))

(defn draw-notes
  [draw-ctx instrument]
  (doseq [note (filter (partial on-screen? instrument)
                       (-> instrument :notes))]
    (draw-note draw-ctx instrument note)))

(defn draw-piano-keys
  [draw-ctx {w :w h :h playhead :playhead :as instrument}]
  (let [piano-keys (-> instrument :piano-keys)
        piano-key-w (piano-key-width instrument)]
    (doseq [[n [freq piano-key]] (map-indexed vector piano-keys)]
      (set! (.-fillStyle draw-ctx)
            ((if (piano-key :on?) :light :dark) (nth colors (mod n (count colors)))))
      (.fillRect draw-ctx
                 (* n piano-key-w)
                 (t->px instrument playhead)
                 piano-key-w h))))

(defn draw-instrument
  [draw-ctx {w :w h :h playhead :playhead :as instrument}]
  (.save draw-ctx)
  (.translate draw-ctx 0 (-> (t->px instrument playhead) -))
  (.clearRect draw-ctx 0 0 w h)
  (draw-piano-keys draw-ctx instrument)
  (draw-notes draw-ctx instrument)
  (.restore draw-ctx))

(defn touch->piano-key
  [x instrument]
  (let [note-index (.floor js/Math (/ x (piano-key-width instrument)))]
    (nth (-> instrument :piano-keys keys) note-index)))

(defn create-note-synth
  [freq]
  (timbre "adsr"
          (js-obj "a" 5 "d" 10000 "s" 0 "r" 500)
          (timbre "fami" (js-obj "freq" freq "mul" 0.1))))

(defn create-instrument
  [scale]
  (let [start (.getTime (js/Date.))]
    {:piano-keys (into (sorted-map) (map-indexed (fn [i freq] [freq {:n i :on? false}])
                                                 scale))
     :notes ()
     :w 0 :h 0
     :sound-ready false
     :px-per-ms 0.1
     :scrolling-touch-id nil
     :start start
     :playhead start}))

(defn add-synths-to-instrument
  [instrument]
  (assoc (reduce (fn [a x] (assoc-in a [:piano-keys x :synth] (create-note-synth x)))
                 instrument
                 (-> instrument :piano-keys keys))
    :sound-ready true))

(defn play-piano-key
  [instrument freq]
  (println "play")
  (.play (.bang (get-in instrument [:piano-keys freq :synth])))
  (assoc-in instrument [:piano-keys freq :on?] true))

(defn stop-piano-key
  [instrument freq]
  (if (get-in instrument [:piano-keys freq :on?])
    (do
      (println "stop")
      (.release (get-in instrument [:piano-keys freq :synth]))
      (assoc-in instrument [:piano-keys freq :on?] false))
    instrument))

(defn touch-data->touches [touch-data]
  (let [event (.-event_ touch-data)
        touches (js->clj (.-changedTouches event) :keywordize-keys true)]
    (map (fn [x] {:type (.-type event)
                  :touch-id (:identifier x)
                  :age 0 ;; ticks
                  :position {:x (:clientX x) :y (:clientY x)}
                  :time (.-timeStamp event)})
         (for [x (range (:length touches))] ((keyword (str x)) touches)))))

(defn touch->freq
  [instrument touch]
  (touch->piano-key (get-in touch [:position :x])
                    instrument))

(defn maybe-init-synths
  [instrument]
  (if (:sound-ready instrument)
    instrument
    (add-synths-to-instrument instrument)))

(defn reduce-val->>
  [f coll val]
  (reduce f val coll))

(defn touch->note
  [{playhead :playhead :as instrument} touch]
  (let [freq (touch->freq instrument touch)]
    (if (not (get-in instrument [:piano-keys freq :on?]))
      {:freq freq
       :on playhead
       :off nil
       :touch-id (touch :touch-id)})))

(defn start-note
  [touch {notes :notes :as instrument}]
  (if-let [new-note (touch->note instrument touch)]
    (-> instrument
        (maybe-init-synths)
        (play-piano-key (new-note :freq))
        (assoc :notes (conj notes new-note)))
    instrument))

(defn end-notes
  [touch {notes :notes playhead :playhead :as instrument}]
  (let [note-off? (fn [n] (= (touch :touch-id) (n :touch-id)))]
    (-> instrument
        (assoc :notes (map (fn [n] (if (note-off? n) (assoc n :off playhead) n)) notes))
        (->> (reduce-val->> stop-piano-key (->> notes (filter note-off?) (map :freq)))))))

(defn distance
  [{x1 :x y1 :y} {x2 :x y2 :y}]
  (.sqrt js/Math (+ (.pow js/Math (- x2 x1) 2) (.pow js/Math (- y2 y1) 2))))

(defn scroll
  [touch {playhead :playhead px-per-ms :px-per-ms :as instrument}]
  (-> instrument
      (assoc :scroll-touch-id (touch :touch-id))
      (assoc :playhead (+ playhead (/ (touch :y-distance) px-per-ms)))))

(defn stop-scroll
  [touch instrument]
  (if (= (touch :touch-id) (instrument :scroll-touch-id))
    (assoc instrument :scroll-touch-id nil)
    instrument))

(defn fire-touch-on-instrument
  [touch instrument]
  (condp = (touch :type)
    "touchstart" (start-note touch instrument)
    "touchmove" (scroll touch instrument)
    "touchend" (->> instrument
                    (end-notes touch)
                    (stop-scroll touch))))

(defn update-size [instrument canvas-id]
  (let [{w :w h :h :as window-size} (util/get-window-size)]
    (do (util/set-canvas-size! canvas-id window-size)
        (.scrollTo js/window 0 0) ;; Safari leaves window part scrolled down after turn
        (assoc (assoc instrument :h h) :w w))))

(defn touches
  [canvas-id type]
  (mapcat< touch-data->touches (util/listen (dom/getElement canvas-id) type)))

(defn scroll?
  [touches cur-t]
  (let [prev-t (get touches (cur-t :touch-id))]
    (and prev-t
         (= (cur-t :type) "touchmove")
         (or (= (prev-t :type) "touchmove")
             (and (= (prev-t :type) "touchstart")
                  (= (prev-t :age) 0)
                  (> (distance (cur-t :position) (prev-t :position)) 5))))))

(defmulti handle-touch (fn [t touches out-c] (:type t)))

(defmethod handle-touch "touchstart"
  [{touch-id :touch-id :as t} touches out-c]
  (assoc touches touch-id t))

(defmethod handle-touch "touchmove"
  [{touch-id :touch-id :as t} touches out-c]
  (if (scroll? touches t)
    (let [{{x1 :x y1 :y} :position} (get touches touch-id)
          {{x2 :x y2 :y} :position} t]
      (assoc touches touch-id
             (-> t (assoc :x-distance (- x1 x2)) (assoc :y-distance (- y1 y2)))))
    (dissoc touches touch-id)))

(defmethod handle-touch "touchend"
  [{touch-id :touch-id :as t} touches out-c]
  (assoc touches touch-id t))

(defn emit-touch?
  [{t-type :type t-age :age} touches]
  (or (and (or (= t-type "touchstart") (= t-type "touchend"))
           (> t-age 0))
      (= t-type "touchmove")))

(defn touch-tick
  [touches out-c]
  (let [{emit true keep false} (group-by emit-touch? (vals touches))]
    (onto-chan out-c (or (vals (select-keys touches (map :touch-id emit))) '()) false)
    (reduce (fn [m k] (update-in m [k :age] inc))
            (select-keys touches (map :touch-id keep))
            (map :touch-id keep))))

(defn touch-chan
  [canvas-id wait]
  (let [out-c (chan)
        c-touchstart (touches canvas-id :touchstart)
        c-touchmove (touches canvas-id :touchmove)
        c-touchend (touches canvas-id :touchend)]
    (go (loop [touches {}]
          (let [[v _] (alts! [c-touchstart c-touchmove c-touchend (timeout wait)])]
            (if (nil? v)
              (recur (touch-tick touches out-c))
              (recur (handle-touch v touches out-c))))))
    out-c))

(defn step-time
  [instrument delta]
  (if (not (instrument :scroll-touch-id))
    (assoc instrument :playhead (+ (instrument :playhead) delta))
    instrument))

(defn tick
  [instrument delta]
  (step-time instrument delta))

(prevent-scrolling)
(set-up-web-audio-on-first-touch)

(let [canvas-id "canvas"
      frame-delay 16
      c-instrument (chan (sliding-buffer 1))
      c-orientation-change (util/listen js/window :orientation-change)
      c-touch (touch-chan canvas-id frame-delay)]

  (go
   (let [draw-ctx (util/get-ctx canvas-id)]
     (util/set-canvas-size! canvas-id (util/get-window-size))
     (loop [instrument (<! c-instrument)
            timer (timeout frame-delay)]
       (let [[data c] (alts! [c-instrument timer])]
         (condp = c
           c-instrument (recur data timer)
           (do (draw-instrument draw-ctx instrument)
               (recur instrument (timeout frame-delay))))))))

  (go
   (loop [instrument (update-size (create-instrument (scales/c-minor)) canvas-id)]
     (>! c-instrument instrument)
     (let [[data c] (alts! [c-orientation-change c-touch (timeout frame-delay)])]
       (condp = c
         c-orientation-change (recur (update-size instrument canvas-id))
         c-touch (recur (fire-touch-on-instrument data instrument))
         (recur (tick instrument frame-delay)))))))
