(ns playmary.one
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [goog.dom :as dom]
            [goog.events :as events]
            [cljs.core.async :as async :refer [<! put! chan timeout sliding-buffer close!]]
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

(defn touch->note
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
     :w 0 :h 0 :sound-ready false
     :px-per-ms 0.1
     :cur-touches {}
     :scrolling? false
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
                  :position {:x (:clientX x) :y (:clientY x)}
                  :time (.-timeStamp event)})
         (for [x (range (:length touches))] ((keyword (str x)) touches)))))

(defn touch->freq
  [instrument touch]
  (touch->note (get-in touch [:position :x])
               instrument))

(defn maybe-init-synths
  [instrument]
  (if (:sound-ready instrument)
    instrument
    (add-synths-to-instrument instrument)))

(defn filter-type
  [type touches]
  (filter (fn [t] (= (t :type) type)) touches))

(defn reduce-val->>
  [f coll val]
  (reduce f val coll))

(defn touches->notes
  [{playhead :playhead :as instrument} touches]
  (->> touches
       (map (fn [t] (assoc t :freq (touch->freq instrument t))))
       (remove (fn [t] (get-in instrument [:piano-keys (t :freq) :on?])))
       (map (fn [t] {:freq (t :freq)
                     :on playhead
                     :off nil
                     :touch-id (t :touch-id)}))))

(defn start-notes
  [touches {notes :notes :as instrument}]
  (let [new-notes (touches->notes instrument (filter-type "touchstart" touches))]
    (reduce play-piano-key
            (assoc instrument :notes (concat new-notes notes))
            (map :freq new-notes))))

(defn end-notes
  [touches {notes :notes playhead :playhead :as instrument}]
  (let [off-touch-ids (->> (filter-type "touchend" touches)
                           (map :touch-id)
                           set)
        note-off? (fn [n] (contains? off-touch-ids (n :touch-id)))]
    (-> instrument
        (assoc :notes (map (fn [n] (if (note-off? n) (assoc n :off playhead) n)) notes))
        (->> (reduce-val->> stop-piano-key (->> notes (filter note-off?) (map :freq)))))))
(defn x-distance
  [{{end-x :x} :position :as t} instrument]
  (- (get-in instrument [:cur-touches (t :touch-id) :position :x]) end-x))

(defn y-distance
  [{{end-y :y} :position :as t} instrument]
  (- (get-in instrument [:cur-touches (t :touch-id) :position :y]) end-y))

(defn distance
  [t instrument]
  (.sqrt js/Math (+ (.pow js/Math (x-distance t instrument) 2)
                    (.pow js/Math (y-distance t instrument) 2))))

(defn filter-scroll-touches
  [touches instrument]
  (filter (fn [t] (> (.abs js/Math (distance t instrument)) 10))
          (filter-type "touchmove" touches)))

(defn record-touches
  [touches instrument]
  (let [starts (filter-type "touchstart" touches)
        moves (filter-type "touchmove" touches)
        ends (filter-type "touchend" touches)]
    (-> instrument
        (update-in [:cur-touches] into (map vector (map :touch-id starts) starts))
        (update-in [:cur-touches] into (map vector (map :touch-id moves) moves))
        (update-in [:cur-touches] (fn [c] (apply dissoc c (map :touch-id ends)))))))

(defn scroll
  [scroll-touches {playhead :playhead px-per-ms :px-per-ms :as instrument}]
  (if-let [scroll-touch (first (sort (fn [a b] (max (.abs js/Math (y-distance a instrument))
                                                    (.abs js/Math (y-distance b instrument))))
                                     scroll-touches))]
    (-> instrument
        (assoc :scrolling? true)
        (assoc :playhead (+ playhead (/ (y-distance scroll-touch instrument) px-per-ms))))
    (assoc instrument :scrolling? false)))

(defn delete-scrolled-notes
  [scroll-touches {notes :notes :as instrument}]
  (let [scroll-touch-ids (set (map :touch-id scroll-touches))
        delete-notes (group-by (fn [t] (and (nil? (t :off))
                                            (contains? scroll-touch-ids (t :touch-id))))
                               notes)]
    (-> instrument
        (assoc :notes (get delete-notes false))
        (->> (reduce-val->> stop-piano-key (map :freq (get delete-notes true)))))))

(defn handle-scrolling
  [touches instrument]
  (let [scroll-touches (filter-scroll-touches touches instrument)]
    (->> instrument
         (scroll scroll-touches)
         (delete-scrolled-notes scroll-touches)
         (record-touches touches))))

(defn fire-touch-data-on-instrument
  [instrument data]
  (let [touches (touch-data->touches data)]
    (->> instrument
         maybe-init-synths
         (start-notes touches)
         (end-notes touches)
         (handle-scrolling touches))))

(defn update-size [instrument canvas-id]
  (let [{w :w h :h :as window-size} (util/get-window-size)]
    (do (util/set-canvas-size! canvas-id window-size)
        (.scrollTo js/window 0 0) ;; Safari leaves window part scrolled down after turn
        (assoc (assoc instrument :h h) :w w))))

(defn touches
  [canvas-id type]
  (map< touch-data->touches (util/listen (dom/getElement canvas-id) type)))

(defn create-touch-input-channel
  [canvas-id]
  (async/merge [(util/listen (dom/getElement canvas-id) :touchstart)
                (util/listen (dom/getElement canvas-id) :touchend)
                (util/listen (dom/getElement canvas-id) :touchmove)]))

(defn step-time
  [instrument delta]
  (if (not (instrument :scrolling?))
    (assoc instrument :playhead (+ (instrument :playhead) delta))
    instrument))

(defn tick
  [instrument delta]
  (step-time instrument delta))

(prevent-scrolling)
(set-up-web-audio-on-first-touch)

(let [canvas-id "canvas"
      c-instrument (chan (sliding-buffer 1))
      c-orientation-change (util/listen js/window :orientation-change)
      c-touch (create-touch-input-channel canvas-id)
      frame-delay 16]

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
     (let [[data c] (alts! [c-touch c-orientation-change (timeout frame-delay)])]
       (condp = c
         c-orientation-change (recur (update-size instrument canvas-id))
         c-touch (recur (fire-touch-data-on-instrument instrument data))
         (recur (tick instrument frame-delay)))))))
