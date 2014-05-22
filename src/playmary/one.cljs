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

(defn piano-key-width
  [{piano-keys :piano-keys w :w}]
  (.round js/Math (/ w (count piano-keys))))

(defn latest-note
  [instrument freq]
  (some (fn [note] (and (= freq (-> note :freq)) note))
        (-> instrument :notes)))

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
  (let [freqs (-> instrument :piano-keys keys)
        piano-key-w (piano-key-width instrument)]
    (dotimes [n (count freqs)]
      (let [note (latest-note instrument (nth freqs n))
            piano-key-on (and note (-> note :off nil?))]
        (set! (.-fillStyle draw-ctx)
              ((if piano-key-on :light :dark) (nth colors (mod n (count colors)))))
        (.fillRect draw-ctx
                   (* n piano-key-w)
                   (t->px instrument playhead)
                   piano-key-w h)))))

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
    {:piano-keys (into (sorted-map) (map-indexed (fn [i freq] [freq {:n i}])
                                                 scale))
     :notes [] :w 0 :h 0 :sound-ready false
     :px-per-ms 0.04
     :start start
     :playhead start}))

(defn add-synths-to-instrument
  [instrument]
  (assoc (reduce (fn [a x] (assoc-in a [:piano-keys x :synth] (create-note-synth x)))
                 instrument
                 (-> instrument :piano-keys keys))
    :sound-ready true))

(defn touch-data->touches [touch-data]
  (let [event (.-event_ touch-data)
        touches (js->clj (.-changedTouches event) :keywordize-keys true)]
    (map (fn [x] {:type (.-type event)
                  :touch-id (:identifier x)
                  :clientX (:clientX x)
                  :time (.-timeStamp event)})
         (for [x (range (:length touches))] ((keyword (str x)) touches)))))

(defn touch->freq
  [instrument touch]
  (touch->note (get touch :clientX)
               instrument))

(def instrument-fns
  {"touchstart" (fn
                  [instrument {touch-id :touch-id time :time :as event}]
                  (let [freq (touch->freq instrument event)
                        piano-key (get-in instrument [:piano-keys freq])]
                    (do
                      ;; (println "play")
                      (.play (.bang (:synth piano-key)))
                      (assoc instrument :notes
                             (conj (get instrument :notes)
                                   {:freq freq :on time :off nil :touch-id touch-id})))))
   "touchend" (fn
                [{piano-keys :piano-keys :as instrument}
                 {touch-id :touch-id time :time :as event}]
                (assoc instrument :notes
                       (map (fn [note]
                              (if (= touch-id (-> note :touch-id))
                                (do
                                  ;; (println "release")
                                  (.release (:synth (get piano-keys (-> note :freq))))
                                  (assoc note :off time))
                                note))
                            (-> instrument :notes))))})

(defn fire-event-on-instrument
  [instrument event]
  (let [f-instrument (get instrument-fns (:type event))]
    (or (and f-instrument (f-instrument instrument event))
        instrument)))

(defn maybe-init-synths
  [instrument]
  (if (:sound-ready instrument)
    instrument
    (add-synths-to-instrument instrument)))

(defn fire-touch-data-on-instrument
  [instrument data]
  (reduce fire-event-on-instrument
          (maybe-init-synths instrument)
          (touch-data->touches data)))

(defn update-size [instrument canvas-id]
  (let [{w :w h :h :as window-size} (util/get-window-size)]
    (do (util/set-canvas-size! canvas-id window-size)
        (.scrollTo js/window 0 0) ;; Safari leaves window part scrolled down after turn
        (assoc (assoc instrument :h h) :w w))))

(defn create-touch-input-channel
  [canvas-id]
  (async/merge [(util/listen (dom/getElement canvas-id) :touchstart)
                (util/listen (dom/getElement canvas-id) :touchend)]))

(let [canvas-id "canvas"
      c-instrument (chan)
      c-orientation-change (util/listen js/window :orientation-change)
      c-touch (create-touch-input-channel canvas-id)]

  (go
   (let [draw-ctx (util/get-ctx canvas-id)]
     (util/set-canvas-size! canvas-id (util/get-window-size))
     (loop [instrument (<! c-instrument)]
       (let [[data c] (alts! [c-instrument (timeout 16)])]
         (condp = c
           c-instrument (recur data)
           (do (draw-instrument draw-ctx instrument)
               (recur instrument)))))))

  (go
   (loop [instrument (update-size (create-instrument (scales/c-minor)) canvas-id)]
     (>! c-instrument instrument)
     (let [[data c] (alts! [c-touch c-orientation-change (timeout 16)])]
       (condp = c
         c-orientation-change (recur (update-size instrument canvas-id))
         c-touch (recur (fire-touch-data-on-instrument instrument data))
         (recur (assoc instrument :playhead (.getTime (js/Date.))))))))
  )
