(ns playmary.one
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [goog.dom :as dom]
            [goog.events :as events]
            [cljs.core.async :refer [<! put! chan timeout merge sliding-buffer close!]]
            [playmary.util :as util]
            [playmary.scales :as scales]))

(def timbre js/T)

(def colors [{:light "#6DA0CB" :dark "#000000"}
             {:light "#A76AB9" :dark "#2E2E2E"}
             {:light "#BB67A2" :dark "#525252"}
             {:light "#C55D83" :dark "#818181"}
             {:light "#D35E4C" :dark "#AAAAAA"}
             {:light "#E18C43" :dark "#D7D7D7"}
             {:light "#E1B040" :dark "#FFFFFF"}])

(defn piano-key-width
  [piano-keys w]
  (.round js/Math (/ w (count piano-keys))))

(defn latest-note
  [instrument freq]
  (util/find-first (fn [note] (= freq (-> note :freq)))
                   (-> instrument :notes)))

(defn draw-instrument [draw-ctx instrument]
  (let [{w :w h :h} instrument
        freqs (-> instrument :piano-keys keys)
        piano-key-w (piano-key-width (:piano-keys instrument) w)]
    (.clearRect draw-ctx 0 0 w h)
    (dotimes [n (count freqs)]
      (let [note (latest-note instrument (nth freqs n))
            piano-key-on (and note (-> note :off nil?))]
        (set! (.-fillStyle draw-ctx)
              ((if piano-key-on :light :dark)
               (nth colors (mod n (count colors)))))
        (.fillRect draw-ctx (* n piano-key-w) 0 piano-key-w h)))))

(defn touch->note
  [x w scale]
  (let [note-index (.floor js/Math (/ x (piano-key-width scale w)))]
    (nth scale note-index)))

(defn create-note-synth
  [freq]
  (timbre "adsr"
          (js-obj "a" 5 "d" 10000 "s" 0 "r" 500)
          (timbre "fami" (js-obj "freq" freq "mul" 0.1))))

(defn create-instrument
  [scale]
  {:piano-keys (into (sorted-map) (map (fn [freq] [freq {}])
                                       scale))
   :notes [] :w 0 :h 0 :sound-ready false})

(defn add-synths-to-instrument
  [instrument]
  (assoc (reduce (fn [a x] (assoc-in a [:piano-keys x :synth] (create-note-synth x)))
                 instrument
                 (-> instrument :piano-keys keys))
    :sound-ready true))

(defn audio-time []
  (.-currentTime js/audioCtx))

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
               (:w instrument)
               (keys (:piano-keys instrument))))

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

(defn fire-touch-data-on-instrument
  [instrument data]
  (if (-> instrument :sound-ready)
    (reduce fire-event-on-instrument
            instrument
            (touch-data->touches data))
    (recur (add-synths-to-instrument instrument) data)))

(defn update-size [instrument canvas-id]
  (let [{w :w h :h :as window-size} (util/get-window-size)]
    (do (util/set-canvas-size! canvas-id window-size)
        (.scrollTo js/window 0 0) ;; Safari leaves window part scrolled down after turn
        (assoc (assoc instrument :h h) :w w))))

(defn create-input-channel
  [canvas-id]
  (merge [(util/listen (dom/getElement canvas-id) :touchstart)
          (util/listen (dom/getElement canvas-id) :touchend)]))

(let [canvas-id "canvas"
      c-instrument (chan)
      c-orientation-change (util/listen js/window :orientation-change)
      c-touch (create-input-channel canvas-id)]

  (go
   (let [draw-ctx (util/get-ctx canvas-id)]
     (util/set-canvas-size! canvas-id (util/get-window-size))
     (loop [instrument nil]
       (let [[data c] (alts! [c-instrument (timeout 30)])]
         (condp = c
           c-instrument (recur data)
           (do (draw-instrument draw-ctx instrument)
               (recur instrument)))))))

  (go
   (loop [instrument (update-size (create-instrument (scales/c-minor)) canvas-id)]
     (>! c-instrument instrument)
     (let [[data c] (alts! [c-touch c-orientation-change])]
       (condp = c
         c-orientation-change (recur (update-size instrument canvas-id))
         c-touch (recur (fire-touch-data-on-instrument instrument data))))))
  )
