(ns musicbox.one
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [goog.dom :as dom]
            [goog.events :as events]
            [cljs.core.async :refer [<! put! chan timeout merge sliding-buffer]]
            [musicbox.util :as util]
            [musicbox.scales :as scales]))

(def timbre js/T)

(def colors [{:light "#6DA0CB" :dark "#314B61"}
             {:light "#A76AB9" :dark "#4F2F59"}
             {:light "#BB67A2" :dark "#5E3152"}
             {:light "#C55D83" :dark "#692F44"}
             {:light "#D35E4C" :dark "#692B22"}
             {:light "#E18C43" :dark "#6B401C"}
             {:light "#E1B040" :dark "#69511A"}])

(defn draw-start-screen [draw-ctx]
  (let [{w-window :w h-window :h} (util/get-window-size)
        w-text 200
        h-text 70
        text "Tap the screen to start"
        center-dimension (fn [window text] (- (/ window 2) (/ text 2)))]
    (set! (.-font draw-ctx) "20px Verdana")
    (.fillText draw-ctx
               text
               (center-dimension w-window w-text)
               (center-dimension h-window h-text))))

(defn piano-key-width
  [piano-keys w]
  (.round js/Math (/ w (count piano-keys))))

(defn draw-instrument [draw-ctx instrument]
  (let [{w :w h :h} instrument
        freqs (keys (:piano-keys instrument))
        note-count (count freqs)
        piano-key-w (piano-key-width (:piano-keys instrument) w)]
    (.clearRect draw-ctx 0 0 w h)
    (dotimes [n note-count]
      (let [note (first (:notes (get (:piano-keys instrument) (nth freqs n))))
            note-on (and note (nil? (:off note)))]
        (set! (.-fillStyle draw-ctx)
              ((if note-on :light :dark)
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
  {:piano-keys (apply sorted-map
                 (flatten (map vector
                               scale
                               (map (fn [x] {:synth (create-note-synth x) :notes []})
                                    scale))))
   :w 0
   :h 0})

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
                        piano-key (get (:piano-keys instrument) freq)]
                    (if (empty? (:notes piano-key))
                      (do
                        (.play (.bang (:synth piano-key)))
                        (assoc-in instrument
                                  [:piano-keys freq :notes]
                                  [{:on time :off nil :touch-id touch-id}])))))
   "touchend" (fn
                [{piano-keys :piano-keys :as instrument} {touch-id :touch-id :as event}]
                (if-let [freq (first (filter (fn [x] (= touch-id (:touch-id (first (:notes (get piano-keys x))))))
                                             (keys piano-keys)))]
                    (do
                      (.release (:synth (get piano-keys freq)))
                      (assoc-in instrument [:piano-keys freq :notes] []))))})

(defn fire-event-on-instrument
  [instrument event]
  (let [f-instrument (get instrument-fns (:type event))]
    (or (and f-instrument (f-instrument instrument event))
        instrument)))

(defn update-size [instrument canvas-id]
  (let [{w :w h :h :as window-size} (util/get-window-size)]
    (do (util/set-canvas-size! canvas-id window-size)
        (.scrollTo js/window 0 0) ;; Safari leaves window part scrolled down after turn
        (assoc (assoc instrument :h h) :w w))))

(let [canvas-id "canvas"
      c-app-state (chan)
      c-instrument (chan)
      c-orientation-change (util/listen js/window :orientation-change)
      c-touch-start (util/listen (dom/getElement canvas-id) :touchstart)
      c-touch-end (util/listen (dom/getElement canvas-id) :touchend)]

  (go
   (<! c-touch-start) (<! c-touch-end) ;; start after first touch so don't break sound
   (>! c-app-state "start"))

  (go
   (let [draw-ctx (util/get-ctx canvas-id)]
     (util/set-canvas-size! canvas-id (util/get-window-size))
     (draw-start-screen draw-ctx)
     (loop [instrument (<! c-instrument)]
       (let [[data c] (alts! [c-instrument (timeout 30)])]
         (condp = c
           c-instrument (recur data)
           (do (draw-instrument draw-ctx instrument)
               (recur instrument)))))))

  (go
   (<! c-app-state)
   (loop [instrument (update-size (create-instrument (scales/c-minor)) canvas-id)]
     (>! c-instrument instrument)
     (let [[data c] (alts! [c-touch-start c-touch-end
                            c-orientation-change])]
       (condp = c
         c-orientation-change (recur (update-size instrument canvas-id))
         (recur (reduce fire-event-on-instrument
                        instrument
                        (touch-data->touches data)))))))

  )