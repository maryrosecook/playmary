(ns musicbox.one
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [goog.dom :as dom]
            [goog.events :as events]
            [cljs.core.async :refer [<! put! chan timeout merge sliding-buffer]]
            [musicbox.util :as util]
            [musicbox.scales :as scales]))

(def timbre js/T)

(def colors [{:light "6DA0CB" :dark "314B61"}
             {:light "A76AB9" :dark "4F2F59"}
             {:light "BB67A2" :dark "5E3152"}
             {:light "C55D83" :dark "692F44"}
             {:light "D35E4C" :dark "692B22"}
             {:light "E18C43" :dark "6B401C"}
             {:light "E1B040" :dark "69511A"}])

;; later: use pagehide to cancel in-progress presses when leaving tab

(defn draw [draw-ctx instrument]
  (let [{w :w h :h} instrument
        freqs (keys (:notes instrument))
        note-count (count freqs)
        note-width (/ w note-count)]
    (.clearRect draw-ctx 0 0 w h)
    (dotimes [n note-count]
      (let [note-on (:touch-id (get (:notes instrument) (nth freqs n)))]
        (set! (.-fillStyle draw-ctx)
              ((if note-on :dark :light)
               (nth colors (mod n (count colors))))))
      (.fillRect draw-ctx (* n note-width) 0 note-width h))))

(defn create-synth
  []
  (.play (timbre "OscGen" (js-obj "wave" "fami" "mul" 0.3 "poly" 8))))
  ;; (timbre "*"
  ;;         (timbre "+"
  ;;                 (timbre "sin" freq)
  ;;                 (timbre "sin" (* freq 2) 0.5)
  ;;                 (timbre "sin" (* freq 4) 0.25)
  ;;                 (timbre "sin" (* freq 5) 0.125))
  ;;         (timbre "adsr" "24db" 5 1000 0.0 2500)))

(defn play-note [synth note]
  (.noteOnWithFreq synth (:freq note) 64))


;; (defn synth-play
;;   [synth note]
;;   (util/dirr synth)
;;   (.bang synth))

;; (defn synth-stop
;;   [synth note]
;;   (.keyoff synth (get (.-args synth) 1)))

(defn audio-time []
  (.-currentTime js/audioCtx))

(defn touch-to-notes [touch interface]
  (let [touches (js->clj (.-touches (.-event_ touch)))]
    (map (fn [x]
           {:time (audio-time)
            :freq (scales/quantize (get x "clientX")
                                   0 (:w interface)
                                   (:scale interface))})
         (for [x (range (get touches "length"))] (get touches (str x))))))

(defn update-size [interface canvas-id]
  (let [{w :w h :h :as dimensions} (util/get-window-size)]
    (if (or (not= w (:w interface) (not= h (:h interface))))
      (do (util/set-canvas-size! canvas-id w h)
          (assoc (assoc interface :h h) :w w))
      interface)))

(let [canvas-id "canvas"
      draw-ctx (util/get-ctx canvas-id)
      interface (update-size {:scale (scales/c-minor)} canvas-id)
      c-touch (merge [(util/listen (dom/getElement canvas-id) :touchstart)])]

  (go
   (<! c-touch) ;; do not start until get first touch, otherwise will break sound
   (let [synth (create-synth)]
     (loop [notes []
            interface interface]
       (draw draw-ctx interface notes)
       (let [new-notes (touch-to-notes (<! c-touch) interface)]
         (dorun (map (partial play-note synth) new-notes))
         (recur (concat notes new-notes) (update-size interface canvas-id))))))
  )
