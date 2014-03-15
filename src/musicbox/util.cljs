(ns musicbox.util
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [goog.dom :as dom]
            [goog.events :as events]
            [cljs.core.async :refer [<! put! chan]]))

(enable-console-print!)

(defn dirr [o]
  (.log js/console o)
  o)

(defn printlnr [o]
  (println o)
  o)


(defn get-window-size []
  { :w (or (.-innerWidth js/window) (.-clientWidth (.-body js/document)))
    :h (or (.-innerHeight js/window) (.-clientHeight (.-body js/document)))})

(defn set-canvas-size! [canvas-id width height]
  (let [canvas (dom/getElement canvas-id)]
    (set! (. canvas -width) width)
    (set! (. canvas -height) height)))

(defn get-ctx [canvas-id]
  (.getContext (dom/getElement canvas-id) "2d"))

(def keyword->event-type
  {:keyup goog.events.EventType.KEYUP
   :keydown goog.events.EventType.KEYDOWN
   :keypress goog.events.EventType.KEYPRESS
   :click goog.events.EventType.CLICK
   :dblclick goog.events.EventType.DBLCLICK
   :mousedown goog.events.EventType.MOUSEDOWN
   :mouseup goog.events.EventType.MOUSEUP
   :mouseover goog.events.EventType.MOUSEOVER
   :mouseout goog.events.EventType.MOUSEOUT
   :mousemove goog.events.EventType.MOUSEMOVE
   :focus goog.events.EventType.FOCUS
   :blur goog.events.EventType.BLUR

   :touchstart goog.events.EventType.TOUCHSTART
   :touchmove goog.events.EventType.TOUCHMOVE
   :touchend goog.events.EventType.TOUCHEND
   :touchcancel goog.events.EventType.TOUCHCANCEL

   :dragstart goog.events.EventType.DRAGSTART
   :drag goog.events.EventType.DRAG
   :dragenter goog.events.EventType.DRAGENTER
   :dragover goog.events.EventType.DRAGOVER
   :dragleave goog.events.EventType.DRAGLEAVE
   :drop goog.events.EventType.DROP
   :dragent goog.events.EventType.DRAGEND

   })

(defn listen
  ([el type] (listen el type nil))
  ([el type f] (listen el type f (chan)))
  ([el type f out]
    (events/listen el (keyword->event-type type)
      (fn [e] (when f (f e)) (put! out e)))
    out))
