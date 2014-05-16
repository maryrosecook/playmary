(ns playmary.scales)

(def note-freqs {:c3 130.81 :c4 261.6})
(def key' {:major [true false true false true true false true false true false true]
           :minor [true false true true false true false true true false true false]})


(defn chromatic
  [freq n]
  (if (> n 0)
    (cons freq
          (chromatic (* freq (.pow js/Math 2 (/ 1 12))) (dec n)))
    []))

(defn c-minor
  []
  (let [all-notes (chromatic (:c3 note-freqs) 37)
        key-notes (:minor key')]
    (filter identity
            (for [x (range (count all-notes))]
              (if (nth key-notes (mod x 12)) (nth all-notes x))))))
