(ns musicbox.scales)

(def note-freqs {:c4 261.6})
(def key' {:major [true false true true false true false true true false true false]
           :minor [true false true false true true false true false true false true]})


(defn chromatic
  [freq n]
  (if (> n 0)
    (cons freq
          (chromatic (* freq (.pow js/Math 2 (/ 1 12))) (dec n)))
    []))

(defn quantize
  [x start-x end-x scale]
  (if (and (>= x start-x) (< x end-x))
    (let [note-index (.floor js/Math (/ x
                                        (/ (- end-x start-x)
                                            (count scale))))]
      (nth scale note-index))))

(defn c-minor
  []
  (let [all-notes (chromatic (:c4 note-freqs) 25)
        key-notes (:major key')]
    (filter identity
            (for [x (range (count all-notes))]
              (if (nth key-notes (mod x 12)) (nth all-notes x))))))
