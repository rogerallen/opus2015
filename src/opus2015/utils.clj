(ns opus2015.utils
  (:use [overtone.live]))

(def Ω (atom {}))
(defn Ω-dest!
  ([] (:dest @Ω)) ; get
  ([s] (swap! Ω assoc :dest (midi-find-connected-receiver s))))
(defn Ω-metro!
  ([] (:metro @Ω))
  ([tempo] (swap! Ω assoc :metro (metronome tempo))))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; random number generators to allow for deterministic sequences.
(def ^:dynamic *my-rng* (java.util.Random. (System/currentTimeMillis)))

(defn my-rand-seed [seed]
  (def ^:dynamic *my-rng* (java.util.Random. seed)))

(defn my-rand
  ([] (.nextDouble *my-rng*))
  ([n] (* n (my-rand))))

(defn my-rand-int
  [n] (int (my-rand n)))

(defn my-rand-nth ;; a/k/a "choose"
  [coll]
  (nth coll (my-rand-int (count coll))))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defn quantize
  "given a sorted seq in s, find the item in the seq closest to n.  n
  can be any type that note converts (string, keyword or integer) or a
  floating-point value."
  [s n]
  (let [nt         (if (float? n) n (note n))
        split-seq  (split-with #(<= % nt) s)
        nt-below   (last (first split-seq))
        nt-above   (first (last split-seq))
        ;; handle ends of the sequence
        nt-below   (if (nil? nt-below) (first s) nt-below)
        nt-above   (if (nil? nt-above) (last s) nt-above)
        Δ-nt-below (- nt nt-below)
        Δ-nt-above (- nt-above nt)]
    (if (> Δ-nt-above Δ-nt-below)
      nt-below
      nt-above)))

(defn play-midi-note
  "play a midi note at beat for duration with midi pitch midi level on
  midi channel"
  [channel beat dur pitch level]
  (apply-at ((Ω-metro!) beat)         #'midi-note-on  [(Ω-dest!) pitch level channel])
  (apply-at ((Ω-metro!) (+ beat dur -0.02)) #'midi-note-off [(Ω-dest!) pitch       channel]))

;; http://mugglinworks.com/chordmapsArchive/images/map1.gif
;; http://mugglinworks.com/chordmapsArchive/part3.htm
;;
;; To use the map, remember two things.
;;   First, you may jump anywhere from I.
;;   Second, if a chord appears at more than one place, there is an "imaginary tunnel"
;;     connecting both spots, so you can move from one to the other.
;;
;; reddit discussion
;; http://www.reddit.com/r/WeAreTheMusicMakers/comments/2g3p7a/chord_progression_map_pick_a_key_and_play_a_chord/
;;
(def chord-transition-map-1
  {0 [1 2 3 4 5] ;; start here
   1 [2 4]
   2 [3 5]
   3 [0 1 4]
   4 [0 2 3]
   5 [1 3]
   6 [0] ;; we never go here, but make it so we can go back to 0
   })

;; from http://www.hooktheory.com/trends
;; Just eyeballing hook theory trends in the Key of C
;; for plausible transitions.  Each value corresponds to approximately
;; a 5% probability.
(def chord-transition-map-2
  {0 [1 1 2 2 3 3 3 3 3 4 4 4 4 4 4 4 5 5 5 6] ; C
   1 [0 0 0 2 2 2 3 3 3 3 3 4 4 4 5 5 5 5 5 5] ; dm
   2 [0 1 1 1 1 3 3 3 3 3 3 3 4 4 5 5 5 5 5 5] ; em
   3 [0 0 0 0 0 0 0 1 1 2 2 4 4 4 4 4 4 4 5 5] ; F
   4 [0 0 0 0 0 1 1 2 3 3 3 3 3 5 5 5 5 5 5 6] ; G
   5 [0 0 0 1 1 2 2 3 3 3 3 3 3 4 4 4 4 4 4 6] ; am
   6 [0 0 0 0 0 0 0 1 3 3 4 4 5 5 5 5 5 5 5 5] ; b (used G/B)
   })

(defn random-next-chord
  "given a chord degree, find a reasonable degree to transition to."
  [deg]
  (my-rand-nth (chord-transition-map-2 deg)))
;; (random-next-chord 0)

(defn random-chord-sequence
  "create a random sequence that has a fulfills the start & end degree"
  ([] (random-chord-sequence 0 0))
  ([start-deg end-deg]
  (loop [deg     start-deg
         deg-seq [start-deg]]
    (let [next-deg (random-next-chord deg)
          deg-seq (conj deg-seq next-deg)]
      (if (and (= next-deg end-deg) (some? deg-seq))
        deg-seq
        (recur next-deg deg-seq))))))
