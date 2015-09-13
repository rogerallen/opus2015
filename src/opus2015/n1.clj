(ns opus2015.n1
  (:use [overtone.music.rhythm :only [metro-bar]]
        [overtone.music.time   :only [apply-by]]
        [overtone.algo.lists   :only [rotate]]
        [opus2015.pitch]
        [opus2015.utils]))

;; ======================================================================
(defn Ω-chan1!
  ([] (:chan1 @Ω))
  ([n] (swap! Ω assoc :chan1 n)))
(defn Ω-chan2!
  ([] (:chan2 @Ω))
  ([n] (swap! Ω assoc :chan2 n)))
(defn Ω-field!
  ([] (:field @Ω))
  ([tonic scale] (swap! Ω assoc :field (scale->field tonic scale))))
(defn Ω-degree!
  ([] (:degree @Ω))
  ([deg] (swap! Ω assoc :degree deg)))
(defn Ω-root
  [] (nth (Ω-field!) (Ω-degree!)))

(Ω-dest!   "Reaper")
(Ω-metro!  92)
(Ω-chan1!  0)
(Ω-chan2!  1)
(Ω-field!  :C :mixolydian)
(Ω-degree! 7r20)

;; ======================================================================
(defn quant1 [n]
  (quantize (Ω-field!) n))
(defn play-note1 [beat dur pitch level]
  (play-midi-note (Ω-chan1!) beat dur pitch level))
(defn play-note2 [beat dur pitch level]
  (play-midi-note (Ω-chan2!) beat dur pitch level))
(defn next-measure-beat []
  (* 4 (metro-bar (Ω-metro!))))
;;(play-note1 ((Ω-metro!)) 1 50 100)

(defn chord-seq-player
  [beat seq-degs seq-durs seq-vels];) ;; <<<
  (let [deg     (first seq-degs)
        dur     (first seq-durs)
        vel     (first seq-vels)
        pitches (deg->chord (+ dur 2) (Ω-field!) deg)
        pitches (map #(+ 12r40 %) pitches)
        ]
    ;;(println beat deg pitches)
    (doall (map #(play-note1 beat dur % vel) pitches))
    (apply-by ((Ω-metro!) (+ beat dur))
              #'chord-seq-player [(+ beat dur) (rotate 1 seq-degs) (rotate 1 seq-durs) (rotate 1 seq-vels)])))

(defn chord-seq-play-once
  "eat thru the seq degs and rotate the durs & vels"
  [beat seq-degs seq-durs seq-vels]
  (let [deg           (first seq-degs)
        next-seq-degs (rest seq-degs)
        dur           (first seq-durs)
        vel           (first seq-vels)
        chord-len     (case dur
                        1 3
                        2 4
                        4 3
                        8 4
                        3)
        pitches       (deg->chord chord-len (Ω-field!) deg)
        pitches       (map #(+ 12r40 %) pitches)]
    ;;(println "play" beat deg pitches)
    (doall (map #(play-note1 beat dur % vel) pitches))
    (when-not (empty? next-seq-degs)
      (apply-by ((Ω-metro!) (+ beat dur))
                #'chord-seq-play-once
                [(+ beat dur) next-seq-degs (rotate 1 seq-durs) (rotate 1 seq-vels)]))))
;;(chord-seq-play-once (next-measure-beat) seqB [1 1 1 1] [60 40])

(defn melody2-play-once
  [beat ps ds vs];)
  (let [pit      (nth (Ω-field!) (first ps))
        nxt-pits (rest ps)
        dur      (first ds)
        vel      (first vs)]
    (play-note2 beat dur pit vel)
    (when-not (empty? nxt-pits)
      (apply-by ((Ω-metro!) (+ beat dur))
                #'melody2-play-once [(+ beat dur) nxt-pits (rotate 1 ds) (rotate 1 vs)]))))
;;(melody2-play-once (next-measure-beat) [7r50 7r51 7r52] [1] [80])

(defn seq-duration
  [seq-degs seq-durs]
  (reduce + (take (count seq-degs) (flatten (repeat seq-durs)))))
;;(chord-seq-duration seqA [1 2 1])

(defn pattern-player
  [play-func beat seq-pats pats];) ;; <<<
  (let [pat-key                      (first seq-pats)
        [seq-degs seq-durs seq-vels] (pat-key pats)
        seq-dur                      (seq-duration seq-degs seq-durs)]
    (println "\n" beat "pat:" pat-key "dur:" seq-dur)
    (play-func beat seq-degs seq-durs seq-vels)
    (apply-by ((Ω-metro!) (+ beat seq-dur))
              #'pattern-player [play-func (+ beat seq-dur) (rotate 1 seq-pats) pats])))

;; ======================================================================
(comment
  ;; got chord seqs via
  ;; (drop-last (random-chord-sequence))

  ;; Reaper has 2 midi tracks live using "dexed" VST synth

  ;; dexed/Dexed_01/30. BANKS, T.
  ;; dexed/User/Sysex/Rhodes/rhodes_2/8.E.PIANO 1
  ;; dexed/User/Sysex/Rhodes/rhodes_1/20.E.PIANO 4
  (pattern-player chord-seq-play-once
                  (next-measure-beat)
                  [:a :a :b :a :b]
                  {:a [[0 2 3 1 2 3] [4 8 8 4 8 4] [40 60 50]]
                   :b [[3 5 4 0]     [4]           [60 40]]
                   })

  ;; dexed/User/Sysex/guitar1/12.LUTE
  (pattern-player melody2-play-once
                  (next-measure-beat)
                  [:a :a :b :a :c]
                  {:a [[7r50 7r52 7r53 7r51 7r52 7r53] [1 2 2 1 2 1] [70 90]]
                   :b [[7r63 7r65 7r64 7r60]           [1]           [70]]
                   :c [[7r63 7r62 7r61 7r63 7r62 7r60] [1]           [90 70]]
                   })

  ;; 2nd pattern
  (pattern-player chord-seq-play-once
                  (next-measure-beat)
                  [:a :a :b :a :b]
                  {:a [[0 1 5 2 3] [8 4 8 4 4] [40 60 50]]
                   :b [[2 3 1 0]   [4]         [60 40]]
                   })
  (pattern-player melody2-play-once
                  (next-measure-beat)
                  [:a :a :b :a :c]
                  {:a [[7r50 7r51 7r55 7r52 7r53] [2 2 1 2 1] [70 90]]
                   :b [[7r62 7r63 7r61 7r60]      [1]         [70]]
                   :c [[7r63 7r62 7r65 7r61 7r60] [1 2 1 2 2] [90 70]]
                   })

  ;; stop
  (defn pattern-player [play-func beat seq-pats pats])
)
