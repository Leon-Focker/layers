(in-package :ly)

;; * stored files
;; ** chords

(defparameter *chords* (make-stored-file-list 'chords '()))

(folder-to-stored-file-list
 *chords*
 "/E/code/layers/samples/chords/organ/normal"
 :markov-list '((rest 2)
		(organchord-whining-3 0.5)
		(organchord-whining-4 1.5))
 :decay 20)

(folder-to-stored-file-list
 *chords*
 "/E/code/layers/samples/chords/organ/whining"
 :markov-list '((rest 2)
		(organchord1 1)
		(organchord4 0.5))
 :decay 20)

;; add a rest
(store-file-in-list (create-rest) *chords*)


;; ** noisy

(defparameter *noisy* (make-stored-file-list 'noisy '()))

(store-file-in-list (create-rest) *noisy*)

(store-file-in-list
 (make-stored-file
  'noisy1
  "/rhythmic/noisy/1.wav"
  :markov '((rest 2)
	    (noisy1 1)
	    (noisy2 1)
	    (noisy3 1)
	    (noisy4 1)
	    (noisy5 1))
  :start 0
  :loop-flag 1)
 *noisy*)

(store-file-in-list
 (make-stored-file
  'noisy2
  "/rhythmic/noisy/2.wav"
  :markov '((rest 2)
    (noisy1 1)
    (noisy2 1)
    (noisy3 1)
    (noisy4 1)
    (noisy5 1))
  :start 0
  :loop-flag 1)
 *noisy*)

(store-file-in-list
 (make-stored-file
  'noisy3
  "/rhythmic/noisy/3.wav"
  :markov '((rest 2)
    (noisy1 1)
    (noisy2 1)
    (noisy3 1)
    (noisy4 1)
    (noisy5 1))
  :start 0
  :loop-flag 1)
 *noisy*)

(store-file-in-list
 (make-stored-file
  'noisy4
  "/rhythmic/noisy/4.wav"
  :markov '((rest 2)
    (noisy1 1)
    (noisy2 1)
    (noisy3 1)
    (noisy4 1)
    (noisy5 1))
  :start 0
  :loop-flag 1)
 *noisy*)

(store-file-in-list
 (make-stored-file
  'noisy5
  "/rhythmic/noisy/5.wav"
  :markov '((rest 2)
    (noisy1 1)
    (noisy2 1)
    (noisy3 1)
    (noisy4 1)
    (noisy5 1))
  :start 0
  :loop-flag 1)
 *noisy*)


;; ** background

(defparameter *background* (make-stored-file-list 'bg '()))

(store-file-in-list (create-rest) *background*)

(store-file-in-list
 (make-stored-file
  'atmo-bass-conv-organ
  "/background/atmo/bass-conv-organ.wav"
  :markov '((rest 1)
    (atmo-bass-conv-organ 1)
    (atmo-organ-conv-organ 1)
    (atmo-whiningOrgan-long 1)
    (atmo-whiningOrgan-long1 1)
    (atmo-piano 2)
    (atmo-piano1 1)
    (atmo-piano2 1)
    (atmo-drone-gnarly 1)
    (digital-asmr 1)
    (phrases-piccolo 1))
  :start 20
  :loop-flag 1)
 *background*)

(store-file-in-list
 (make-stored-file
  'atmo-organ-conv-organ
  "/background/atmo/organ-conv-organ.wav"
  :markov '((atmo-organ-conv-organ 3)
    (atmo-bass-conv-organ 1)
    (atmo-whiningOrgan-long 1)
    (atmo-piano 1)
    (atmo-drone-gnarly 1)
    (digital-asmr 1)
    (phrases-piccolo 1))
  :start 20)
 *background*)

(store-file-in-list
 (make-stored-file
  'atmo-whiningOrgan-long
  "/background/atmo/whiningOrgan-long.wav"
  :markov '((atmo-whiningOrgan-long 2)
    (atmo-whiningOrgan-long1 1)
    (atmo-bass-conv-organ 1)
    (atmo-organ-conv-organ 1)
    (atmo-piano 1)
    (rest 0.5)
    (atmo-drone-gnarly 1)
    (digital-asmr 1)
    (phrases-piccolo 1))
  :start 20
  :loop-flag 1)
 *background*)

(store-file-in-list
 (make-stored-file
  'atmo-whiningOrgan-long1
  "/background/atmo/whiningOrgan-long1.wav"
  :markov '((atmo-whiningOrgan-long 0.5)
    (atmo-bass-conv-organ 1)
    (atmo-organ-conv-organ 1)
    (atmo-piano 1)
    (rest 0.5)
    (atmo-drone-gnarly 1)
    (digital-asmr 1)
    (phrases-piccolo 1))
  :start 20
  :loop-flag 1)
 *background*)

(store-file-in-list
 (make-stored-file
  'atmo-piano
  "/background/atmo/piano.wav"
  :markov '((atmo-piano 4)
    (atmo-piano1 2)
    (atmo-piano2 2)
    (rest 3)
    (atmo-bass-conv-organ 1)
    (atmo-organ-conv-organ 1)
    (atmo-whiningOrgan-long 1)
    (atmo-whiningOrgan-long1 1)
    (atmo-piano 2)
    (atmo-drone-gnarly 1)
    (digital-asmr 1)
    (phrases-piccolo 1))
  :start 20
  :loop-flag 1)
 *background*)

(store-file-in-list
 (make-stored-file
  'atmo-piano1
  "/background/atmo/piano1.wav"
  :markov '((atmo-piano 4)
    (rest 3)
    (atmo-bass-conv-organ 1)
    (atmo-organ-conv-organ 1)
    (atmo-whiningOrgan-long 1)
    (atmo-whiningOrgan-long1 1))
  :start 20
  :loop-flag 1)
 *background*)

(store-file-in-list
 (make-stored-file
  'atmo-piano2
  "/background/atmo/piano2.wav"
  :markov '((atmo-piano1 4)
    (rest 3)
    (atmo-bass-conv-organ 1)
    (atmo-organ-conv-organ 1)
    (atmo-whiningOrgan-long 1)
    (atmo-whiningOrgan-long1 1))
  :start 20
  :loop-flag 1)
 *background*)

(store-file-in-list
 (make-stored-file
  'phrases-piccolo
  "/phrases/piccolo.wav"
  :markov '((atmo-bass-conv-organ 1)
    (atmo-organ-conv-organ 1)
    (atmo-whiningOrgan-long 1)
    (atmo-whiningOrgan-long1 1)
    (atmo-drone-gnarly 1)
    (atmo-piano 1)
    (digital-asmr 1))
  :start 20
  :loop-flag 1)
 *background*)

(store-file-in-list
 (make-stored-file
  'digital-asmr
  "/background/digital/asmr.wav"
  :markov '((rest 2)
    (atmo-bass-conv-organ 1)
    (atmo-organ-conv-organ 1)
    (atmo-whiningOrgan-long 1)
    (atmo-piano 1)
    (atmo-drone-gnarly 1)
    (phrases-piccolo 1))
  :start 20
  :loop-flag 1)
 *background*)

(store-file-in-list
 (make-stored-file
  'atmo-drone-gnarly
  "/background/atmo/drone-gnarly.wav"
  :markov '((atmo-piano 4)
    (rest 3)
    (atmo-bass-conv-organ 1)
    (atmo-organ-conv-organ 1)
    (atmo-whiningOrgan-long 1)
    (atmo-piano 2)
    (digital-asmr 1)
    (phrases-piccolo 1))
  :start 20
  :loop-flag 1)
 *background*)

;; ** bass

(defparameter *bass* (make-stored-file-list 'bass '()))

(store-file-in-list (create-rest) *bass*)

(store-file-in-list
 (make-stored-file
  'bass-1
  "/one-shots/bass/bass-1.wav"
  :markov '((rest 5)
    (bass-1 0.5)
    (bass-2 2)
    (bass-4 2)
    (bass-5 2))
  :start 0.5)
 *bass*)

(store-file-in-list
 (make-stored-file
  'bass-2
  "/one-shots/bass/bass-2.wav"
  :markov '((rest 3)
    (bass-1 1)
    (bass-2 2)
    (bass-3 3))
  :start 0.5)
 *bass*)

(store-file-in-list
 (make-stored-file
  'bass-3
  "/one-shots/bass/bass-3.wav"
  :markov '((rest 4)
    (bass-3 3)
    (bass-5 3)
    (bass-1 3))
  :start 0.5)
 *bass*)

(store-file-in-list
 (make-stored-file
  'bass-4
  "/one-shots/bass/bass-4.wav"
  :markov '((rest 3)
    (bass-1 1)
    (bass-2 2)
    (bass-4 2)
    (bass-5 1))
  :start 0.5)
 *bass*)

(store-file-in-list
 (make-stored-file
  'bass-5
  "/one-shots/bass/bass-5.wav"
  :markov '((rest 3)
    (bass-5 4)
    (bass-4 3)
    (bass-5 4))
  :start 0.5)
 *bass*)

(store-file-in-list
 (make-stored-file
  'bass-6
  "/one-shots/bass/bass-6.wav"
  :markov '((rest 1)
    (bass-1 2)
    (bass-4 1)
    (bass-5 1)
    (bass-7 3))
  :start 0.5)
 *bass*)

(store-file-in-list
 (make-stored-file
  'bass-7
  "/one-shots/bass/bass-7.wav"
  :markov '((rest 3)
    (bass-4 2)
    (bass-1 2)
    (bass-8 4))
  :start 0.5)
 *bass*)

(store-file-in-list
 (make-stored-file
  'bass-8
  "/one-shots/bass/bass-8.wav"
  :markov '((rest 1)
    (bass-5 4)
    (bass-6 3)
    (bass-8 4))
  :start 0.5)
 *bass*)


;; ** breath

(defparameter *breath* (make-stored-file-list 'breath '()))

(folder-to-stored-file-list
 *breath*
 "/E/code/layers/samples/one-shots/breath/"
 :decay 0.5)

;; ** piano-swells

(defparameter *piano-swells* (make-stored-file-list 'piano-swells '()))

(folder-to-stored-file-list
 *piano-swells*
 "/E/code/layers/samples/background/atmo/piano-swells/"
 :decay 10
 :start 'random
 :loop-flag 1)

;; ** whispers

(defparameter *whispers* (make-stored-file-list 'whispers '()))

(folder-to-stored-file-list
 *whispers*
 "/E/code/layers/samples/one-shots/whispers/"
 :decay 1
 :start 'random)

;; ** drumloops

(defparameter *drumloops* (make-stored-file-list 'drumloops '()))

(folder-to-stored-file-list
 *drumloops*
 "/E/code/layers/samples/phrases/drums/"
 :decay 0.1
 :start 'random
 :loop-flag 1
 :panorama 90
 )

;; ** drums

(defparameter *drums* (make-stored-file-list 'drums '()))

;;(store-file-in-list (create-rest) *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'kick1
  "/one-shots/drums/kick1.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0
  :y 0
  :z 0)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'kick2
  "/one-shots/drums/kick2.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0.35
  :y 0
  :z 0)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'kick3
  "/one-shots/drums/kick3.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0.7
  :y 0
  :z 0)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'kick4
  "/one-shots/drums/kick4.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 1
  :y 0
  :z 0)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'snare1
  "/one-shots/drums/snare1.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0
  :y 0.3
  :z 0)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'snare2
  "/one-shots/drums/snare2.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0.35
  :y 0.5
  :z 0)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'snare3
  "/one-shots/drums/snare3.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0.7
  :y 0.7
  :z 0)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'snare4
  "/one-shots/drums/snare4.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 1
  :y 1
  :z 0)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'hat1
  "/one-shots/drums/hat1.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0.2
  :y 0.2
  :z 0.5)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'hat2
  "/one-shots/drums/hat2.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0.4
  :y 0.4
  :z 0.6)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'hat3
  "/one-shots/drums/hat3.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0.7
  :y 0.7
  :z 0.7)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'hat4
  "/one-shots/drums/hat4.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0.8
  :y 0.8
  :z 0.9)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'ride1
  "/one-shots/drums/ride1.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0
  :y 0
  :z 1)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'ride2
  "/one-shots/drums/ride2.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 0
  :y 1
  :z 1)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'ride3
  "/one-shots/drums/ride3.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 1
  :y 0
  :z 1)
 *drums*)

;;#################
(store-file-in-list
 (make-stored-file
  'ride4
  "/one-shots/drums/ride4.wav"
  :decay 0.5
  :loop-flag nil
  :start 0
  :panorama 45
  :preferred-length '(0.3 5)
  :x 1
  :y 1
  :z 1)
 *drums*)

;; ** set alternate-sfls

(set-alternate-sfls *whispers* 0.5 100 *bass* *whispers*) ; noisy was breath
(set-alternate-sfls *bass* 0 0.5 *bass* *whispers*)

(set-alternate-sfls *background* 13 100 *piano-swells* *background*)
(set-alternate-sfls *piano-swells* 0 17 *piano-swells* *background*)
