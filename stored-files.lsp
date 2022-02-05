(in-package :sc)

;; * stored files
;; ** chords

(defparameter *chords* (make-stored-file-list 'chords '()))

(folder-to-stored-file-list
 *chords*
 "/E/ZKF/Layers/samples/chords/organ/normal"
 :markov-list '((rest 2)
		(organchord-whining-3 0.5)
		(organchord-whining-4 1.5))
 :decay 20)

(folder-to-stored-file-list
 *chords*
 "/E/ZKF/Layers/samples/chords/organ/whining"
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
  '((rest 2)
    (noisy1 1)
    (noisy2 1)
    (noisy3 1)
    (noisy4 1)
    (noisy5 1))
  0
  :loop-flag 1)
 *noisy*)

(store-file-in-list
 (make-stored-file
  'noisy2
  "/rhythmic/noisy/2.wav"
  '((rest 2)
    (noisy1 1)
    (noisy2 1)
    (noisy3 1)
    (noisy4 1)
    (noisy5 1))
  0
  :loop-flag 1)
 *noisy*)

(store-file-in-list
 (make-stored-file
  'noisy3
  "/rhythmic/noisy/3.wav"
  '((rest 2)
    (noisy1 1)
    (noisy2 1)
    (noisy3 1)
    (noisy4 1)
    (noisy5 1))
  0
  :loop-flag 1)
 *noisy*)

(store-file-in-list
 (make-stored-file
  'noisy4
  "/rhythmic/noisy/4.wav"
  '((rest 2)
    (noisy1 1)
    (noisy2 1)
    (noisy3 1)
    (noisy4 1)
    (noisy5 1))
  0
  :loop-flag 1)
 *noisy*)

(store-file-in-list
 (make-stored-file
  'noisy5
  "/rhythmic/noisy/5.wav"
  '((rest 2)
    (noisy1 1)
    (noisy2 1)
    (noisy3 1)
    (noisy4 1)
    (noisy5 1))
  0
  :loop-flag 1)
 *noisy*)


;; ** background

(defparameter *background* (make-stored-file-list 'bg '()))

(store-file-in-list (create-rest) *background*)

(store-file-in-list
 (make-stored-file
  'atmo-bass-conv-organ
  "/background/atmo/bass-conv-organ.wav"
  '((rest 1)
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
  20
  :loop-flag 1)
 *background*)

(store-file-in-list
 (make-stored-file
  'atmo-organ-conv-organ
  "/background/atmo/organ-conv-organ.wav"
  '((atmo-organ-conv-organ 3)
    (atmo-bass-conv-organ 1)
    (atmo-whiningOrgan-long 1)
    (atmo-piano 1)
    (atmo-drone-gnarly 1)
    (digital-asmr 1)
    (phrases-piccolo 1))
  20)
 *background*)

(store-file-in-list
 (make-stored-file
  'atmo-whiningOrgan-long
  "/background/atmo/whiningOrgan-long.wav"
  '((atmo-whiningOrgan-long 2)
    (atmo-whiningOrgan-long1 1)
    (atmo-bass-conv-organ 1)
    (atmo-organ-conv-organ 1)
    (atmo-piano 1)
    (rest 0.5)
    (atmo-drone-gnarly 1)
    (digital-asmr 1)
    (phrases-piccolo 1))
  20
  :loop-flag 1)
 *background*)

(store-file-in-list
 (make-stored-file
  'atmo-whiningOrgan-long1
  "/background/atmo/whiningOrgan-long1.wav"
  '((atmo-whiningOrgan-long 0.5)
    (atmo-bass-conv-organ 1)
    (atmo-organ-conv-organ 1)
    (atmo-piano 1)
    (rest 0.5)
    (atmo-drone-gnarly 1)
    (digital-asmr 1)
    (phrases-piccolo 1))
  20
  :loop-flag 1)
 *background*)

(store-file-in-list
 (make-stored-file
  'atmo-piano
  "/background/atmo/piano.wav"
  '((atmo-piano 4)
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
  20
  :loop-flag 1)
 *background*)

(store-file-in-list
 (make-stored-file
  'atmo-piano1
  "/background/atmo/piano1.wav"
  '((atmo-piano 4)
    (rest 3)
    (atmo-bass-conv-organ 1)
    (atmo-organ-conv-organ 1)
    (atmo-whiningOrgan-long 1)
    (atmo-whiningOrgan-long1 1))
  20
  :loop-flag 1)
 *background*)

(store-file-in-list
 (make-stored-file
  'atmo-piano2
  "/background/atmo/piano2.wav"
  '((atmo-piano1 4)
    (rest 3)
    (atmo-bass-conv-organ 1)
    (atmo-organ-conv-organ 1)
    (atmo-whiningOrgan-long 1)
    (atmo-whiningOrgan-long1 1))
  20
  :loop-flag 1)
 *background*)

(store-file-in-list
 (make-stored-file
  'phrases-piccolo
  "/phrases/piccolo.wav"
  '((atmo-bass-conv-organ 1)
    (atmo-organ-conv-organ 1)
    (atmo-whiningOrgan-long 1)
    (atmo-whiningOrgan-long1 1)
    (atmo-drone-gnarly 1)
    (atmo-piano 1)
    (digital-asmr 1))
  20
  :loop-flag 1)
 *background*)

(store-file-in-list
 (make-stored-file
  'digital-asmr
  "/background/digital/asmr.wav"
  '((rest 2)
    (atmo-bass-conv-organ 1)
    (atmo-organ-conv-organ 1)
    (atmo-whiningOrgan-long 1)
    (atmo-piano 1)
    (atmo-drone-gnarly 1)
    (phrases-piccolo 1))
  20
  :loop-flag 1)
 *background*)

(store-file-in-list
 (make-stored-file
  'atmo-drone-gnarly
  "/background/atmo/drone-gnarly.wav"
  '((atmo-piano 4)
    (rest 3)
    (atmo-bass-conv-organ 1)
    (atmo-organ-conv-organ 1)
    (atmo-whiningOrgan-long 1)
    (atmo-piano 2)
    (digital-asmr 1)
    (phrases-piccolo 1))
  20
  :loop-flag 1)
 *background*)

;; ** bass

(defparameter *bass* (make-stored-file-list 'bass '()))

(store-file-in-list (create-rest) *bass*)

(store-file-in-list
 (make-stored-file
  'bass-1
  "/one-shots/bass/bass-1.wav"
  '((rest 5)
    (bass-1 0.5)
    (bass-2 2)
    (bass-4 2)
    (bass-5 2))
  0.5)
 *bass*)

(store-file-in-list
 (make-stored-file
  'bass-2
  "/one-shots/bass/bass-2.wav"
  '((rest 3)
    (bass-1 1)
    (bass-2 2)
    (bass-3 3))
  0.5)
 *bass*)

(store-file-in-list
 (make-stored-file
  'bass-3
  "/one-shots/bass/bass-3.wav"
  '((rest 4)
    (bass-3 3)
    (bass-5 3)
    (bass-1 3))
  0.5)
 *bass*)

(store-file-in-list
 (make-stored-file
  'bass-4
  "/one-shots/bass/bass-4.wav"
  '((rest 3)
    (bass-1 1)
    (bass-2 2)
    (bass-4 2)
    (bass-5 1))
  0.5)
 *bass*)

(store-file-in-list
 (make-stored-file
  'bass-5
  "/one-shots/bass/bass-5.wav"
  '((rest 3)
    (bass-5 4)
    (bass-4 3)
    (bass-5 4))
  0.5)
 *bass*)

(store-file-in-list
 (make-stored-file
  'bass-6
  "/one-shots/bass/bass-6.wav"
  '((rest 1)
    (bass-1 2)
    (bass-4 1)
    (bass-5 1)
    (bass-7 3))
  0.5)
 *bass*)

(store-file-in-list
 (make-stored-file
  'bass-7
  "/one-shots/bass/bass-7.wav"
  '((rest 3)
    (bass-4 2)
    (bass-1 2)
    (bass-8 4))
  0.5)
 *bass*)

(store-file-in-list
 (make-stored-file
  'bass-8
  "/one-shots/bass/bass-8.wav"
  '((rest 1)
    (bass-5 4)
    (bass-6 3)
    (bass-8 4))
  0.5)
 *bass*)


;; ** breath

(defparameter *breath* (make-stored-file-list 'breath '()))

(folder-to-stored-file-list
 *breath*
 "/E/ZKF/Layers/samples/one-shots/breath/"
 :decay 0.5)

;; ** piano-swells

(defparameter *piano-swells* (make-stored-file-list 'piano-swells '()))

(folder-to-stored-file-list
 *piano-swells*
 "/E/ZKF/Layers/samples/background/atmo/piano-swells/"
 :decay 10
 :start 'random
 :loop-flag 1)

;; ** whispers

(defparameter *whispers* (make-stored-file-list 'whispers '()))

(folder-to-stored-file-list
 *whispers*
 "/E/ZKF/Layers/samples/one-shots/whispers/"
 :decay 1
 :start 'random)

;; ** drumloops

(defparameter *drumloops* (make-stored-file-list 'drumloops '()))

(folder-to-stored-file-list
 *drumloops*
 "/E/ZKF/Layers/samples/phrases/drums/"
 :decay 0.1
 :start 'random
 :loop-flag 1
 :panorama 90
 )

;; ** set alternate-sfls

(set-alternate-sfls *whispers* 0.5 100 *bass* *whispers*) ; noisy was breath
(set-alternate-sfls *bass* 0 0.5 *bass* *whispers*)

(set-alternate-sfls *background* 13 100 *piano-swells* *background*)
(set-alternate-sfls *piano-swells* 0 17 *piano-swells* *background*)
