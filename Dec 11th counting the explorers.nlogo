;;;;
;;;;
;;;;    code created by
;;;;    joshua becker
;;;;    standing on the shoulders of giants
;;;;
;;;;



;;;;
;;;;  LEARNING POINTS
;;;;
;;;;  extension for this:
;;;;      when is the game over?
;;;;      adding the error factor
;;;;      allowing agents to change more than 1 bit at a time
;;;;      reducing the frequency with which agents copy
;;;;

extensions[array palette]

breed [one-turtles one-turtle]
breed [two-turtles two-turtle]

globals [
  nk-space         ;; an array of length B^K that determines the value of a given allele
  allele-map       ;; an array of length N that maps each digit-location to K other digit-locations
  infinity                             ;; a very large number.
                                         ;; used to denote distance between two turtles which
                                         ;; don't have a connected or unconnected path between them

  b

  global-max
  global-min

  plist            ;; used for
  numpeaks         ;; value distribution
  global-min-solution
  global-max-solution
 time-to-zero
  final-score
  final-solutions
  moment
  minority-start-solution
  majority-start-solution
  paired-list
  numexplorers
]

turtles-own [
  solution                      ;; a turtle's solution to the complex problem.
  distance-from-other-turtles   ;; list of distances of this node from other turtles
                                ;; (just to make sure the small-world rewiring doesn't leave islands
  fitness-value
]

links-own [
  rewired?
  weight
]

to setup
  clear-all
  ask patches [ set pcolor white ]
  set infinity 99999  ;; just an arbitrary choice for a large number
  set b 2

  ;; initialize distribution
  set numpeaks 8

  set plist []
  let i 0
  while [i < numpeaks]
  [
    set plist lput random-float 1 plist
    set i i + 1
  ]



  load-landscape ; Load the landscape
  set paired-list []
  set-solutions


  spawn-turtles
  wire-ringlat
  reset-ticks
 set time-to-zero -1
 set final-score -1
  set moment 1




end

to go
  run-step
  let keepgoing false
  let compare-to [solution] of one-of turtles
  ask turtles [
    ; If anybody's doesn't match, then we keep going.
    if (solution != compare-to) [ set keepgoing true ]
  ]
  if (not keepgoing) [
    set time-to-zero  ticks
    set final-score average-score
    set moment 1
    store-final-solutions
    stop ]

end



;;;  run a step   ;;;

to run-step
  ask turtles [
    ;; Initialize variables
    let best-solution solution
    let better-one? false
    let best-utility expected-utility solution

    ;; Step 1: Check neighbors for better solutions
    ifelse best-local-solution [
      ;; When the switch is ON: Find the best solution among neighbors
      ask link-neighbors [
        let link-weight [weight] of link-with myself
        let neighbor-utility expected-utility [solution] of self
        if (neighbor-utility > best-utility and random-float 1 < link-weight) [
          set best-solution [solution] of self
          set best-utility neighbor-utility
          set better-one? true
        ]
      ]
    ] [
      ;; When the switch is OFF: Stop at the first better solution
      ask link-neighbors [
        let link-weight [weight] of link-with myself
        let neighbor-utility expected-utility [solution] of self
        if (neighbor-utility > expected-utility best-solution and random-float 1 < link-weight) [
          set best-solution [solution] of self
          set better-one? true
          stop  ;; Exit the loop after finding the first better solution
        ]
      ]
    ]

    ;; Step 2: If no better neighbor solution, explore
    if not better-one? [
      let found-solution explore

      if expected-utility found-solution > expected-utility solution [
        set best-solution found-solution
        set better-one? true
      ]
    ]

    ;; Update the agent's solution
    set solution best-solution
    set fitness-value evaluate-fitness solution

    ;; Update visual properties
    let this-color precision evaluate-fitness solution 2
    ifelse enable-space-analysis [
      set color palette:scale-scheme "Sequential" "Reds" 7 this-color 0 1
    ] [
      set label this-color
    ]
  ]
  tick
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                 ;;;
;;; the following set of functions  ;;;
;;; defines the agent behavior.     ;;;
;;;                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to spawn-turtles
  if network-structure = "uniform-spread"
  [spread-uniform]
  if network-structure = "clusters"
  [spread-cluster]

end

to spread-uniform
  create-turtles Number-Of-Agents [

    set label-color black
    setxy random-xcor random-ycor
    ;; Assign breed based on group-proportion
    ifelse random-float 1 < group-proportion [
      set breed one-turtles
      set shape "triangle"
      set color palette:scale-scheme "Sequential" "Blues" 7 0 0 1
      use-standpoint
    ] [
      set breed two-turtles
      set shape "circle"
      set color palette:scale-scheme "Sequential" "Greens" 7 0 0 1
      set-majority-standpoint
    ]
  ]

  layout-circle (sort turtles) max-pxcor - 1
end

to spread-cluster
  let num-one-turtles round (Number-Of-Agents * group-proportion)
  let num-two-turtles Number-Of-Agents - num-one-turtles

  create-one-turtles num-one-turtles [

    set label-color black
    set color palette:scale-scheme "Sequential" "Blues" 7 0 0 1
    set shape "triangle"
    use-standpoint
  ]

  create-two-turtles num-two-turtles [

    set label-color black
    set color palette:scale-scheme "Sequential" "Greens" 7 0 0 1
    set shape "circle"
    set-majority-standpoint

  ]

  layout-circle (sort turtles) max-pxcor - 1
end

to use-standpoint
  if minority-knowledge = "random" [
    set solution n-values n [random b]
  ]
  if minority-knowledge = "consenses" [
    ;; Get the binary string
    set solution minority-start-solution
  ]
end

to set-majority-standpoint
  if majority-knowledge = "random" [
    set solution n-values n [random b]
  ]
  if majority-knowledge = "consenses" [
    ;; Get the binary string
    set solution majority-start-solution
  ]
end




to-report explore
  ;; set the variable 'new answer' to be the turtle's solution variable
  ;; with a randomly chosen item (from 0 to n) replaced with a random value
  ;; from 0 to b that does not include the one already there.
  let replaced-item random n
  let new-answer replace-item replaced-item solution (item random (b - 1) list-not-x (item replaced-item solution) 0 b )
  report new-answer
end




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               ;;;
;;; the following set of methods  ;;;
;;; also known as 'functions'     ;;;
;;; also known as 'to-do-things'  ;;;
;;; defines the nk space.         ;;;
;;;                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                                                                           ;;
  ;; NK space is generated according to Appendix A in                                          ;;
  ;;      Lazer, D., & Friedman, A. (2007). The network structure of exploration and           ;;
  ;;      exploitation. Administrative Science Quarterly, 52(4), 667-694.                      ;;
  ;; There are two key components to this nk-space:                                            ;;
  ;;    a)  the value of a given "allele," which is here represented as a numeric string.      ;;
  ;;        - an agent has N alleles                                                           ;;
  ;;    b)  the pairs of digits that make up each allele.  these are randomly chosen           ;;
  ;;        - each allele has k digits                                                         ;;
  ;;                                                                                           ;;
  ;; The one departure from Lazer's implementation is that here we allow 'b' to be >2, meaning ;;
  ;; that our numeric strings CAN be bitstrings, as his were, but do not have to be.           ;;
  ;;                                                                                           ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




to define-allele-values
  ;; this function defines the value of each
  ;; allele of length k.  together with set-interdependencies
  ;; this defines the nk-space by creating an identifiable
  ;; value for any nk-string
  ;;
  ;; we're not actually directly defining the alleles:  rather,
  ;; we're defining them by their index in a list.  later,
  ;; a converter will take an allele (0 1 1) and convert it
  ;; to a decimal index.
  ;;
  ;; for a given index - allele - the value is randomly chosen
  ;; from a given distribution.

  set nk-space []
  let i 0
  repeat B ^ (K + 1) [
    set nk-space lput nk-distribution nk-space
    set i i + 1
  ]

end

to set-interdependencies
  ;; for each digit-location in the numeric string,
  ;; this function sets (k - 1) other digit-locations
  ;; to define alleles of size k.

  if k > (n - 1)
  [
    show "Error!  K cannot be greater than N-1.  Setting K to N-1."
    set k (n - 1)
  ]

  set allele-map []
  let i 0
  repeat n
  [
    set allele-map lput fput i n-of k (list-not-x i 0 n) allele-map
    set i i + 1
  ]
end



to-report nk-distribution
  ;; this function simply outputs a random number
  ;; from the appropriate distribution for the
  ;; desired nk-space.


  let p item (random numpeaks) plist

  let bin_ct 0
  repeat 1000 [
      if random-float 1 < p
      [
        set bin_ct bin_ct + 1
      ]
  ]
  report bin_ct
end


to-report evaluate-fitness [ test-solution ]
  let fitness 0
  let i 0
  while [i < n]
  [
    let this-allele []
    foreach item i allele-map
    [ [?1] ->
      set this-allele lput item ?1 test-solution this-allele
    ]
    set fitness fitness + item convert-allele this-allele nk-space
    set i i + 1
  ]
  set fitness (fitness / n)

  ; If max-value has been calculated,
  ; normalize the score.

  if (global-max != 0) [
    set fitness fitness / global-max
    set fitness fitness ^ 8
  ]

  report fitness
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               ;;;
;;; some generic, non-model       ;;;
;;; utilities.                    ;;;
;;;                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to-report list-not-x [x x.min x.max]
  ;; a little utility to draw-without-replacement
  ;; from the range x.min to x.max excluding x.
  let i x.min
  let report-list []
  repeat (x.max - x.min)
  [
    if i != x
    [
      set report-list lput i report-list
    ]
    set i i + 1
  ]
  report report-list
end

to-report convert-allele [string]
  ;; this utility converts a base b string into
  ;; a decimal number - the index location of that
  ;; allele's value.
  ;; eg, when b = 2, this is a decimal-to-binary converter.
  let i 0
  let decimal-output 0
  repeat length string
  [
    set decimal-output decimal-output + ( ( b ^ (length string - i - 1) ) * item i string )
    set i i + 1
  ]
  report decimal-output
end

to-report convert-to-allele [ decimal ]
  ;; reverse process of above - so if b=2, this
  ;; is a decimal-to-binary converter.


  let output-string []
  repeat n [
    set output-string fput (decimal mod b) output-string
    set decimal floor (decimal / b)
  ]

  report output-string
end











;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;
;;;
;;; much of this network construction code was
;;; copy-pasted from the uri wilensky small worlds model
;;;
;;; some of it was copy-pasted from previous work of mine
;;;
;;;
;;; remember:
;;;
;;; if you're not copy-pasting,
;;; you're not coding!
;;;
;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;



to wire-ringlat
  layout-circle (sort turtles) max-pxcor - 1

  layout-circle (sort turtles with [ who mod 2 = 0] ) max-pxcor - 4

  ;; iterate over the turtles
  let ni 0
  while [ni < count turtles]
  [
    ;; make edges with the next two neighbors
    ;; this makes a lattice with average degree of 4
    let z 1
    while [z <= floor (degree / 2)]
    [
      make-edge turtle ni turtle ((ni + z) mod count turtles)
      set z z + 1
    ]
    set ni ni + 1
  ]

end

to make-edge [node1 node2]
  ifelse [breed] of node1 = [breed] of node2
  [ask node1 [create-link-with node2 [
    set rewired? false
    set weight 1
    ]]
  ][
    if random-float 1 < disconnect-likelihood [
      ask node1 [create-link-with node2 [
        set rewired? false
        set weight Trust
      ]]
    ]
  ]

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            ;;;
;;;   game state computations  ;;;
;;;                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report number-of-unique-solutions
  let solution-set []
  ask turtles
  [
    let already-there? false
    foreach solution-set
    [ [?1] ->
      if solution = ?1
      [
        set already-there? true
      ]
    ]
    if not already-there?
    [
      set solution-set lput solution solution-set
    ]
  ]
let population count turtles
  report length solution-set / population
end

to-report number-of-unique-solutions-circle
  let solution-set []
  ask two-turtles
  [
    let already-there? false
    foreach solution-set
    [ [?1] ->
      if solution = ?1
      [
        set already-there? true
      ]
    ]
    if not already-there?
    [
      set solution-set lput solution solution-set
    ]
  ]
let two-population count two-turtles
  report length solution-set / two-population
end

to-report number-of-unique-solutions-triangle
  let solution-set []
  ask one-turtles
  [
    let already-there? false
    foreach solution-set
    [ [?1] ->
      if solution = ?1
      [
        set already-there? true
      ]
    ]
    if not already-there?
    [
      set solution-set lput solution solution-set
    ]
  ]
 let one-population count one-turtles
  report length solution-set / one-population
end

to-report average-score
  let avg 0
  ask turtles [
    set avg avg + evaluate-fitness solution
  ]
  report ( avg / count turtles )
end

to-report average-score-triangle
  let avg 0
  ask one-turtles [
    set avg avg + evaluate-fitness solution
  ]
  report ( avg / count one-turtles )
end

to-report average-score-circle
  let avg 0
  ask two-turtles [
    set avg avg + evaluate-fitness solution
  ]
  report ( avg / count two-turtles )
end

to determine-peaks
  if enable-space-analysis [
    ;; WARNING!!!
    ;; this function can easily take a long time
    ;; if n or b are too high...

    ;; determine decimal value of highest numerical-string
    let mylist []
    repeat n
    [
      set mylist lput (b - 1) mylist
    ]

    let best-answer 0
    let worst-answer 999999999
    let best-solution []
    let worst-solution []
    let i 0
    while [i <= convert-allele mylist]
    [
      let this-allele convert-to-allele i
      let this-value evaluate-fitness this-allele
      if this-value > best-answer
      [
        set best-answer this-value
        set best-solution this-allele
      ]
      if this-value < worst-answer
      [
        set worst-answer this-value
        set worst-solution this-allele
      ]
      set i i + 1
    ]

    set global-max best-answer
    set global-min worst-answer
    set global-min-solution worst-solution
    set global-max-solution best-solution
  ]
end

to-report global-peak

  ;; report global max without recalculating
  report global-max
end

to-report global-minimum

  ;; report global min without recalculating

  report global-min
end


;; loading the landscape


to load-landscape
  ifelse file-exists? "landscape.txt" [
    file-close-all
    file-open "landscape.txt"

    ; Load nk-space
    set nk-space []
    let line-nk file-read-line
    while [line-nk != "----"] [
      set nk-space lput (read-from-string line-nk) nk-space
      set line-nk file-read-line
    ]

    ; Load allele-map
    set allele-map []
    let line-allele-map ""
    while [file-at-end? != true] [
      set line-allele-map file-read-line
      set allele-map lput (read-from-string line-allele-map) allele-map
    ]

    file-close
  ] [
    ; If no saved landscape, generate a new one
    define-allele-values
    set-interdependencies
  ]
end

to-report global-minimum-solution
  report global-min-solution
end

;;;;;adding asymmetric conformity condition
to-report count-neighboring-agents-sharing-solution [solution-count]
  ;;Count the number of agents sharing the same solution
  ifelse ingroup-conformity [
  ifelse asymmetric-conformity [
    ifelse breed = one-turtles [
      report count link-neighbors with [solution = solution-count]
    ][
      report count link-neighbors with [solution = solution-count and breed = [breed] of myself]
  ]]

  [report count link-neighbors with [solution = solution-count and breed = [breed] of myself]]
  ]

  [report count link-neighbors with [solution = solution-count]]

end

to-report expected-utility [solution-count]

  let current-fitness evaluate-fitness solution-count
  let NiA count-neighboring-agents-sharing-solution solution-count
  let Ni count link-neighbors
  let conformity-pressure NiA / Ni
  ifelse with-conformity-pressure
    [report (((1 - kappa) * current-fitness) + (kappa * conformity-pressure))]
  [report current-fitness]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to-report final-score-at-zero-solution
  report average-score
end

to-report time-to-zero-solution
  report ticks
end

to store-final-solutions
  set final-solutions []
  ask turtles [
    set final-solutions lput solution final-solutions
  ]
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-solutions
   ; Initialize num-list and binary-list
  let num-list [0.669 0.619 0.567 0.522 0.4858 0.409 0.367 0.332 0.326 0.290]
  let binary-list [
    [0 1 1 1 1 1 1 1 1 0]
    [0 0 0 0 0 1 1 1 0 1]
    [0 1 1 1 1 1 1 1 0 0]
    [0 0 0 0 1 1 1 1 1 1]
    [0 1 0 0 0 0 1 1 1 1]
    [0 1 0 0 0 0 0 1 0 0]
    [1 0 0 0 0 0 1 0 0 0]
    [0 1 0 1 1 1 1 1 1 1]
    [1 1 1 1 1 1 1 1 1 1]
    [1 1 0 0 0 1 0 0 0 0]
  ]

  ; Pair the numbers with their corresponding binary strings
  set paired-list []
 let index 0  ; A manual index to iterate through the lists
  while [index < length num-list] [
    let num-item item index num-list
    let binary-item item index binary-list
    set paired-list lput (list num-item binary-item) paired-list
    set index index + 1
  ]

  set minority-start-solution report-binary-string-minority

  set majority-start-solution report-binary-string-majority

end

to-report report-binary-string-minority

  ;; Find the corresponding binary string from the paired list
  let filtered-item filter [x -> item 0 x = minority-solution] paired-list

 ;; Ensure filtered-item is not empty before proceeding
  ifelse length filtered-item > 0 [
    let corresponding-binary-item item 1 first filtered-item
    report corresponding-binary-item
  ] [
    error "No matching item found in paired-list"
  ]
end



to-report report-binary-string-majority

  ;; Find the corresponding binary string from the paired list
  let filtered-item filter [x -> item 0 x = majority-solution] paired-list

 ;; Ensure filtered-item is not empty before proceeding
  ifelse length filtered-item > 0 [
    let corresponding-binary-item item 1 first filtered-item
    report corresponding-binary-item
  ] [
    error "No matching item found in paired-list"
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
647
448
-1
-1
13.0
1
10
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
42
75
106
108
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
43
128
126
161
One Step
run-step
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
113
74
176
107
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
931
215
1109
248
enable-space-analysis
enable-space-analysis
0
1
-1000

INPUTBOX
695
308
850
368
Number-Of-Agents
40.0
1
0
Number

SLIDER
695
382
867
415
degree
degree
2
50
4.0
2
1
NIL
HORIZONTAL

INPUTBOX
682
87
837
147
N
10.0
1
0
Number

INPUTBOX
686
161
841
221
K
5.0
1
0
Number

TEXTBOX
692
40
842
58
NK Space Variables
11
0.0
1

TEXTBOX
689
228
839
284
The nk-space is NOT randomly generated every time you click 'setup'. It is a fixed space that I import. 
11
0.0
1

PLOT
929
38
1147
207
Average
Time
Score
0.0
20.0
0.0
1.0
true
true
"determine-peaks\nif enable-space-analysis [\nset-plot-y-range 0 1\n]" ""
PENS
"default" 1.0 0 -16777216 true "" "\nplot average-score"
"Triangle" 1.0 0 -13791810 true "" "plot average-score-triangle"
"Circle" 1.0 0 -2674135 true "" "plot average-score-circle"

PLOT
930
281
1148
450
Number of Unique Solutions
Time
Unique Solutions
0.0
20.0
0.0
1.0
true
true
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot number-of-unique-solutions"
"Triangle" 1.0 0 -14454117 true "" "plot number-of-unique-solutions-triangle"
"Circle" 1.0 0 -2674135 true "" "plot number-of-unique-solutions-circle"

SLIDER
696
426
868
459
group-proportion
group-proportion
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
695
471
867
504
Trust
Trust
0
1
0.1
0.01
1
NIL
HORIZONTAL

MONITOR
48
317
117
362
global-min
global-min
17
1
11

MONITOR
49
362
165
407
NIL
global-min-solution
17
1
11

CHOOSER
45
492
212
537
network-structure
network-structure
"uniform-spread" "clusters"
0

MONITOR
49
409
169
454
NIL
global-max-solution
17
1
11

SLIDER
242
493
432
526
disconnect-likelihood
disconnect-likelihood
0
1
1.0
0.01
1
NIL
HORIZONTAL

CHOOSER
474
482
612
527
minority-knowledge
minority-knowledge
"random" "consenses"
1

SWITCH
943
502
1138
535
with-conformity-pressure
with-conformity-pressure
0
1
-1000

SWITCH
944
548
1105
581
ingroup-conformity
ingroup-conformity
1
1
-1000

SWITCH
946
599
1127
632
asymmetric-conformity
asymmetric-conformity
1
1
-1000

CHOOSER
478
553
616
598
majority-knowledge
majority-knowledge
"consenses" "random"
0

MONITOR
52
553
133
598
NIL
time-to-zero
17
1
11

MONITOR
165
556
236
601
NIL
final-score
17
1
11

MONITOR
263
556
428
601
NIL
number-of-unique-solutions
17
1
11

MONITOR
52
605
215
650
final-score-at-zero-solution
final-score-at-zero-solution
17
1
11

SLIDER
689
539
861
572
kappa
kappa
0
1
0.3
0.01
1
NIL
HORIZONTAL

PLOT
648
597
848
747
histogram solutions
solution
frequency
0.0
1.0
0.0
10.0
true
false
"" ""
PENS
"default" 0.01 1 -16777216 true "" "Histogram [fitness-value] of turtles"

MONITOR
41
219
159
264
final-solutions
final-solutions
17
1
11

SWITCH
949
641
1105
674
best-local-solution
best-local-solution
1
1
-1000

CHOOSER
253
621
391
666
minority-solution
minority-solution
0.669 0.619 0.567 0.522 0.485 0.409 0.367 0.332 0.326 0.29
1

CHOOSER
257
680
395
725
majority-solution
majority-solution
0.669 0.619 0.567 0.522 0.485 0.409 0.367 0.332 0.326 0.29
7

MONITOR
57
658
192
703
NIL
minority-start-solution
17
1
11

MONITOR
58
709
194
754
NIL
majority-start-solution
17
1
11

@#$#@#$#@
## WHAT IS IT?

Here I want to add a list so I can choose the score instead of the binary string from the chooser. 


Here instead of the turtle adopting the better solution of any of their neighbors, the turtle adopts the best solution among them.  



the solutions correspond to the scores of 0.66930330, 0.619, 0.5675896080944891, 0.5229077364196485, 0.4858061018622518, 0.4091826960116906, 0.36724452429328247, 0.332726444471566, 0.32672922833847334, 0.219

## NOTICE
when conformity pressure is on, agents won't stop looking. But they won't improve either. Without conformity pressure, they just stop looking. 

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## Change FROM PREVIOUS VERSION

You can compare now whether or not progress means the same thing when we start from a consensus comparing to when we start in an exploration period  ***NEW***

## THINGS TO TRY
See whether you see the kind of difference that Fazelpour observes or not. 

1) set trust = 1 and the proportion = 0.5 and run the model for different levels of connectivity. Then set the trust to lower degree and proportion to varying amount and see whether the difference between high, low, and moderate degrees of connectivity would be any different 
(Note: see what are the connectivity levels that Lazer and Friedman report)
lower dgfree trust confirms what Muldoon talks about. It also confirms the Fazelpour observation about demographic diversity and its benefits for reducing the conformity pressure. 

2) check for variation in connectivity level for groups



## EXTENDING THE MODEL

What I want to do next is to see wgar happens when the following happens: 
1) you let the agents to move around
2) you let the agents to cluster
3) you start from a bad state that everyone has consensus on an what it takes to get out of that. 

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Diversity Project" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>time-to-zero-unique-solutions</metric>
    <metric>average-score-at-zero-unique-solutions</metric>
    <enumeratedValueSet variable="N">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Trust" first="0" step="0.1" last="1"/>
    <enumeratedValueSet variable="Number-Of-Agents">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-space-analysis">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degree">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="38"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="0"/>
      <value value="2"/>
      <value value="5"/>
      <value value="9"/>
    </enumeratedValueSet>
    <steppedValueSet variable="group-proportion" first="0" step="0.1" last="0.5"/>
  </experiment>
  <experiment name="Low trust in one group regardless of the other" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="N">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Trust" first="0" step="0.1" last="1"/>
    <enumeratedValueSet variable="Number-Of-Agents">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-space-analysis">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degree">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
      <value value="8"/>
      <value value="38"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-proportion">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Starting from a bad solution" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="N">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Trust">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-Agents">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-space-analysis">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degree">
      <value value="38"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-proportion">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="consensus or not" repetitions="10000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>time-to-zero-solution</metric>
    <metric>final-score-at-zero-solution</metric>
    <enumeratedValueSet variable="with-conformity-pressure">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-Agents">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-space-analysis">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroup-conformity">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-proportion">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asymmetric-conformity">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disconnect-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Trust">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degree">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;uniform-spread&quot;"/>
      <value value="&quot;clusters&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minority-knowledge">
      <value value="&quot;random&quot;"/>
      <value value="&quot;consenses&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="majority-knowledge">
      <value value="&quot;random&quot;"/>
      <value value="&quot;consenses&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="small consensus or not" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>time-to-zero-solution</metric>
    <metric>final-score-at-zero-solution</metric>
    <enumeratedValueSet variable="with-conformity-pressure">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-Agents">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-space-analysis">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroup-conformity">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-proportion">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asymmetric-conformity">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disconnect-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Trust">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degree">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;uniform-spread&quot;"/>
      <value value="&quot;clusters&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minority-knowledge">
      <value value="&quot;consenses&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="majority-knowledge">
      <value value="&quot;consenses&quot;"/>
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="All Variables" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>time-to-zero-solution</metric>
    <metric>final-score-at-zero-solution</metric>
    <enumeratedValueSet variable="with-conformity-pressure">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="majority-knowledge">
      <value value="&quot;consenses&quot;"/>
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-Agents">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-space-analysis">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroup-conformity">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-proportion">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asymmetric-conformity">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disconnect-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Trust" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="degree">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;clusters&quot;"/>
      <value value="&quot;uniform-spread&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minority-knowledge">
      <value value="&quot;consenses&quot;"/>
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Trust and Proportion with Conformity and Network" repetitions="10000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>time-to-zero-solution</metric>
    <metric>final-score-at-zero-solution</metric>
    <enumeratedValueSet variable="with-conformity-pressure">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="majority-knowledge">
      <value value="&quot;consenses&quot;"/>
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-Agents">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-space-analysis">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroup-conformity">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="group-proportion" first="0.1" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="asymmetric-conformity">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disconnect-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Trust" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="degree">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;uniform-spread&quot;"/>
      <value value="&quot;clusters&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minority-knowledge">
      <value value="&quot;consenses&quot;"/>
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="recreating the weird things about the chart" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>time-to-zero-solution</metric>
    <metric>final-score-at-zero-solution</metric>
    <enumeratedValueSet variable="with-conformity-pressure">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="majority-knowledge">
      <value value="&quot;consenses&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-Agents">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-space-analysis">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroup-conformity">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-proportion">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asymmetric-conformity">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disconnect-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Trust">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degree">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;uniform-spread&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minority-knowledge">
      <value value="&quot;consenses&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="from uniform spread with different values of kappa" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>final-score-at-zero-solution</metric>
    <enumeratedValueSet variable="with-conformity-pressure">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minority-solution">
      <value value="0.619"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="majority-knowledge">
      <value value="&quot;consenses&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-Agents">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enable-space-analysis">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ingroup-conformity">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="best-local-solution">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-proportion">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asymmetric-conformity">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disconnect-likelihood">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Trust">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="majority-solution">
      <value value="0.29"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kappa">
      <value value="0.4"/>
      <value value="0.41"/>
      <value value="0.42"/>
      <value value="0.43"/>
      <value value="0.44"/>
      <value value="0.45"/>
      <value value="0.46"/>
      <value value="0.47"/>
      <value value="0.48"/>
      <value value="0.49"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degree">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-structure">
      <value value="&quot;uniform-spread&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minority-knowledge">
      <value value="&quot;consenses&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
