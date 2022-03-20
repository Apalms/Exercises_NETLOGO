turtles-own[
  deg ;;degree
  decision? ;; boolean variable to exit the algorithm except the MIS3_bio that we use marked to exit the algorithm
  marked? ;; bolean variable for marking nodes
  max_steps ;; steps of the algorithm
  signal? ;; variable for signaling
  activeProb ;; activation probability
  message? ;; variable for nodes who received message
  broadcast? ;; variable for broadcasting
]


globals [messages
         bits] ;; #messages and #bits


to create-graph
  clear-all

  set-default-shape turtles "circle"

  ask patches [set pcolor white] ;;κάνουμε το patch άσπρο
  crt num-nodes [ ;;δημιουργούμε τους κόμβους
    set color blue ;; tous kanoume mple
    set decision? false ;; thetoume to xaraktiristiko tous decision true
  ]

  if not any? turtles [ stop ]
  ;; an ine to graph-model randmo tote tha exoume tixeo grafima me random diataksi idalios tha exi diataksi kiklou
  ifelse graph-model = "random"
  [ rand-layout ]
  [circle-layout]


  reset-ticks
end

;random layout
to rand-layout
  repeat 50 [
  layout-spring turtles links 0.5 (sqrt count links) 0.5
  ]
  while [(count links < num-links) and (count links <= ((count turtles)*(count turtles - 1)/ 2) - 1)][
    ask one-of turtles
    [ create-link-with one-of other turtles ]
   ]
end

;circle layout
to circle-layout
  layout-circle turtles max-pxcor
  while [(count links < num-links) and (count links <= ((count turtles)*(count turtles - 1)/ 2) - 1)][
    ask one-of turtles
    [ create-link-with one-of other turtles ]
   ]
end



to lubys_MIS
 ask turtles [
    set color blue
    set decision? false
    set label who
    set marked? false
    let nei count link-neighbors
    set deg nei
    set max_steps 0
    set messages 0
    set bits log num-nodes 10
  ]


  while [any? turtles with [not decision?]] [
    let Prob 0
    ask turtles with [not decision?] [
    ;;tsekaroume an iparxoun gitones tote an iparxoun orizoume to deg kai meta tin pithanota me tin opia tha vlepoume
    ;;pioi tha mpoun sto MIS kai sti sinexia oi gitones tous tha aporifthoun

    if(count link-neighbors != 0)[
     set deg ((count link-neighbors) - (count link-neighbors with [color = orange]))
      set Prob (1 / (2 * deg))
      ifelse random-float 1.0 < Prob [
        set marked? true
        if (not any? link-neighbors with [marked?])[ ;; se afto to if sto prwto exchange se kathe loop metrame ta minimata pou stelnoun oi komvoi
           set messages messages + count link-neighbors
        ]
       ]
       [set marked? false]

    ]
    if (count link-neighbors = 0) [set marked? true] ;an o komvos den exi gitones tote mpeni sto MIS
    set max_steps (max_steps + 1) ;;afksanoume ta steps tou algorithmou  + 1
  ]
  ;;rwtame tous komvous pou den exoun apofasisi an tha mpoun sto mis i oxi kai aftous pou ine taftoxrona markarismenoi
  ;;otan oloi oi komvoi kanoun tin metavliti decision true tote teliwni kai o algorithmos
  ask turtles with [not decision? and marked?] [
    let finished 1
    let degneighborList []
    ask link-neighbors [
      ;; dimiourgoume mialista me ta deg twn gitonwn
      set degneighborList lput deg degneighborList
    ]

    ;;gia kathe komvo pou ine ipopsifios gia na mpi sto MIS elexoume to deg tou
    ;;me ta deg twn gitonwn tou kai analogos mpeni sto MIS

      foreach degneighborList [ d ->
        let ctrl 0
        if(deg < d) [
          ask link-neighbors with [deg = d] [
            if (marked?) [set ctrl 1]
          ]
          if (ctrl = 1) [
            set marked? false
            set finished 0
          ]
          ]
        if (deg = d )[
         let id who
         set ctrl 0
         ask link-neighbors with [deg = d] [
          if (id > who) [
            set marked? false
            set ctrl 1
            ]
         ]
         if (ctrl = 0) [
           set marked? false
           ]

        ]
      ]

  ;;se afton ton giro an meta ton elexo pou ginete pio panw ine katalilos gia nampi sto MIS
  ;;tote xromatizete me xroma kokkino kai sti sinexia enimerwni tous gitones tou oti mpike sto MIS gia na min mpoun aftoi.
  ;;Xromatizontas tou prasinous simeni den mpenoun sto MIS
  if (finished = 1 and marked?) [

    set color red
    set decision? true

    ask my-links [
      set color orange
    ]

    ask link-neighbors with [not decision?] [
      set color green
      set decision? true
      set deg 0
      ask my-links [
        set color orange
      ]
    ]
  ]

  ]
  tick
  ]
  set bits bits * messages ;;metrame ta sinolika bits twn minimatwn
end

to MIS2_bio
  ask turtles [
    set color blue
    set decision? false
    set label who
    set marked? false
    set message? false
    set broadcast? false
    set max_steps 0
    set messages 0
    set bits 1
  ]
  let max_deg max[count link-neighbors] of turtles ;; orizoume tin metavliti max_deg i opoia apothikevi to megisto degree tou grafimatos metrontas tous link-neighbors

  while [any? turtles with [not decision?]] [
    let i 0 ;; thetoume tin metavliti i isi me 0
    let Prob 0 ;; thetoume tin pithanotita isi me 0

    while [i <= log max_deg 10][
      let j 0 ;; thetoume tin metavliti j ise me 0
      while [j <= log num-nodes 10][
       ;;rwtame tous komvous pou den exoun apofasisei akomi me mia pithanotita na markaristoun (marked=true)
       ;;kai epipleon kanoume tin metavliti braodcast true gia na diksoume oti o komvos kani broadcast
       ask turtles with [not decision?][
          let n (max_deg - i)
          set Prob (1 / (2 ^ log n 10 )) ;; ipologizoume tin pithanotita me tin opia o komvos markarete kai kani broadcast
          if random-float 1.0 <= Prob
            [set marked? true
             set broadcast? true
               if (not any? link-neighbors with [ marked?])[ ;;se afto to if sto prwto exchange se kathe loop metrame ta minimata pou stelnoun oi komvoi
                  set messages messages + count link-neighbors
          ]]
           ;;an kapios gitonas tou komvou ine markarismenos kai stelni minima kai komvos lamvani to minima
           ;;tote ton ksimarkaroume kai kanoume tin metavliti message true
           if any? link-neighbors with [ marked? and broadcast? ] [
              set marked?  false
              set message? true
          ]

          set max_steps (max_steps + 1) ;;afksanoume ta vimata tou algorithmou kata 1
       ]
       ;; sto 2o exchange rwtamw tous komvous pou akoma den exoun apofasisi, pou ine markarismenoi kai den exoun lavi minina
       ;; na kanoun tin metavliti broadcast true na diksoun oti kanoun broadcast na ksana ipologistoun ta minimata pou steloun oti mpenoun sto MIS
       ;; na xrvmatistoun kokkini epidi mpenoun sto MIS, na kanoun tin metavliti decision true dixnontas oti apofasisan kai na xrwmatisoun tis akmes tou portokalies
       ask turtles with [ not decision? and  marked? and not message? ][
          set broadcast? true
          if (not any? link-neighbors with [ marked?])[
               set messages messages + count link-neighbors
              ]
          set color red
          set decision? true

          ask my-links [set color orange]
          ;;enimerwni tous gitones tou oti mpike sto MIS gia na min mpoun aftoi.
          ;;Xromatizontas tou prasinous simeni den mpenoun sto MIS, kai kani ti metavliti decision true
          ask link-neighbors with [not decision?][
            set color green
            set decision? true

            ask my-links [set color orange] ;;rwtoun tis akmes tous gia na xrvmatistoun portokalies afou apofasisan
          ]
          if any? turtles with [ message?];; an kapios komvos exi lavi minima tote ksimarkarete kai apofasizi
            [
              set marked? false
             set decision? true]
         ]
         set j j + 1 ;; afksanoume ti metavliti j kata 1
         tick
      ]
     set i i + 1 ;;afkanoume ti metavliti i kata 1
    ]
 ]
 set bits bits * messages ;;metrame ta sinolika bits twn minimatwn
end

to MIS3_bio
   ask turtles [
    set color blue
    set label who
    set marked? true ;;all nodes are active
    set signal? false
    set max_steps 0
    set messages 0
    set activeProb 0.5 ;; thetoume os xaraktiristiko twn komvwn pithanotita energopioisis isi me 0.5
    set bits 1
  ]
;; kanoume ena while opou oso kapios komvos ine energos
    while [any? turtles with [ marked?]][
        ;;rwtame tous komvous pou ine energoi an me random-float 1 mikrotero i iso tis pithanotitas energopioisis
        ;;na thesoun tin metavliti signal pou dixni oti enas komvos kani signaling, true
        ;; kai meta rwtame an den iparxi kapios gitonas pou kani signaling tote na metrisoume ta minimata pou estile o komvos stous gitones tou
        ask turtles with [marked?][
          ifelse random-float 1.0 <= activeProb
            [ set signal? true
              if (not any? link-neighbors with [signal?])[
                 set messages messages + count link-neighbors
               ]
             ][set signal? false]
        ;;rotame an kapios gitonas kani signaling (signal? = true), tote na stamatisi na kani signal (signal? = flase)
        ;;kai na gini i pithanotita energopioisi dia 2
        ;;alios thetoume sti metavliti a ti diplasia pithanotita kia sti metavliti b tin pithanotita 1/2
        ;;meta ta vazoume sti lista probList me ti xrisi tou lput kai sto telos thetoume sto activeProb tin elaxisti timi apo tin probList
        ifelse any? link-neighbors with [ signal?]
           [set signal? false
            set activeProb activeProb / 2  ]
           [let a 2 * activeProb
            let b 0.5
            let probList []
            set probList lput a probList
            set probList lput b probList
            set activeProb min probList      ]

           set max_steps (max_steps + 1) ;; afksanoume ta vimata tou algorithmou kata 1
         ]
      ;;Sto 2o exchange rotame tous komvous pou ine enrgoi (marked?=true) kai pou kanoun signaling (signal? = true)
      ;; na xromatistoun kokkinoi kai na apenergopioithoun (afto simeni oti mpenoun sto MIS)
      ;;meta rvta tous gintones tou pou ine energoi na xromatistoun prasini gt mpike aftos sto MIS, na apenergopioithoun (marked? = flase)
      ;;kai na xromatistoun oi akmes tous portokalies
         ask turtles with [ marked? and signal? ][

              set color red
              set marked? false

              ask link-neighbors with [ marked?][
                set color green
                set marked? false
                ask my-links [
                   set color orange
                 ]
               ]
            ;; edo opos lei kai o algorithmos kanoume ena if pou rotame an kapio gitonas kani signaling (signal? = true)
            ;; na xromatistoun prasioi kai na apenergopioithoun
            if any? link-neighbors with [   signal? ]
                 [set color green
                  set marked? false]
            ]
            tick
          ]
        set bits bits * messages ;;ipologizoume to plithos twn bits

end

to mis_sel13
  ask turtles [
    set color blue
    set decision? false
    set label who
    set marked? false
    set max_steps 0
  ]

  let D max [count link-neighbors] of turtles ;; thetoume stin metavliti D to megisto arithmo gitonon. Etsi ipologizoume to max degree
  let prob 1 / D ;; thetoume os pithanotita eklogis enos komvou gia na mpi sto MIS 1/D
  ;;kanoume while me sinithiki oso iparxoun komvoi pou den exoun apofasisoi
  while [any? turtles with [not decision?]][
    ;;rwtame tou komvous pou den exoun apofsisi akomi na ginoun ipopsifioi gia na mpoun sto MIS me pithantita 1/D
    ask turtles with [not decision?][
     ifelse random-float 1.0 < prob [
        set marked? true]
       [set marked? false]
      ;; an kapios gitonas exi eklexthi (marked = true) tote ginete (marked = false) dld den tha ine pia ipopsifios gia to MIS
      if any? link-neighbors with [marked?]
        [set marked? false]

      set max_steps (max_steps + 1);; afksanoume ta vimata tou algorithmou kata 1
    ]
    ;; rotame tou komvous pou den exoun apofasisoi kai ine ipopsifioi na xromatistoun kokkinoi (ara mpenoun sto MIS)
    ask turtles with [not decision? and marked?][
     set decision? true
     set color red
     ask my-links [set color orange]
     ask link-neighbors with [not decision?][ ;;edo oloi oi gitones tou xromatizontai prasinoi (ara den mpenoun sto MIS)
       set color green
       set decision? true
       ask my-links [set color orange]
      ]
    ]
    tick
  ]
end

to mis_sel8
  ask turtles [
    set color blue
    set decision? false
    set label who
    set max_steps 0
  ]
  ;;kanoume ena while me sinthiki, oso kapios komvos den exi apofasis
  while[any? turtles with[not decision? ]][
    ask turtles with [not decision?][
      let id who ;;anathetoume sti metavliti id to label tou kathe komvou
      let neiList [who] of link-neighbors ;;dimiourgoume mia lista me ta ids ton gitonwn
      show neiList
      if (length neiList > 0) [ ;;an to mikos tis listas ine megalitero tou 0
        let maxId max neiList ;;thetoume sti metavliti maxId tin megisti timi apo ti lista neiList
        ifelse (id > maxId) ;; an to id tou komvou ine megalitero apo to megisto id pou exi kapios gitonas
        [set decision? true ;; tote apofasise
         set color red ;; xromatistou kokkinos (ara mpenis sto MIS)
         ask my-links[set color orange] ;;xromatise tis akmes tou portokalies
         ask link-neighbors with [not decision?][ ;;rota tous gitones
           set color green ;;na xromatistoun prasinoi (ara den mpenoun sto MIS )
           set decision? true ;; apofasie
           ask my-links [set color orange];;xromatise tis akmes tous portokalies
          ]
        ]
        [if (id < maxId)[ ;;an to id tou komvou ine mikrotero apo to megisto id pou exi kapios gitonas
          set decision? true ;; tote apofasise
          set color green ;; xromatistou prasinos (ara den mpenis sto MIS)
          ask my-links [set color orange] ;; xromatise tis akmes tou portokalies
          ]
         ]
        if (length neiList = 0)[ ;;an to mikos tis listas ine iso me 0
         set decision? true ;; tote apofasise
         set color red ;; gine kokkinos (ara mpenis sto MIS)
        ]
      ]
      set max_steps (max_steps + 1) ;; afksanis ta vimata tou algorithmou kata 1

    ]
    tick
  ]

end
@#$#@#$#@
GRAPHICS-WINDOW
372
10
894
533
-1
-1
15.6
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

SLIDER
106
162
278
195
num-nodes
num-nodes
0
1000
13.0
1
1
NIL
HORIZONTAL

SLIDER
104
226
276
259
num-links
num-links
0
1500
19.0
1
1
NIL
HORIZONTAL

CHOOSER
123
65
261
110
graph-model
graph-model
"circle" "random"
1

BUTTON
952
68
1056
101
create-graph
create-graph
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
987
138
1050
171
go1
lubys_MIS
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
980
412
1053
457
max_steps
max [max_steps] of turtles
17
1
11

MONITOR
980
539
1054
584
joined_MIS
count turtles with [color = red]
17
1
11

MONITOR
965
478
1065
523
not_joined_MIS
count turtles with [color = green]
17
1
11

BUTTON
993
194
1056
227
go2
MIS2_bio
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
987
610
1049
655
max-deg
max[count link-neighbors] of turtles
17
1
11

BUTTON
994
252
1057
285
go3
MIS3_bio
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
978
673
1055
718
#messages
max [messages] of turtles
17
1
11

BUTTON
980
311
1073
344
goBonus13
mis_sel13
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
986
365
1072
398
goBonus8
mis_sel8
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
981
750
1038
795
#bits
max [bits] of turtles
17
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

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
NetLogo 6.2.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>create-graph</setup>
    <go>create-graph
MIS2_bio</go>
    <timeLimit steps="150"/>
    <metric>count turtles with  [color = red]</metric>
    <metric>max [max_steps] of turtles</metric>
    <enumeratedValueSet variable="graph-model">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-links">
      <value value="60"/>
      <value value="180"/>
      <value value="420"/>
      <value value="900"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>create-graph</setup>
    <go>create-graph
MIS2_bio</go>
    <timeLimit steps="150"/>
    <metric>count turtles with [color = red]</metric>
    <metric>max [messages] of turtles</metric>
    <enumeratedValueSet variable="graph-model">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-links">
      <value value="60"/>
      <value value="180"/>
      <value value="420"/>
      <value value="900"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>create-graph</setup>
    <go>create-graph
MIS2_bio</go>
    <timeLimit steps="150"/>
    <metric>max [bits] of turtles</metric>
    <enumeratedValueSet variable="graph-model">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-links">
      <value value="60"/>
      <value value="180"/>
      <value value="420"/>
      <value value="900"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>create-graph</setup>
    <go>create-graph
lubys_MIS</go>
    <timeLimit steps="150"/>
    <metric>max [max_steps] of turtles</metric>
    <metric>max [messages] of turtles</metric>
    <metric>max [bits] of  turtles</metric>
    <enumeratedValueSet variable="graph-model">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-links">
      <value value="60"/>
      <value value="180"/>
      <value value="420"/>
      <value value="900"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>create-graph</setup>
    <go>create-graph
MIS3_bio</go>
    <timeLimit steps="150"/>
    <metric>max [max_steps] of turtles</metric>
    <metric>max [messages] of turtles</metric>
    <metric>max [bits] of turtles</metric>
    <enumeratedValueSet variable="graph-model">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-links">
      <value value="60"/>
      <value value="180"/>
      <value value="420"/>
      <value value="900"/>
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
