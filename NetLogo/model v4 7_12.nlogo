extensions [table]
globals[  ;;these are global values - so they exist beyond patches/agents; they need to be set up here to assign values later

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;global values for seagrass;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;sg-density ;seagrass density (shoots/m^2)
  Nc.ag    ;critical nutrient threshold for ag-growth (both growth-1 and -2)
  Nc.bg    ;critical nutrient threshold for bg-growth
  d1.ag    ;rate coefficient for loss of biomass to detrital pool (slough proportion)
  d1.bg    ;
  d_D      ;rate of decomposition of detritus to release nutrients into available pool
  dD


  ;;Nutrient uptake parameters;;
  k1       ;1/2 saturation for ag-growth-1     ;;this is based on the lower limit of k1 and r1 values from Lee & Dunton 1999
  Vmax1     ;max growth rate for ag-growth-1
  k2       ;1/2 sat. for ag-growth-2
  Vmax2    ;max growth rate for ag-growth-2
  k.bg     ;1/2 saturation for bg-growth       ;;this is based on the median values of k1 and r1 values for roots from Lee & Dunton 1999
  Vmax.bg     ;max growth rate for bg-growth


  gamma-ag      ;fixed fraction of nutrient in ab-biomass based on control blades
  gamma-bg      ;fixed fraction of nutrient in bg-biomass

  ag-max-1      ;maximum above ground biomass under standard growth
  ag-max-2      ;maximum above ground biomass under fertilized growth
  ag-min        ;minimum above ground biomass
  bg-max        ;maximum below ground biomass
  bg-min        ;minimum below ground biomass

  ;bg.nut.threshold

;;;;;;;;;;;;;;;;;;;;;
;;;general globals;;;
;;;;;;;;;;;;;;;;;;;;;
  ;nutrient-diffusion-rate     ;proportion of nutrient that is diffused into neighbor cells

  t                  ; time as represented by ticks
  umol>g.N           ; multiplies by (18.039g/mol * 1mol/10^6umol) to convert from umol to g
  g>umol.N           ; multiplies by (10^6umol/mol * 1mol/18.039g) to convert from g to umol
  water.temp         ; water temp in degrees C

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;fish bioenergetics globals;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  f.Tc                       ;temperature function for consumption
  N.blade                    ;% wet weight of N in seagrass blades
  N.invert                   ;% wet weight of N in inverts
  invert.assimilation        ;assimilation rate of N from detrital pool to invert biomass

  ;;fish length-weight relationship where Weight = a * Length ^ b
  a.grunt
  b.grunt

 ;;growth parameters
 l-inf.grunt
 K.grunt

]

patches-own[ ;;these are patch-specific variables
  reef?               ; TRUE if the patch is a reef
  seagrass?           ; TRUE if the patch is seagrass
  ag-biomass          ; X above ground (g biomass)
  bg-biomass          ; X below ground (g biomass)
  detrital-pool       ; D pool for water column (gN detritus)
  wc-nutrients        ; N pool for ag-growth (g nutrient per patch; 1 patch = 1m x 1m x 10m = 10 m^3 of water = 10,000 L)
  blade.slough.amount ; blade biomass sloughed in each timestep
  bg.slough.amount    ; bg biomass sloughed in each timestep
  accel.slough.amount ; blade biomass sloughed during accelerated growth

  reef-visibility     ; allows for reef-attraction

  reef-proximity      ; this allows for summary statistics to be done on near, mid, and far distance seagrass patches


   accel.uptake.amount

  ;;testers;;
  dN/dt
  nut.available
  exceed
  death-detritus

  detritus.old
  ag-diff
  bg-diff

  dN.ag/dt
  dN.bg/dt
  dN.accel/dt

]

turtles-own [ ;;these are fish specific variables
  aen                  ; species specific N assimilation efficiency
  Nbody                ; species specific N of body
  weight               ; weight of fish (g)
  body-length-current  ;current length of fish (cm)
  body-length-old      ;length of fish in previous timestep [t-1] (cm)
  age                  ; age of fish (min)
  Cmax                 ; Max consumption depending on length
  Activity             ; Activity level depending on movement
  travel-distance      ; distance travelled at time-step to determine activity
  resp                 ; respiration at time step
  prop.cons            ; proportion of C.max actually consumed
  consumption-required ; actual consmption in g(fish) required for growth
  nutrient-excretion   ; output of bioenergetics of nutrients excreted to environment
  death-prob           ; probability of 'death' or reset of body size

  reserves-max         ; this is our theoretical 'doggy bag' - I've dubbed it 'reserves' as a place where the fish could get the nutrients necessary to not have negative growth.
  reserves             ; if these reserves go to 0, then the fish 'dies', resetting the biomass
  reserves-new

  Cn.desired
  Cn.real

  reset-count
  reserve-reset-count

  death-nutrient-contribution

  growth-amount
  growth-amount-weight
  growth-nutrient
  egestion-nutrient
  new-size
  body-length-start      ;;this is the initial size of the turtle

  reserves.old

]

breed [ blue-fishes blue-fish ]   ;;creates blue-fish species
breed [ red-fishes red-fish ]     ;;creates red-fish species


to go    ;instructions for running model
  ;ask patches[ recolor ]
  ask patches [
    recolor
    if seagrass? = TRUE[ ;;this is necessary or else the seagrass procedures will try to run on the reef patches, causing model to crash
      seagrass-procedures
      dead-fish-detritus

      set detritus.old detrital-pool
  ]]
  ask turtles[
    move-red
    move-blue
    ;consumption-setup ;;Cmax and F(t)_c no longer necessary
    activity.go
    respiration
    fish-grow-setup
    excretion-consumption-growth
    aging
    ;l-w-convert
    death-rebirth

  ]

nutrient-diffusion

tick
end

to setup    ;model setup
  clear-all
  set-default-shape turtles "fish"
  parameter-setup
  patch-setup
  fish-setup
  reset-ticks
end

to time
  set t ticks * minutes-per-tick
end

to patch-setup
  if model-version = "reef" [  ;if model version is "reef", there is a reef and attraction parameters set
    ask patches [
      seagrass-setup
      reef-setup
    set reef-visibility 100 - distancexy 0 0    ;;this sets up term reef-visibility term such that max (100) is at (0,0) and decreases by the distance from this point
  ]]
  if model-version = "no reef" [  ;if model version is "no reef", there is no reef, no attraction parameters, and there is just seagrass
    ask patches[
      seagrass-setup
      set reef-visibility 0
  ]]
  ask patches[
  if distancexy 0 0 <= 10 [
    set reef-proximity "near"]

  if 10 < distancexy 0 0 and distancexy 0 0 <= 20 [
    set reef-proximity "mid"]

  if 20 < distancexy 0 0 [
    set reef-proximity "far"]
  ]
end

to seagrass-setup   ;;this sets the initial seagrass community
  ifelse model-version = "reef" [
    set seagrass?  1.2 <= (distancexy 0 0)]
  [set seagrass? 0 <= (distancexy 0 0 )]

  if seagrass?
  [set pcolor 64]
  if seagrass?[
    ;; these starting ag and bg-biomass conditions are based on average values from 15m seagrasses from Layman et al. 2016 (data: allbiodata)
    set ag-biomass starting-ag-biomass  * sg-density ;;(biomass/shoot * shoots/m^2)
    set bg-biomass (starting-bg-biomass + 0.0396) / 0.0941  ; this is set per m^2 based on Layman 2016 data
  ]

;; start with equivalent levels of biomass in both detrital pools
;   set wc-detritus (random-normal 0.459 0.1275) * sg-density
;   set bg-detritus (random-normal 37 5) * sg-density

;; start detrital levels in each pool as some fraction of the initial biomass pool size
  set detrital-pool (ag-biomass * gamma-ag + bg-biomass * gamma-bg) * detrital-fraction

;; start with moderate pools to ensure that there are sufficient nutrients for preliminary growth; roughly estimated for realistic values based on Lee & Dunton 2000
  set wc-nutrients (starting-wc-nutrient)  ;; (X g nutrient / patch)

end

to reef-setup
  set reef? (distancexy 0 0) < 1.2
  if reef? [set pcolor grey]
end

to fish-setup
  create-turtles population-1
  [
    set color blue
    set age 0
    setxy random-xcor random-ycor
     let m starting-mean-size ;mean
     let v 10  ;variance
     let u ln(m ^ 2 / sqrt ( v + m ^ 2))         ;this generates a lognormal where m = 30 and v = 3
     let o sqrt(ln( 1 + (v / (m ^ 2))))
     let ns random-normal u o
    set body-length-start (exp ns)
       if body-length-start = 0 [
       set body-length-start 20]
    set body-length-current body-length-start
    set weight a.grunt * (body-length-current ^ b.grunt)   ; this uses the basic relations of W(g) = a * L(cm) ^b where a and b are constants from fish base
    set aen  0.75
    set Nbody 2.999
    set size (2 * body-length-current / 40)

   set reserves-max  Nbody / 100 * weight * 0.05     ;
   set reserves Nbody / 100 * weight * 0.05   ;
  ]

;  create-turtles population-2
;  [set size (2 * body-length-current / 25)
;   set color red
;   set age 0
;   setxy random-xcor random-ycor
;   set body-length-current (random 23)
;      if body-length-current = 0 [
;      set body-length-current 1]
;   set weight 0.015 * (body-length-current ^ 3.059)
;   set aen 0.75
;   set Nbody 2.999
;
;   set reserves-max Nbody / 100 * weight * 0.05      ;
;   set reserves Nbody / 100 * weight * 0.01  ;
;  ]
end



to parameter-setup
;; Layman et al 2016 had densities ranging from 1000 ~ 3300  ; Lee and Dunton find average in two Gulf of Mexico densities 1700 ~ 2000 shoots/m^2
;set sg-density random-normal 200 5            ;shoots/m^2

  ;conversion terms
  set umol>g.N  18.039 / (10 ^ 6)
  set g>umol.n  (10 ^ 6) / 18.039

;; all of the below k and Vmax are based on values from Lee and Dunton (1999)
set k1 12.6                                      ;Km for above ground growth-1      //  {uM}
set Vmax1 8.1 * (1 / 60) * (Minutes-per-tick)    ;Vmax for ag growth-1              //  {umol/g(dry)/tick}
set k2 26                                        ;Km for ag growth-2                //
set Vmax2 14.5 * (1 / 60) * (Minutes-per-tick)   ;Vmax for ag growth-2
set k.bg 178.1                                   ;Km for bg growth
set Vmax.bg 9.8 * (1 / 60 * (Minutes-per-tick)) ;Vmax for bg growth

;;Below values set based on max and minimum wet-weight equivalents from Layman 2016 paper dataset (data: seagrass production_above-below ground_C-N-P)
set ag-max-1 0.76 * sg-density    ;;(0.24 +0.0213)/0.3436 = 0.76
  ;set ag-max-2 3.41 * sg-density    ;;(1.15 +0.0213)/0.3436 = 3.41
set ag-min 0.15 * sg-density      ;;(0.00231 +0.0213)/0.3436 = 0.068707;set by min biomass/shoot
set bg-max (450 + 0.039) / 0.0941   ;this is set by approximate near reef root + rhizome biomass from layman 2016
  ;set bg-max 8.69 * sg-density     ;;(0.778 + 0.0396)/0.0941 = 8.69      ;set by max biomass/shoot of root+rhizome dry weight converted to wet weight
set bg-min 1.149 * sg-density     ;;(0.068 + 0.0396)/0.0941 = 1.149    ;set by min  "     / "

;;Below values set by mean blade N% and belowground N% based on 15m seagrass from Layman et al. 2016 paper (data: complete blade nutrient data)
set gamma-ag 0.00484 ;;1.44%N dry weight --> 0.0144 / ((1 + 0.0213)/0.3436) = 0.00484 or 0.484 %N wet weight
set gamma-bg 0.000724   ;;0.8%N dry weight --> 0.008 / ((1 + 0.0396)/0.0941) = 0.000724 or 0.0724 %N wet weight


  ;;these are arbitrary best estimates

if Nc-thresholds = true [
    set Nc.ag 0.07 * g>umol.n * (1 / 10000)               ;critical N threshold ag-growth    //  {uM}   0.7ug/L * (10^-6 ug / 1g) * (1mol/18.039g)
    set Nc.bg 0.07 * g>umol.n * (1 / 10000) ]; ag-max-1 * gamma-ag * g>umol.n * (1 / 10000) ; 7 * (10 ^ -6) / 18.039                 ;critical N threshold bg-growth    //  {uM}  //  arb.
set d1.ag percent-growth-sloughed-ag * (1 / 100) ;this now represents the proportion of dX/dt that is equivalent to slough rate. If dX/dt is small, slough should also be small  * (1 / 60) * minutes-per-tick             ;slough-rate for ag-biomass        // {% biomass / tick} // arb
set d1.bg percent-growth-sloughed-bg * (1 / 100) ;* (1 / 60) * minutes-per-tick             ;root shedding rate                // ""  // ""
set d_D decomp-rate * (1 / (365 * 24 * 60 * 100 )) *  minutes-per-tick                 ;decomposition rate of detritus    // ""  // ""
set dD percent-slough-to-detritus * (1 / 100)

;set nutrient-diffusion-rate 0.75 ;proportion of nutrient that is diffused into neighbor cells

set N.invert 0.01788 ;from Jake's bioenergetics model

set water.temp 26 ;(degrees C)


set a.grunt 0.0121
set b.grunt 3.161

 ;;growth parameters
set l-inf.grunt 41.6    ;;(cm)
set K.grunt 0.2         ;;(1/year)

end


to nutrient-diffusion
  diffuse wc-nutrients nutrient-diffusion-rate
  diffuse death-detritus death-detritus-diffusion
  diffuse detrital-pool detrital-diffusion
end

   ;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;seagrass equations;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;

to seagrass-procedures
;;parameter set-up;;
  let X.ag.dry 0.3436 * ag-biomass - 0.0213 ;; this converts the agbiomass to the equivalent dry mass for M-M equation where Vmax is in units of g(dry)
  let X.bg.dry 0.0941 * bg-biomass - 0.0396
  let N wc-nutrients * g>umol.n * (1 / 10000)  ;;this converts gN/patch to umol/L or uM
  let X.ag.wet ag-biomass
  let X.bg.wet bg-biomass
  let D detrital-pool

;;;;;;;;;;;;;;;;;;initial growth funtions;;;;;;;;;;;;;;;;;;

  set ag-diff ag-biomass - ag-max-1
  if ag-diff > 20 [set ag-diff 20]
  if ag-diff < 20 [set ag-diff -20]


  let blade.uptake.amount (1 / (1 + e ^ (sigmoid-slope.ag1 * (ag-diff)))) * (Vmax1 * (N - Nc.ag) / (k1 + (N - Nc.ag))) * X.ag.dry    ;; uptake amount (umol/tick)
  ifelse (blade.uptake.amount > N * 10000) [set exceed 1][set exceed 0] ;;;test
  if (blade.uptake.amount > N * 10000) [ set blade.uptake.amount (N * 10000 - 0.001)] ;; if the uptake amount exceeds the available nutrients in the water column, set uptake amount equal to - N to deplete the local nutrients to 0


  ;;take the nutrient uptake amount (umol/tick) and converts to biomass.wet/tick growth
  let dX.ag/dt blade.uptake.amount * umol>g.N * (gamma-ag ^ -1)

  ;;set blade slough rate as a function of growth rate. if the nutrient uptake biomass is less than zero, slough amount goes to 0 for that timestep
  ifelse (dX.ag/dt > 0) [
   set blade.slough.amount (base-slough-rate * ag-biomass) + dX.ag/dt * d1.ag] ;;(gBiomass/tick)  ;;the end part of this used to also multiply by conversion umol>g.N and gamma-ag^-1 but this was duplicated bc the conversion already happens above
    ;set blade.slough.amount (base-slough-rate * ag-biomass) + dX.ag/dt * umol>g.N * (gamma-ag ^ -1) * d1.ag] ;;(gBiomass/tick)
  [set blade.slough.amount base-slough-rate * ag-biomass ]

  ;;now combine the above two to find the biomass rate based on growth abd slough
  let dX.ag1/dt dX.ag/dt - blade.slough.amount                                          ;;(gBiomass.wet/tick)

  ;;Jake's new method for detritus: fraction dD of growth goes into detrital pool as nutrients.
  ;;The only output for detrital pool is a TINY amount of decomposition, but will mostly be facilitated by fish
  let dD.ag/dt (dD) * blade.slough.amount * gamma-ag - d_D * D

  ;;Jake's new method for nutrients: and the remaining fraction (1 - dD) goes straight into available nutrient pool.
  set dN.ag/dt (- 1) * blade.uptake.amount * umol>g.N + (1 - dD) * blade.slough.amount * gamma-ag


  ;;take the slough amount to calculate input to detrital pool and subtract the detritus decomposed to nutrient pool to find dD/dt
  ;let dD.ag/dt blade.slough.amount * gamma-ag - (d_D * D) ;;(gN/tick)

  ;;take -1 * uptake amount to find change in nutrients and add the input from detrital pool to calculate dN/dt;
  ;let dN.ag/dt -1 * blade.uptake.amount * umol>g.N + d_D * D  ;;(gN/tick)



;;;;;;;;;;;;;;;;;;belowground functions;;;;;;;;;;;;;;;;;;

  let bg.uptake.amount (1 / (1 + e ^ ( -1 * sigmoid-slope.bg * (N - bg.nut.threshold) ) ) ) * (Vmax.bg * (N - Nc.bg) / (k.bg + (Nc.bg))) * X.bg.dry   ;;uptake amount by belowground (umol/tick)
  if (bg.uptake.amount > N * 10000)[ set bg.uptake.amount (N * 10000 - 0.001)]     ;;same logic as above, if the upake amount exceeds available water column nutrients, then set uptake to -N

 ;;take belowground nutrient uptake, and convert to biomass
  let dX.bg/dt bg.uptake.amount * umol>g.N * (gamma-bg ^ -1)

  ;;set root slough rate as a function of belowground growth. If nutrient uptake biomass is less than zero, slough goes to 0 for that timestep.
  ifelse (dX.bg/dt > 0) [
    set bg.slough.amount base-slough-rate-bg * bg-biomass + dX.bg/dt * d1.bg]
   [set bg.slough.amount base-slough-rate-bg * bg-biomass]

  ;;now combite the above two to find the rate of biomass growth and slough
  let dX.bg1/dt dX.bg/dt - bg.slough.amount

  ;;new slough
  let dD.bg/dt (dD) * bg.slough.amount * gamma-bg - (d_D * D)

  ;;new nutrients
  set dN.bg/dt (-1) * bg.uptake.amount * umol>g.N + (1 - dD) * bg.slough.amount * gamma-bg


  ;;slough used to find input to detrital pool minus detritus decomp
  ;let dD.bg/dt bg.slough.amount * gamma-bg - (d_D * D)                                 ;;can't remember if I did something different here in the old code

  ;;-1 * uptake amount is dN/dt
  ;let dN.bg/dt -1 * bg.uptake.amount * umol>g.N  + d_D * D

;;;;;;;;;;;;;;;;;;accelerated growth functions;;;;;;;;;;;;;;;;;;
 set bg-diff bg-biomass - bg-max
  if bg-diff > 20 [set bg-diff 20]
  if bg-diff < 20 [set bg-diff -20]


  let accel.uptake.amount.desired (1 /(1 + e ^ ((-1) * sigmoid-slope.ag2 * bg-diff))) * (Vmax2 * (N - Nc.ag) / (k2 + (N - Nc.ag))) * X.ag.dry    ;; uptake amount (umol/tick)
  ifelse (accel.uptake.amount.desired > N * 10000 ) [ set accel.uptake.amount (N * 10000 - 0.00001)][set accel.uptake.amount accel.uptake.amount.desired ];; if the uptake amount exceeds the available nutrients in the water column, set uptake amount equal to - N to deplete the local nutrients to 0.

  ;;take the nutrient uptake amount (umol/tick) and converts to biomass.wet/tick growth
  let dX.accel/dt accel.uptake.amount * umol>g.N * (gamma-ag ^ -1)

  ;;set blade slough rate as a function of growth rate. if the nutrient uptake biomass is less than zero, slough amount goes to 0 for that timestep
  ifelse (dX.accel/dt > 0) [
   set accel.slough.amount base-slough-rate * ag-biomass + dX.accel/dt * d1.ag] ;;(gBiomass/tick)
  [set accel.slough.amount base-slough-rate * ag-biomass ]

  ;;now combine the above two to find the biomass rate based on growth and slough
  let dX.accel1/dt dX.accel/dt - accel.slough.amount                                          ;;(gBiomass.wet/tick)

  ;detritus
  let dD.accel/dt (dD) * accel.slough.amount * gamma-ag - (d_D * D)

  ;nutrients
  set dN.accel/dt (- 1) * accel.uptake.amount * umol>g.N + (1 - dD) * accel.slough.amount * gamma-ag

  ;;take the slough amount to calculate input to detrital pool and subtract the detritus decomposed to nutrient pool to find dD/dt
  ;let dD.accel/dt accel.slough.amount * gamma-ag - (d_D * D) ;;(gN/tick)

  ;;take -1 * uptake amount to find change in nutrients and add the input from detrital pool to calculate dN/dt;
  ;let dN.accel/dt -1 * accel.uptake.amount * umol>g.N + d_D * D  ;;(gN/tick)


;;;;;;;;;;;;;;; below actually changes values of seagrass, nutrient, and detrital pools
  set ag-biomass ag-biomass + dX.ag1/dt + dX.accel1/dt
  set bg-biomass bg-biomass + dX.bg1/dt
  set detrital-pool detrital-pool + dD.ag/dt + dD.bg/dt + dD.accel/dt
  set wc-nutrients wc-nutrients + dN.ag/dt + dN.bg/dt + dN.accel/dt






  ;;;;;;;;;;;;;;;;;the below nested ifelses set up the threshold system for the system;;;;;;;;;;;;;;;;;;
;  ifelse ag-biomass <= ag-max-1 [     ;;if ag-biomass is less than ag-max-1, then we will run control growth
;    set ag-biomass ag-biomass + dX.ag1/dt
;    set detrital-pool detrital-pool + dD.ag/dt
;    set wc-nutrients wc-nutrients + dN.ag/dt]
;
;  [ifelse bg-biomass <= bg-max [    ;;if ag-biomass surpasses ag-max, but the bg-biomass is less than bg-max, then run belowground growth
;
;      set bg-biomass bg-biomass + dX.bg1/dt
;      set detrital-pool detrital-pool + dD.bg/dt
;      set wc-nutrients wc-nutrients + dN.bg/dt
;         ][
;
;        set ag-biomass ag-biomass + dX.accel1/dt   ;;if both ag-max-1 and bg-max have been surpassed, then run accel growth
;        set detrital-pool detrital-pool + dD.accel/dt
;        set wc-nutrients wc-nutrients + dN.accel/dt
;            ]]


end


;to biomass-growth
;let X.ag 0.3436 * ag-biomass - 0.0213 ;; this converts the agbiomass to the equivalent dry mass for M-M equation where Vmax is in units of g(dry)
;let X.bg 0.0941 * bg-biomass - 0.0396
;let N wc-nutrients * (1 / 18.04) * (10 ^ 6) * (1 / 10000) ; (gN/patch * 18.04g/mol * 10^6 umol/mol * 1 patch/10,000L)
;
;let dX.ag/dt (Vmax1 * (N - Nc.ag) / (k1 + (N - Nc.ag))) * X.ag    ;;(uM/tick)
;  if (dX.ag/dt) > N [ set dX.ag/dt -1 * (N - 0.001)]
;
;let slough.rate dX.ag/dt * (18.039 / (10 ^ 6)) * (gamma-ag ^ -1) * 0.8
;
;let dX.ag1/dt dX.ag/dt * (18.039 / (10 ^ 6)) * (gamma-ag ^ -1) - slough.rate    ;;(gBio(wet)/tick)
;
;;let dX.ag1/dt (Vmax1 * (N - Nc.ag) / (k1 + (N - Nc.ag))) * X.ag * (18.039 / (10 ^ 6)) * (gamma-ag ^ -1)  - d1.ag * ag-biomass   ;;(umol/tick * gbio)*(g_bio/g_N)*(18g/1mol)*(1mol/10^6umol)
;;let dX.bg/dt (Vmax.bg * (N - Nc.bg) / (k.bg + (N - Nc.bg))) * X.bg * (18.039 / (10 ^ 6)) * (gamma-bg ^ -1) - d1.bg * bg-biomass
;;let dX.ag2/dt  (Vmax2 * (N - Nc.ag) / (k2 + (N - Nc.ag))) * X.ag * (18.039 / (10 ^ 6))* (gamma-ag ^ -1) - d1.ag * ag-biomass
;
; set ag-biomass ag-biomass + dX.ag1/dt
;
;  ;if N > Nc.ag [
;   ; ifelse ag-biomass <= ag-max-1 [set ag-biomass ag-biomass + dX.ag1/dt][ifelse bg-biomass <= bg-max [set bg-biomass bg-biomass + dX.bg/dt][set ag-biomass ag-biomass + dX.ag2/dt]]]
;end
;
;to detrital-flux
;  let X.ag ag-biomass
;  let X.bg bg-biomass
;  let D detrital-pool
;  let dD.ag/dt (d1.ag * X.ag * gamma-ag) - (d_D * D)
;  let dD.bg/dt (d1.bg * X.bg * gamma-bg) - (d_D * D)
;
;  set detrital-pool detrital-pool + dD.ag/dt
;  ;ifelse ag-biomass <= ag-max-1 [set detrital-pool detrital-pool + dD.ag/dt]
;  ;[ifelse bg-biomass <= bg-max [set detrital-pool detrital-pool + dD.bg/dt] [set detrital-pool detrital-pool + dD.ag/dt]]
;end
;
;
;to nutrient-flux ;;nutrient flux during aboveground growth-1
;let X.ag 0.3436 * ag-biomass - 0.0213               ;converts to dry equivalent for M-M equation
;let X.bg 0.0941 * bg-biomass - 0.0396    ; "" ""
;let D detrital-pool
;let N wc-nutrients * (18.04 / 1 ) * (10 ^ 6) * (1 / 10000) ; wc-nutrients (gN/patch * 18.04mol/g * 10^6 umol/mol * 1 patch/10,000L)
;
;set dN/dt ((-1) * ((Vmax1 * N) / (k1 + N)) * X.ag)  ;; just uptake by seagrass (uM/tick)
;  if (-1 * dN/dt) > N [ set dN/dt -1 * (N - 0.001)]
;
;;set dN1/dt ((-1) * ((Vmax1 * N) / (k1 + N)) * X.ag * (18.039 / (10 ^ 6))) + d_D * D ;[M-M](umol) * (18.039g/mol) (1mol/10^6umol)
;;let dN.bg/dt ((-1) * ((Vmax.bg * N) / (k.bg + N)) * X.bg * (18.039 / (10 ^ 6))) + d_D * D ;;this gives nitrogen flux in grams: (umol/tick * g_bio) * (g_bio) * (gN/mol) * (1mol/10^6 umol)
;;let dN2/dt ((-1) * ((Vmax2 * N) / (k2 + N)) * X.bg * (18.039 / (10 ^ 6))) + d_D * D
;
;;; if water column nutrients are at 0, there cannot be watercolumn drawdown by biomass, but there still can be input by detritus
;let dN.decomp/dt d_D * D ;this represents nutrient change only by decomposition of detritus, and not nutrient uptake by seagrass
;
;set dN/dt dN/dt * ((1 / 18.04) * (10 ^ -6)) + dN.decomp/dt
;
;set wc-nutrients wc-nutrients + dN/dt
;
;;set wc-nutrients wc-nutrients + dN1/dt
;;  ifelse N > Nc.ag [
;;  ifelse ag-biomass <= ag-max-1 [set wc-nutrients wc-nutrients + dN1/dt]
;;    [ifelse bg-biomass <= bg-max [set wc-nutrients wc-nutrients + dN.bg/dt] [set wc-nutrients wc-nutrients + dN2/dt]]]
;;  ;;;;if water column nutrients is essentially 0, no more nutrient uptake from plants, only nutrient input by decomp of detrital pool
;;  [ifelse ag-biomass <= ag-max-1 [set wc-nutrients wc-nutrients + dN/dt]
;;    [ifelse bg-biomass <= bg-max [set wc-nutrients wc-nutrients + dN/dt] [set wc-nutrients wc-nutrients + dN/dt]]]
;end

  ;;;;;;;;;;;;;;;;;;;
  ;;fish procedures;;
  ;;;;;;;;;;;;;;;;;;;

to reef-attraction ;;look for reef and head in that direction
 let reef-ahead reef-visibility-at-angle 0
 let reef-right reef-visibility-at-angle 45
 let reef-left reef-visibility-at-angle -45
 if (reef-right > reef-ahead) or (reef-left > reef-ahead)
[ifelse reef-right > reef-left
    [rt 45]
    [lt 45] ]

end

to-report reef-visibility-at-angle [angle]
  let p patch-right-and-ahead angle 1
  if p = nobody [report 0 ]
  report [reef-visibility] of p
end

to bounce
  if abs[pxcor] of patch-ahead travel-distance = max-pxcor
      [set heading (- heading)]
  if abs[pycor] of patch-ahead travel-distance = max-pycor
      [set heading (180 - heading)]
end

;move red fish
to move-red
    if color = red [
    reef-attraction
    set travel-distance random-normal mean-travel-red 1
;    if open? = TRUE [
;        if abs[pxcor] of patch-ahead travel-distance = max-pxcor
;          [set heading (- heading)]
;        if abs[pycor] of patch-ahead travel-distance = max-pycor
;          [set heading (180 - heading)]
;    ]
    fd travel-distance
    ]
  rt random 360
end



;move blue fish
to move-blue
  if color = blue [
    reef-attraction

     let m mean-travel-blue ;mean
     let v 5  ;variance
     let u ln(m ^ 2 / sqrt ( v + m ^ 2))
     let o sqrt(ln( 1 + (v / (m ^ 2))))
     let td random-normal u o
     set travel-distance exp(td)

    ;set travel-distance random-normal mean-travel-blue 1
    fd travel-distance
  ]
  rt random 25
end

to-report reef-at-angle [angle]
  let p patch-right-and-ahead angle 1
  if p = nobody [ report 0 ]
  report [reef-visibility] of p
end


to aging
  set age age + Minutes-per-tick * (1 / 60) * (1 / 24)  ;unit in days (min * hr/min * day/hr)
end

to l-w-convert
l-w-conversion-grunt
l-w-conversion-squirrelfish
end

;; length to weight conversion for grunts
to l-w-conversion-grunt
  if color = blue[
  set weight a.grunt * (body-length-current ^ b.grunt)

    set reserves-max 0.05 * weight * (Nbody / 100)              ;5% of body nutrients represents the max doggy bag size
  ]

end

to l-w-conversion-squirrelfish
  if color = red[
   set weight 0.015 * (body-length-current ^ 3.059)

    set reserves-max 0.05 * weight * Nbody / 100
  ]
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bioenergetics model ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
to activity.go
   let max-travel mean-travel-blue + 1 ; 1 is the standard dev set up in movement parameters
  if color = blue[
      set Activity ( 1 / max-travel ) * travel-distance + 1
  ]
;   let max-travel mean-travel-red + 1 ; 1 is the standard deviation set up in the movement parameters
;  if color = red [
;    set Activity ( 1 / max-travel ) * travel-distance + 1
;  ]
end

to respiration
  let RA 0.0108 * (1 / 24) * (1 / 60 ) * Minutes-per-tick ;(g/g/day --> tick)  ;; unclear whether 0.035 is appropriate for this. May need to be lower???
  let RB -0.2
  let RQ 2.1
  let RTO 36
  let RTM 40

  ;;for f(T) temperature dependence function for respiration
  let Vr  (RTM - water.temp)/(RTM - RTO)
  let Zr  ln RQ *(RTM - RTO)
  let Yr  ln RQ *(RTM - RTO + 2)
  let Xr  (Zr ^ 2 * (1 + (1 + 40 / Yr) ^ 0.5 ) ^ 2) / 400
  ;this is the f(t) equation 2 ()
  let f.Tr  Vr ^ Xr * exp(Xr * (1 - Vr))

  ;Respiration (mass specific g/g/day --> /tick)
  set resp (RA * weight ^ (RB) * f.Tr * Activity )    ; (gO2/gfish)
  set resp resp * 13560 * (1 / 4800) ;;  (oxycaloric coeff: 13560 J/gO2 consumed) * (energy-density of fish: 1/(J/g(wet weight))) -->  (1/tick)
end

to consumption-setup  ;;sets Cmax and f(T) for consumption
  let C_A 0.25 * (1 / 24) * (1 / 60 ) * Minutes-per-tick ;(/day -->tick )
  let CQ 2.3
  let CB -0.3                    ; should this be negative?
  let CTO 29
  let CTM 39
  set Cmax C_A * (weight ^ CB)   ; (g(prey) / grams (biomass)/ tick)

  ;for f(T) temperature dependence function for consumption

  let V (CTM - water.temp)/(CTM - CTO) ;26 deg C is the average water temperature in the Bahamas
  let Z ln (CQ) * ( CTM - CTO)
  let Y  ln (CQ)*(CTM - CTO + 2)
  let X (Z ^ 2 * (1 + (1 + 40 / Y ) ^ 0.5 ) ^ 2 ) / 400
  set f.Tc V ^ X * exp(X * (1 - V))
end

to fish-grow-setup  ;;note that growth is in length - in order to make weight work, it depends on having updated weights
      if color = blue[
    set growth-amount K.grunt * (1 / 365) * (1 / 24) * (1 / 60 ) * Minutes-per-tick  * (l-inf.grunt - body-length-current)   ;;this represents the amount a fish would like to grow given VB growth in cm
;    set body-length-old body-length-current
;    set body-length-current body-length-old + growth-amount

    set growth-amount-weight a.grunt * ((body-length-current + growth-amount) ^ (b.grunt) - (body-length-current) ^ (b.grunt))  ;;g Biomass ;; this is how much it would grow in grams bodyweight

    ;find p (proportion of maximum consumption) at this size
    ;set prop.cons ( ( (growth-amount-weight) + Resp * weight ) / 0.55 ) / ( Cmax * weight * f.Tc)  ;;gFish/tick <--Numerator
    ;set consumption-rate Cmax * prop.cons * f.Tc  ;;(g of prey / g of body mass / tick)            ;;Cmax and f.Tc cancel and left with gFish/(tick * gWeight)  --> (1/tick)

    set consumption-required (growth-amount-weight + resp * weight ) / ( 0.55) ;; grams of consumption required for given growth amount dictated by VB (in units g(wet weight))

    ;set weight a.grunt * (body-length-current ^ b.grunt)
    ;set reserves-max 0.05 * weight * (Nbody / 100)              ;5% of body nutrients represents the max doggy bag size

    set size (2 * body-length-current / 40)         ;sets size of the fish proportional to body size in the simulation display
  ]
;     if color = red[
;   let K 0.94           ;;values from fishbase for Holocentrus rufus
;   let l-inf 23.5
;   let growth-amount K * (1 / 365) * (1 / 24) * (1 / 60 ) * Minutes-per-tick  * (l-inf - body-length-current)
;   set body-length-old body-length-current
;   set body-length-current body-length-old + growth-amount
;
;   set prop.cons ( ( (growth-amount) + Resp * Weight ) / 0.55 ) / ( Cmax * Weight * f.Tc)
;   set consumption-rate Cmax * prop.cons * f.Tc  ;; (g of prey / g of body mass / tick)
;   set size (2 * body-length-current / 18)
;  ]
end


to excretion-consumption-growth

  set reserves.old reserves

  set Cn.desired consumption-required * (Nbody / 100) ;((1/tick) * (gFish) * Nfish       ;; commented out code is WRONG bc calculation of Nassimilation to body tisse to account for growth, not how much prety needs to be consumed: g.invert/g.body/tick) * (g.body) * (N/g.invert(wetweight)) = N/tick
  let FAn 1 - aen                                       ; aen = 0.75 and is the N assimilation efficiency
  ;let egestion-nutrient FAn * Cn.desired
;  set growth-nutrient (a.grunt * (body-length-current ^ (b.grunt) - body-length-old ^ (b.grunt)) * (Nbody / 100) )
;  set nutrient-excretion Cn - egestion-nutrient - growth-nutrient
;  set wc-nutrients wc-nutrients + nutrient-excretion


;; Consumption and doggy-bag/reserves procedures:

;;Here, detrital pool is sufficient to feed the fish's desired growth
  ifelse detrital-pool >= Cn.desired [                                                    ;; if detrital-pool is greater than or equal to nutrients desired for growth in a given timestep (Cn.desired)
    set Cn.real Cn.desired                                                       ;; then Cn.real = Cn.desired

    set growth-nutrient (growth-amount-weight * (Nbody / 100) )                       ;; this is the nutrient that is allocated to growth
    set body-length-current body-length-current + growth-amount                       ;; now update body-length to new length
    set weight weight + growth-amount-weight     ;a.grunt * (body-length-current ^ b.grunt)                              ;; and the new weight
    set reserves-max 0.05 * weight * (Nbody / 100)                                    ;; and the new max-reserves based on this weight

    set detrital-pool detrital-pool - Cn.real                                         ;; it takes up what is needed for ALL of its processes

    let remaining-nutrient (detrital-pool)                                            ;; the remaining amount of nutrients in the detrital pool after the fish has consumed its required nutrients

    if reserves < reserves-max and remaining-nutrient > (reserves-max - reserves) [   ;; if the fish's reserves are not maxed out, and the remaining nutrient in the patch exceeds what it will require to fill reserves
  ifelse remaining-nutrient >= (reserves-max - reserves) [
    set detrital-pool detrital-pool - (reserves-max - reserves)                        ;; the detrital pool will decrease by the amount that was needed to fill to max
      set reserves reserves-max                                                        ;; then the fish will have reserves set at max
   ]
  ;if reserves < reserves-max and remaining-nutrient < (reserves-max - reserves)[      ;; if reserves are below max, but remaining nutrient is not sufficient to totally fill to max
   [   set reserves reserves + remaining-nutrient                                      ;; the reserves will be increased by the amount of remaining nutrient
        set detrital-pool detrital-pool - remaining-nutrient] ]                           ;; and the detrital pool will decrease by the remaining nutrient (which should go to 0)
  ]

;;ELSE: Here, the detrital pool is less than what is desired by the fish to grow  {in the condition where detrital-pool < Cn.desired}
  [
    ifelse (Cn.desired) <= (reserves + detrital-pool)                                  ;;If the nutrient desired is LESS than what the fish has in its reserves + the detrital pool, then it survives
    [set Cn.real Cn.desired                                                            ;;because between detritus and reserves, there is sufficient nutrient, its Cn.real is set at Cn.desired

      set growth-nutrient (growth-amount-weight * (Nbody / 100) )
      set body-length-current body-length-current + growth-amount
      set weight a.grunt * (body-length-current ^ b.grunt)
      set reserves-max 0.05 * weight * (Nbody / 100)
      set reserves reserves - (Cn.real - detrital-pool)
      set detrital-pool 0                                             ;;it comsumes everything it can from the detrital pool, setting it to 0
                                                 ;;and it gets the reaminder (Cn.real - D) from its reserves.
       ]
    [                                                                                 ;;if Cn.desired is GREATER than what the fish has in reserves + detrital pool, then it will die
;     let m 20 ;mean
;     let v 10  ;variance
;     let u ln(m ^ 2 / sqrt ( v + m ^ 2))
;     let o sqrt(ln( 1 + (v / (m ^ 2))))
;     let ns random-normal u o

;     ifelse body-length-current < 20 [
;        set new-size body-length-current][
;        set new-size 20 ] ;exp(ns)                                                        ;;then a new fish of size pulled from log-normal distribution with given mean and stdev


      set new-size body-length-start                                                   ;; the new fish is generated at its original size
      set growth-nutrient 0
      set growth-amount 0
      set growth-amount-weight 0
    ;; when a fish 'dies' and is replaced, then the difference in weight between the two is converted to the nutrients in that biomass and is added to detrital pool plus whatever reserves that fish had in reserves
    let Nfish 0.02999
    let mass-difference a.grunt * (body-length-current ^ (b.grunt) - (new-size) ^ (b.grunt)) ;;calculate the mass difference
    set death-nutrient-contribution Nfish * mass-difference + reserves                  ;;calculate the contribution of that fish's body mass to the detrital pool (N in tissue lost + reserves)
    set death-detritus death-detritus + death-nutrient-contribution                     ;;this nutrient goes into the "death-detritus pool"


    set body-length-current new-size                                                    ;;now set the body length to the new size
    set weight a.grunt * (new-size ^ b.grunt)                                           ;;calculates the new weight from L-W relationship based on new body length

    let reserves-wanted Nfish * weight * 0.01                                           ;;this calculates the reserves that the new fish should be set with

    ifelse reserves-wanted >= detrital-pool[                                            ;;if the reserves wanted are greater than what is available in the detrital pool
        set reserves-new (detrital-pool)]                                               ;;then, the new reserves are set to take up what is available in the detrital pool
       [set reserves-new reserves-wanted]                                               ;;if they are sufficient, then the reserves new are filled up to reserves-wanted
    set reserves reserves-new                                                           ;;sets fish's reserves at this amount
    set reserves-max Nfish * weight * 0.05                                              ;;sets a new reserves-max
    set detrital-pool detrital-pool - reserves-new                                      ;;and this removes the amount of nutrient it required to fill reserves-new
    set age 0                                                                           ;;resets the age, death-prob, Cn.real and death-nutrient contribution to 0
    set death-prob 0
    set death-nutrient-contribution 0
    set Cn.real 0
    set reserve-reset-count reserve-reset-count + 1                                     ;;this is just a counter to keep track of how many times a fish has died from this pathway
  ]]



                                                           ;; if detrital pool is less than what is required by growth, then the fish will eat anything that is available
set egestion-nutrient 0 ;Cn.real * FAn
set detrital-pool detrital-pool + egestion-nutrient
set nutrient-excretion Cn.real - egestion-nutrient - growth-nutrient
set wc-nutrients wc-nutrients + nutrient-excretion

;  ifelse detrital-pool > Cn [
;    set detrital-pool detrital-pool - Cn ][   ;;consumption of nutrients from the detrital pool
;    set reserves reserves - Cn]
end

to dead-fish-detritus
  set detrital-pool detrital-pool + death-detritus * death-detritus-decomp
  set death-detritus death-detritus - death-detritus * death-detritus-decomp
end

to death-rebirth ; as fish ages and gets bigger, there is an increase likelihood of it 'dying' and is replaced by a smaller individual 1-for-1
  if color = blue[
  let Y body-length-current                    ;if Y is the current body length
  set death-prob exp(1 * (Y - 45)) / 120
    ;if Y > 30 [ set death-prob Y / 75 * .05]      ;OLD death protocol when fish get bigger than 55 cm, they have a Y/75 * 0.05 chance of dying (ranges from 3 - 5% chance per tick)
  if random-float 1 < death-prob [             ;if a random number is lower than they % chance of dying, then they die
      set new-size body-length-start


;     ifelse body-length-current < 20 [
;        set new-size body-length-current][
;        set new-size 20 ] ;exp(ns)                    ;then a new fish of size pulled from log-normal distribution with u = 40cm and stdev = 10cm

      ;; when a fish 'dies' and is replaced, then the difference in weight between the two is converted to the nutrients in that biomass and is added to detrital pool plus whatever reserves that fish had in reserves
    let Nfish 0.02999
    let mass-difference a.grunt * (body-length-current ^ (b.grunt) - (new-size) ^ (b.grunt))
    set death-nutrient-contribution Nfish * mass-difference + reserves
    set death-detritus death-detritus + death-nutrient-contribution

    set body-length-current new-size           ;set the body length to the new size
    set weight a.grunt * (new-size ^ b.grunt)  ;calculates the weight from L-W relationship based on new body length

    let reserves-wanted Nfish * weight * 0.01   ;this calculates the reserves that the new fish should be set with

    ifelse reserves-wanted >= detrital-pool[
        set reserves-new (detrital-pool) ][set reserves-new reserves-wanted]


    set reserves reserves-new              ;sets fish's reserves at this amount
    set reserves-max Nfish * weight * 0.05
    set detrital-pool detrital-pool - reserves-new  ;and this removes the amount of nutrient it required to fill reserves-new
    set age 0 ;resets the age of the agent to 0
    set death-prob 0
    set death-nutrient-contribution 0
    set growth-nutrient 0
    set growth-amount 0
    set growth-amount-weight 0
    set reset-count reset-count + 1

    ]
  ]
  ;;below is death for species 2 with different length-weight constants !! needs to be updated with new death and reserve reset code from above.
;  if color = red[
;  let X body-length-current
;  if X > 20 [set death-prob X / 25 * .05]
;
;  if random-float 1 < death-prob [
;    set detrital-pool detrital-pool + 0.015 * (body-length-current - 1)^(3.059) * (Nbody / 100)  + reserves;;this calculates the difference between the length at death minus the new length (1), converted to weight, then multiplied by Nbody to find the amount of nutrients released back from decomp of dead fish to detrital pool
;    set body-length-current 1
;      l-w-convert
;     let reserves-reborn (Nbody / 100) * weight * 0.01
;     set reserves reserves-reborn
;     set detrital-pool detrital-pool - reserves-reborn
;    set age 0
;   ]
;  if reserves <= 0 [
;     set detrital-pool detrital-pool + 0.015 * (body-length-current - 1)^(3.059) * (Nbody / 100) + reserves ;;this calculates the difference between the length at death minus the new length (1), converted to weight, then multiplied by Nbody to find the amount of nutrients released back from decomp of dead fish to detrital pool
;     set body-length-current 1
;     l-w-convert
;     let reserves-reborn (Nbody / 100) * weight * 0.01
;     set reserves reserves-reborn
;     set detrital-pool detrital-pool - reserves-reborn
;      set age 0 ]
;  ]

     ;if a random number is lower than they % chance of dying, then they die

;     let m 20 ;mean
;     let v 10  ;variance
;     let u ln(m ^ 2 / sqrt ( v + m ^ 2))
;     let o sqrt(ln( 1 + (v / (m ^ 2))))
;     let ns random-normal u o
;     let new-size 20 ;exp(ns)                    ;then a new fish of size pulled from log-normal distribution with u = 40cm and stdev = 10cm

;      ;; when a fish 'dies' and is replaced, then the difference in weight between the two is converted to the nutrients in that biomass and is added to detrital pool plus whatever reserves that fish had in reserves
;    let Nfish 0.0299
;    let mass-difference 0.0121 * (body-length-current ^ (3.161) - (new-size) ^ (3.161))
;    set death-nutrient-contribution Nfish * mass-difference + reserves
;    set death-detritus death-detritus + death-nutrient-contribution
;
;
;
;    set body-length-current new-size           ;set the body length to the new size
;    set weight 0.0121 * (new-size ^ 3.161)  ;calculates the weight from L-W relationship based on new body length
;     let reserves-new Nfish * weight * 0.01   ;this calculates the reserves that the new fish should be set with
;    set reserves reserves-new              ;sets fish's reserves at this amount
;    set reserves-max Nfish * weight * 0.05
;    set detrital-pool detrital-pool - reserves-new  ;and this removes the amount of nutrient it required to fill reserves-new
;    set age 0
;    set death-prob 0
;    set death-nutrient-contribution 0
;    set reset-count reset-count + 1  ;resets the age of the agent to 0
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;patch coloration options;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to recolor
  if visualization = "default" []

  if visualization = "wc-nutrients" [
      if seagrass? = TRUE [
    set pcolor scale-color blue wc-nutrients 0.5 0.1 ]]

  if visualization = "ag-biomass" [
     if seagrass? = TRUE [
    set pcolor scale-color green ag-biomass 500 386]]

  if visualization = "death-detritus" [
    if seagrass? = true [
      set pcolor scale-color pink death-detritus 0 0.8]]
end
@#$#@#$#@
GRAPHICS-WINDOW
399
10
719
331
-1
-1
6.12
1
10
1
1
1
0
0
0
1
-25
25
-25
25
0
0
1
ticks
30.0

BUTTON
31
80
94
113
NIL
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

BUTTON
110
80
176
113
NIL
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

SLIDER
14
184
248
217
starting-wc-nutrient
starting-wc-nutrient
0
1.5
0.25
0.01
1
g/patch
HORIZONTAL

SLIDER
14
148
186
181
minutes-per-tick
minutes-per-tick
1
120
120.0
1
1
NIL
HORIZONTAL

PLOT
13
393
421
640
Aboveground Biomass
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"near" 1.0 0 -16777216 true "" "plot mean [ag-biomass] of patches with [reef-proximity = \"near\" and seagrass? = true]"
"mid" 1.0 0 -7500403 true "" "plot mean [ag-biomass] of patches with [reef-proximity = \"mid\" and seagrass? = true]"
"far" 1.0 0 -2674135 true "" "plot mean [ag-biomass] of patches with [reef-proximity = \"far\" and seagrass? = true]"
"mean" 1.0 0 -955883 true "" "plot mean [ag-biomass] of patches with [seagrass? = true]"

SLIDER
20
237
192
270
sg-density
sg-density
0
1000
1000.0
100
1
NIL
HORIZONTAL

BUTTON
34
43
97
76
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
13
640
420
800
detrital pool
NIL
NIL
0.0
10.0
0.0
2.0
true
true
"" ""
PENS
"near" 1.0 0 -16777216 true "" "plot mean [detrital-pool] of patches with [reef-proximity = \"near\" and seagrass? = true]"
"mid" 1.0 0 -7500403 true "" "plot mean [detrital-pool] of patches with [reef-proximity = \"mid\" and seagrass? = true]"
"far" 1.0 0 -2674135 true "" "plot mean [detrital-pool] of patches with [reef-proximity = \"far\" and seagrass? = true]"

PLOT
424
393
807
640
Belowground Biomass
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"near" 1.0 0 -16777216 true "" "plot mean [bg-biomass] of patches with [reef-proximity = \"near\" and seagrass? = true]"
"mid" 1.0 0 -7500403 true "" "plot mean [bg-biomass] of patches with [reef-proximity = \"mid\" and seagrass? = true]"
"far" 1.0 0 -2674135 true "" "plot mean [bg-biomass] of patches with [reef-proximity = \"far\" and seagrass? = true]"
"mean" 1.0 0 -955883 true "" "plot mean [bg-biomass] of patches with [ seagrass? = true]"

MONITOR
953
33
1059
78
NIL
bg-max
3
1
11

MONITOR
870
32
948
77
NIL
ag-max-1
17
1
11

CHOOSER
1285
340
1424
385
model-version
model-version
"reef" "no reef"
1

SLIDER
742
125
861
158
population-1
population-1
0
150
1.0
1
1
NIL
HORIZONTAL

SLIDER
1352
832
1472
865
population-2
population-2
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
1478
832
1604
865
mean-travel-red
mean-travel-red
0
10
0.0
1
1
NIL
HORIZONTAL

SLIDER
1005
125
1132
158
mean-travel-blue
mean-travel-blue
0.0001
10
8.0
1
1
NIL
HORIZONTAL

CHOOSER
1287
390
1426
435
visualization
visualization
"default" "wc-nutrients" "ag-biomass" "death-detritus"
1

PLOT
813
398
1216
636
Mean body length of fish
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"body-weight" 1.0 0 -7500403 true "" "plot mean [weight] of turtles"
"body-length" 1.0 0 -2674135 true "" "plot mean [body-length-current] of turtles"

MONITOR
264
284
393
329
Sum fish pee
sum [nutrient-excretion] of turtles
10
1
11

MONITOR
270
200
377
245
Days
ticks * minutes-per-tick / (60 * 24)
2
1
11

MONITOR
719
343
809
388
bg-bio mean
mean [bg-biomass] of patches with [seagrass? = true]
2
1
11

SLIDER
19
312
239
345
starting-ag-biomass
starting-ag-biomass
0
1
0.7
0.05
1
g/shoot
HORIZONTAL

SLIDER
20
350
284
383
starting-bg-biomass
starting-bg-biomass
0
600
250.0
10
1
g/m^2 (dry)
HORIZONTAL

SLIDER
20
274
193
307
detrital-fraction
detrital-fraction
0
5
0.15
0.05
1
NIL
HORIZONTAL

MONITOR
330
347
422
392
ag-bio mean
mean [ag-biomass] of patches with [seagrass? = true]
2
1
11

SLIDER
744
240
973
273
percent-growth-sloughed-bg
percent-growth-sloughed-bg
0
100
15.0
1
1
%
HORIZONTAL

SLIDER
745
202
971
235
percent-growth-sloughed-ag
percent-growth-sloughed-ag
0
100
15.0
1
1
%
HORIZONTAL

SLIDER
1350
867
1570
900
decomp-rate
decomp-rate
0
100
0.0
1
1
% loss/year
HORIZONTAL

SLIDER
744
165
969
198
nutrient-diffusion-rate
nutrient-diffusion-rate
0
1
0.6
0.05
1
NIL
HORIZONTAL

MONITOR
173
663
266
708
detritus mean
mean [detrital-pool] of patches with [seagrass? = true]
7
1
11

MONITOR
909
643
1002
688
nutrient mean
mean [wc-nutrients]  of patches with [seagrass? = true]
3
1
11

MONITOR
812
645
907
690
[wc.nut] (uM)
mean [wc-nutrients * g>umol.N / 10000] of patches
3
1
11

SLIDER
1349
792
1519
825
base-slough-rate
base-slough-rate
0
.001
0.0
0.00001
1
NIL
HORIZONTAL

SLIDER
744
280
971
313
percent-slough-to-detritus
percent-slough-to-detritus
0
100
90.0
1
1
NIL
HORIZONTAL

MONITOR
1274
44
1366
89
near biomass
mean [ag-biomass] of patches \nwith [reef-proximity = \"near\" and seagrass? = true]
3
1
11

MONITOR
1275
92
1367
137
mid biomass
mean [ag-biomass] of patches \nwith [reef-proximity = \"mid\"]
3
1
11

MONITOR
1275
139
1367
184
far biomass
mean [ag-biomass] of patches \nwith [reef-proximity = \"far\"]
3
1
11

MONITOR
1473
42
1562
87
near detritus
mean [detrital-pool] of patches \nwith [reef-proximity = \"near\" and seagrass? = true]
4
1
11

MONITOR
1473
89
1563
134
mid detritus
mean [detrital-pool] of patches \nwith [reef-proximity = \"mid\"]
3
1
11

MONITOR
1474
138
1564
183
far detritus
mean [detrital-pool] of patches \nwith [reef-proximity = \"far\"]
3
1
11

MONITOR
1573
42
1667
87
near nutrients
mean [wc-nutrients] of patches \nwith [reef-proximity = \"near\"]
3
1
11

MONITOR
1572
89
1667
134
mid nutrients
mean [wc-nutrients] of patches \nwith [reef-proximity = \"mid\"]
3
1
11

MONITOR
1572
138
1669
183
far nutrients
mean [wc-nutrients] of patches \nwith [reef-proximity = \"far\"]
3
1
11

MONITOR
1373
45
1461
90
near bg-bio
mean [bg-biomass] of patches \nwith [reef-proximity = \"near\" and seagrass? = true]
3
1
11

MONITOR
1374
93
1462
138
mid bg-bio
mean [bg-biomass] of patches \nwith [reef-proximity = \"mid\" and seagrass? = true]
3
1
11

MONITOR
1375
139
1463
184
far bg-bio
mean [bg-biomass] of patches \nwith [reef-proximity = \"far\" and seagrass? = true]
3
1
11

MONITOR
1124
479
1214
524
mean length
mean [body-length-current] of turtles
3
1
11

PLOT
425
643
809
793
WC nutrients
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"near" 1.0 0 -16777216 true "" "plot mean [wc-nutrients] of patches with [reef-proximity = \"near\"]"
"mid" 1.0 0 -7500403 true "" "plot mean [wc-nutrients] of patches with [reef-proximity = \"mid\"]"
"far" 1.0 0 -2674135 true "" "plot mean [wc-nutrients] of patches with [reef-proximity = \"far\"]"

MONITOR
1289
444
1472
489
mean reserve/max-reserve
(mean [reserves] of turtles) / (mean [reserves-max] of turtles)
3
1
11

MONITOR
270
152
327
197
years
ticks * minutes-per-tick / (60 * 24 * 365)
3
1
11

SLIDER
863
125
1004
158
starting-mean-size
starting-mean-size
0
30
9.0
1
1
NIL
HORIZONTAL

SLIDER
1280
207
1483
240
death-detritus-diffusion
death-detritus-diffusion
0
1
0.6
.05
1
NIL
HORIZONTAL

SLIDER
1282
243
1482
276
death-detritus-decomp
death-detritus-decomp
0
1
0.6
0.05
1
NIL
HORIZONTAL

SLIDER
1523
792
1710
825
base-slough-rate-bg
base-slough-rate-bg
0
0.001
0.0
0.00001
1
NIL
HORIZONTAL

MONITOR
273
663
366
708
sum detritus
sum [detrital-pool] of patches
5
1
11

TEXTBOX
1349
757
1517
785
Sliders not getting used for now:
11
0.0
1

MONITOR
1284
288
1474
333
Sum
(sum [detrital-pool] of patches) \n + (sum [wc-nutrients] of patches) \n + (sum [ag-biomass] of patches * gamma-ag )\n + (sum [bg-biomass] of patches * gamma-bg)\n + (sum [weight] of turtles * 0.02999)\n + (sum [death-detritus] of patches)\n + (sum [reserves] of turtles)
17
1
11

MONITOR
1074
35
1144
80
NIL
Nc.ag
3
1
11

MONITOR
1153
35
1237
80
NIL
Nc.bg
3
1
11

SWITCH
882
362
1026
395
Nc-thresholds
Nc-thresholds
0
1
-1000

MONITOR
1124
529
1214
574
weight
mean [weight] of turtles
17
1
11

SLIDER
980
167
1150
200
bg.nut.threshold
bg.nut.threshold
0
5
3.0
0.1
1
uM
HORIZONTAL

SLIDER
978
202
1151
235
sigmoid-slope.ag1
sigmoid-slope.ag1
0
50
6.0
1
1
NIL
HORIZONTAL

MONITOR
783
34
861
79
NIL
ag-max-2
17
1
11

SLIDER
980
239
1153
272
sigmoid-slope.bg
sigmoid-slope.bg
0
50
6.0
1
1
NIL
HORIZONTAL

SLIDER
877
324
1050
357
detrital-diffusion
detrital-diffusion
0
1
0.0
0.1
1
NIL
HORIZONTAL

MONITOR
1308
579
1490
624
dN.bg/dt
mean [dN.bg/dt] of patches
17
1
11

MONITOR
1308
628
1491
673
dN.ag/dt
mean [dN.ag/dt] of patches
17
1
11

MONITOR
1308
529
1491
574
dN.accel/dt
mean [dN.accel/dt] of patches
17
1
11

SLIDER
982
280
1155
313
sigmoid-slope.ag2
sigmoid-slope.ag2
0
40
6.0
1
1
NIL
HORIZONTAL

SWITCH
1114
721
1217
754
open?
open?
1
1
-1000

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
NetLogo 6.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="reef v no reef" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1500"/>
    <metric>mean [ag-biomass] of patches</metric>
    <metric>mean [bg-biomass] of patches</metric>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="water.temp">
      <value value="26"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;reef&quot;"/>
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="54"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;ag-biomass&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-1">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-nutrient">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="fish-pop" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8760"/>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="water.temp">
      <value value="26"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="5.0E-4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;ag-biomass&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="population-1" first="1" step="50" last="300"/>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="fish-travel" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="16000"/>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="water.temp">
      <value value="26"/>
    </enumeratedValueSet>
    <steppedValueSet variable="mean-travel-blue" first="0" step="2" last="10"/>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="5.0E-4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;ag-biomass&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-1">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="reef-no-reef" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1600"/>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches</metric>
    <metric>mean[detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean[ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="water.temp">
      <value value="26"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;reef&quot;"/>
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;ag-biomass&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-1">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="longterm dynamics" repetitions="40" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="30000"/>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches</metric>
    <metric>mean[detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean[ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [body-length-current] of turtles</metric>
    <metric>(mean [reserves] of turtles) / (mean [reserves-max] of turtles)</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="water.temp">
      <value value="26"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="2.0E-4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;ag-biomass&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-1">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="reef-no-reef-pop" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches</metric>
    <metric>mean[detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean[ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [body-length-current] of turtles</metric>
    <metric>(mean [reserves] of turtles) / (mean [reserves-max] of turtles)</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="water.temp">
      <value value="26"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;reef&quot;"/>
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="nutrient-diffusion-rate" first="0.1" step="0.3" last="1"/>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;death-detritus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="350"/>
    </enumeratedValueSet>
    <steppedValueSet variable="population-1" first="1" step="30" last="180"/>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="slough.to.detritus" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="400"/>
    <metric>mean [wc-nutrients] of patches with [seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>sum [detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [seagrass? = true]</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="55"/>
      <value value="75"/>
      <value value="95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="water.temp">
      <value value="26"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.65"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;death-detritus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-1">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="percent-growth-sloughed-ag" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="400"/>
    <metric>mean [wc-nutrients] of patches with [seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>sum [detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [seagrass? = true]</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="water.temp">
      <value value="26"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="55"/>
      <value value="75"/>
      <value value="95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.65"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;death-detritus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-1">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="starting.wc" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="400"/>
    <metric>mean [wc-nutrients] of patches with [seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>sum [detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [seagrass? = true]</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="water.temp">
      <value value="26"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.65"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;death-detritus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-1">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="starting.ag.bio" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="400"/>
    <metric>mean [wc-nutrients] of patches with [seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>sum [detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [seagrass? = true]</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="water.temp">
      <value value="26"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.3"/>
      <value value="0.5"/>
      <value value="0.65"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;death-detritus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-1">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="pop.size.2" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <metric>mean [wc-nutrients] of patches with [seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>sum [detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [seagrass? = true]</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="water.temp">
      <value value="26"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.65"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;death-detritus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="350"/>
    </enumeratedValueSet>
    <steppedValueSet variable="population-1" first="50" step="25" last="150"/>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="reef.no.reef.travel." repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="21600"/>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches</metric>
    <metric>mean[detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean[ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [body-length-current] of turtles</metric>
    <metric>(mean [reserves] of turtles) / (mean [reserves-max] of turtles)</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="water.temp">
      <value value="26"/>
    </enumeratedValueSet>
    <steppedValueSet variable="mean-travel-blue" first="0" step="2" last="10"/>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.65"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;reef&quot;"/>
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;death-detritus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-1">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="reef.no.reef.travel.3" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="35000"/>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches</metric>
    <metric>mean[detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean[ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [body-length-current] of turtles</metric>
    <metric>(mean [reserves] of turtles) / (mean [reserves-max] of turtles)</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="water.temp">
      <value value="26"/>
    </enumeratedValueSet>
    <steppedValueSet variable="mean-travel-blue" first="0" step="2" last="10"/>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.3"/>
      <value value="0.5"/>
      <value value="0.65"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;reef&quot;"/>
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0"/>
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;death-detritus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="200"/>
      <value value="250"/>
      <value value="300"/>
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-1">
      <value value="75"/>
      <value value="100"/>
      <value value="125"/>
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nc-thresholds">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sigmoid shape implications" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="7500"/>
    <metric>mean[ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="6.0001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.ag">
      <value value="1"/>
      <value value="5"/>
      <value value="15"/>
      <value value="25"/>
      <value value="35"/>
      <value value="45"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bg.nut.threshold">
      <value value="3.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.bg">
      <value value="1"/>
      <value value="5"/>
      <value value="15"/>
      <value value="25"/>
      <value value="35"/>
      <value value="45"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;death-detritus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nc-thresholds">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-1">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sigmoid shape ag2" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="7500"/>
    <metric>mean[ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="6.0001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.ag">
      <value value="1"/>
      <value value="5"/>
      <value value="15"/>
      <value value="25"/>
      <value value="35"/>
      <value value="45"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bg.nut.threshold">
      <value value="3.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.bg">
      <value value="1"/>
      <value value="5"/>
      <value value="15"/>
      <value value="25"/>
      <value value="35"/>
      <value value="45"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;death-detritus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="420"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nc-thresholds">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-1">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="reef.noreef" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean[ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="6.0001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;reef&quot;"/>
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.ag">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bg.nut.threshold">
      <value value="3.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.bg">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;death-detritus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="420"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nc-thresholds">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="population-1" first="1" step="10" last="101"/>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="diffusion" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean[ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="6.0001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;reef&quot;"/>
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.ag">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <steppedValueSet variable="nutrient-diffusion-rate" first="0" step="0.3" last="1"/>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0.2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="detrital-diffusion" first="0" step="0.3" last="1"/>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bg.nut.threshold">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.bg">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;death-detritus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nc-thresholds">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-1">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popsize" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean[ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="6.0001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;no reef&quot;"/>
      <value value="&quot;reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.ag">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-diffusion">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bg.nut.threshold">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.bg">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;death-detritus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nc-thresholds">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="population-1" first="1" step="10" last="101"/>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popsize" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>sum [ag-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>sum [bg-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>sum [detrital-pool] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>sum [wc-nutrients] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>sum [ag-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>sum [bg-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>sum [detrital-pool] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>sum [wc-nutrients] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>sum [ag-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>sum [bg-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>sum [detrital-pool] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>sum [wc-nutrients] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>sum [ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>sum [bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>sum [detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>sum [wc-nutrients] of patches</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="6.0001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;no reef&quot;"/>
      <value value="&quot;reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.ag">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-diffusion">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bg.nut.threshold">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.bg">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;death-detritus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nc-thresholds">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="population-1" first="1" step="10" last="101"/>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="popsize.detsmall" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean[ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches</metric>
    <metric>sum [ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>sum [bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>sum [detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>sum [wc-nutrients] of patches</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="6.0001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;reef&quot;"/>
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.ag1">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.ag2">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <steppedValueSet variable="detrital-fraction" first="0.05" step="0.05" last="0.35"/>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-diffusion">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bg.nut.threshold">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.bg">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;death-detritus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-1">
      <value value="15"/>
      <value value="50"/>
      <value value="85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Nc-thresholds">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="full.exploration" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>count turtles</metric>
    <steppedValueSet variable="starting-wc-nutrient" first="0.01" step="0.01" last="1.5"/>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;reef&quot;"/>
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="population-1" first="1" step="10" last="100"/>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="mean-travel-blue" first="1" step="2" last="10"/>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;default&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="250"/>
    </enumeratedValueSet>
    <steppedValueSet variable="detrital-fraction" first="0.05" step="0.05" last="1"/>
    <steppedValueSet variable="percent-growth-sloughed-bg" first="1" step="5" last="30"/>
    <steppedValueSet variable="percent-growth-sloughed-ag" first="1" step="5" last="30"/>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="nutrient-diffusion-rate" first="0.5" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="percent-slough-to-detritus" first="95" step="1" last="99"/>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nc-thresholds">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="bg.nut.threshold" first="0.01" step="0.5" last="4"/>
    <steppedValueSet variable="sigmoid-slope.ag1" first="1" step="2" last="20"/>
    <steppedValueSet variable="sigmoid-slope.bg" first="1" step="2" last="20"/>
    <steppedValueSet variable="detrital-diffusion" first="0.5" step="0.1" last="0.9"/>
    <steppedValueSet variable="sigmoid-slope.ag2" first="1" step="2" last="20"/>
  </experiment>
  <experiment name="base-threshold and slope experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;reef&quot;"/>
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-1">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;default&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nc-thresholds">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="bg.nut.threshold" first="0.01" step="0.5" last="4"/>
    <steppedValueSet variable="sigmoid-slope.ag1" first="1" step="5" last="20"/>
    <steppedValueSet variable="sigmoid-slope.bg" first="1" step="5" last="20"/>
    <enumeratedValueSet variable="detrital-diffusion">
      <value value="0.75"/>
    </enumeratedValueSet>
    <steppedValueSet variable="sigmoid-slope.ag2" first="1" step="5" last="20"/>
  </experiment>
  <experiment name="sigmoid slope experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="15000"/>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean[ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches</metric>
    <metric>sum [ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>sum [bg-biomass] of patches with [seagrass? = true]</metric>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;reef&quot;"/>
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-1">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;default&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nc-thresholds">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bg.nut.threshold">
      <value value="2.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="sigmoid-slope.ag1" first="1" step="5" last="15"/>
    <steppedValueSet variable="sigmoid-slope.bg" first="1" step="5" last="15"/>
    <enumeratedValueSet variable="detrital-diffusion">
      <value value="0.75"/>
    </enumeratedValueSet>
    <steppedValueSet variable="sigmoid-slope.ag2" first="1" step="5" last="15"/>
  </experiment>
  <experiment name="small popsize increments" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="25000"/>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean[ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches</metric>
    <metric>sum [ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>sum [bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>sum [detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>sum [wc-nutrients] of patches</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;reef&quot;"/>
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.ag1">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.ag2">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-diffusion">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bg.nut.threshold">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.bg">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;death-detritus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="250"/>
    </enumeratedValueSet>
    <steppedValueSet variable="population-1" first="10" step="10" last="100"/>
    <enumeratedValueSet variable="Nc-thresholds">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="small popsize increments finish" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="25000"/>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean[ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches</metric>
    <metric>sum [ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>sum [bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>sum [detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>sum [wc-nutrients] of patches</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;reef&quot;"/>
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.ag1">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.ag2">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0.1"/>
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-diffusion">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bg.nut.threshold">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.bg">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;death-detritus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="250"/>
    </enumeratedValueSet>
    <steppedValueSet variable="population-1" first="10" step="10" last="100"/>
    <enumeratedValueSet variable="Nc-thresholds">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="bg.nut.thresh" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="25000"/>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "near" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "mid" and seagrass? = true]</metric>
    <metric>mean [ag-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [bg-biomass] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [detrital-pool] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches with [reef-proximity = "far" and seagrass? = true]</metric>
    <metric>mean[ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>mean[detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>mean [wc-nutrients] of patches</metric>
    <metric>sum [ag-biomass] of patches with [seagrass? = true]</metric>
    <metric>sum [bg-biomass] of patches with [seagrass? = true]</metric>
    <metric>sum [detrital-pool] of patches with [seagrass? = true]</metric>
    <metric>sum [wc-nutrients] of patches</metric>
    <enumeratedValueSet variable="percent-slough-to-detritus">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-blue">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-ag">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-diffusion">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-travel-red">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-ag-biomass">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;reef&quot;"/>
      <value value="&quot;no reef&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-growth-sloughed-bg">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.ag1">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sigmoid-slope.ag2">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sg-density">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-wc-nutrient">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-detritus-decomp">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nutrient-diffusion-rate">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minutes-per-tick">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-fraction">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decomp-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="detrital-diffusion">
      <value value="0.3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="bg.nut.threshold" first="1.5" step="0.5" last="4.5"/>
    <enumeratedValueSet variable="sigmoid-slope.bg">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-mean-size">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;death-detritus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-bg-biomass">
      <value value="250"/>
    </enumeratedValueSet>
    <steppedValueSet variable="population-1" first="10" step="25" last="100"/>
    <enumeratedValueSet variable="Nc-thresholds">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="base-slough-rate-bg">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-2">
      <value value="0"/>
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
