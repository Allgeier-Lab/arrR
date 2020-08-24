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
