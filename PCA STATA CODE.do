
label list water toilet
gen water1=0
replace water1=1 if water==11
replace water1=1 if water==12
replace water1=1 if water==13
replace water1=1 if water==21
replace water1=1 if water==91
label variable water1 "drinking water source"
label define Water1 0 "unsafe" 1 "safe"
label values water1 Water1

gen toilet1=0
replace toilet1=1 if toilet==11
replace toilet1=1 if toilet==13
replace toilet1=1 if toilet==22
label variable toilet1 "toilet usage"
label define Toilet1 0 "unhygienic practice" 1 "hygienic_practice"
label values toilet1 Toilet1
sum electricity-toilet1

pca electricity radio tv mobile refregerator almirah table chair watch ///
cycle motorcycle rikshow hhland firmland water1 toilet1, factor(1)
Or, 
pca electricity - motorcycle rikshow - toilet1, factor(1)
predict comp1
ren comp1 w_index 
xtile w_quintile=w_index, nq(5)
lab var w_quintile "wealth quintile"
la de w_quintile0 1"poorest" 2"poorer" 3"middle" 4"richer" 5"richest"
la values w_quintile w_quintile0
tab w_quintile