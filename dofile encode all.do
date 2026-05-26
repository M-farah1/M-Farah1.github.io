foreach v of varlist _all {
    capture confirm string variable `v'
    if !_rc {
        encode `v', gen(n_`v')
        drop `v'
        rename n_`v' `v'
    }
}

import excel "C:\Users\THINKPAD\Documents\Practice Data_COVID-19_FINAL.xls", sheet("Sheet1")
do "C:\Users\THINKPAD\AppData\Local\Temp\STD3ed8_000000.tmp"
edit
save "covid19_vaccine.dta"
use "covid19_vaccine.dta", clear
edit
edit
label variable A "ID"
label variable B "Age"
label variable C "Age range"
label variable D "Sex"
label variable E "Marital Status"
label variable F "Highest Education Level Attained"
label variable G "Has COVID-19 vaccine arrived your country"
label variable H "Have you accepted vaccination for COVID-19"
label variable I "Do you think the COVID-19 vaccine is safe"
label variable J "Do you think the COVID-19 vaccine is efficacious"
label variable K "Do you think the COVID-19 vaccine will alter your DNA"
label variable L "Do you think the COVID-19 vaccine contains a tracking device"
label variable M "Do you think the COVID-19 vaccine have serious side effects"
label variable N "Do you think the COVID-19 vaccine for Africa is different from that in other continents"
label variable O "Do you think one can still get COVID-19 after vaccination"
label variable P "Have you done COVID-19 test before"
label variable Q "Have you been diagnosed with COVID-19 before"
label variable R "If you were diagnosed with COVID-19, what will be your first option"
label variable S "Should I still get the COVID-19 vaccine if I recovered from COVID"
label variable T "Hand washing hygiene"
label variable U "Wearing of nose mask or face shield"
label variable V "Social distancing"
label variable W "If the government makes the COVID-19 vaccine available to the public, would you get vaccinated"
label variable X "Are you willing to pay for the vaccine"
label variable Y "Do you think enough awareness has been created about the COVID-19 vaccine"
label variable Z "COVID-19 Test Result"
label variable AA "Weight"
label variable AB "Height"
label variable AC "BMI"
label variable AD "BMI interpretation"
label variable AE "Depression status"
label variable AF "Depression status_dichot"
save "covid19_vaccinee.dta", replace
cls