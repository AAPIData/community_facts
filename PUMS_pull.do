*import delimited "/Users/sunnyshao/Dropbox/APIAVote Fact Sheets/2020 Fact Sheets/raw data/csv_pia/psam_p19.csv"
*importing 2016 5-year pums as baseline data

import delimited "/Users/sunnyshao/Dropbox/APIAVote Fact Sheets/2020 Fact Sheets/2012-2016 pums/ss16pia.csv"

use "/Users/sunnyshao/Dropbox/APIAVote Fact Sheets/2020 Fact Sheets/2012-2016 pums/ss16pia.dta"
gen nhpi = 1 if racnh==1 | racpi==1
gen aa = 1 if racasn == 1
gen aapi = 0
replace aapi = 1 if aa == 1 | nhpi == 1
recode cit (1/4=1) (5=0), gen(citizen)
gen adult = 1 if agep >= 18
gen cvap = 0
replace cvap = 1 if citizen == 1 & adult == 1
gen aapi_cvap = 0
replace aapi_cvap = 1 if aapi == 1 & cvap == 1

recode nativity (2=1) (1=0), gen(foreign_born)
gen aapi_fb = 0
replace aapi_fb = 1 if foreign_born == 1 & aapi == 1

svyset [pweight = pwgtp]
*what is the percent AAPI electorate of total electorates?
svy:tab aapi_cvap if cvap==1
svy:tab aapi_fb if foreign_born == 1
svy:tab aapi


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
clear
///set working directory to your local file
* for example 
set more off
use "/Users/sunnyshao/Dropbox/APIAVote Fact Sheets/2020 Fact Sheets/raw data/csv_pia/2018-5YR-pia.dta"
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

import delimited "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/2018 5-YEAR pus/psam_pusa.csv"
save "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/2018 5-YEAR pus/a.dta"
clear all
import delimited "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/2018 5-YEAR pus/psam_pusb.csv"
save "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/2018 5-YEAR pus/b.dta"
clear all
import delimited "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/2018 5-YEAR pus/psam_pusc.csv"
save "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/2018 5-YEAR pus/c.dta"
clear all
import delimited "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/2018 5-YEAR pus/psam_pusd.csv"
save "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/2018 5-YEAR pus/d.dta"
append using "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/2018 5-YEAR pus/a.dta"
append using "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/2018 5-YEAR pus/b.dta"
append using "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/2018 5-YEAR pus/c.dta"
save "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/2018 5-YEAR pus/2018-5Y-pus.dta"
save "/Users/sunnyshao/Dropbox/AAPI DATA Desktop/2018 5-YEAR pus/2018-5Y-pus-slim.dta"



import delimited "/Users/sunnyshao/Documents/csv_pus (5)/ss12pusa.csv"
save "/Users/sunnyshao/Documents/csv_pus (5)/a.dta"
clear all
import delimited "/Users/sunnyshao/Documents/csv_pus (5)/ss12pusb.csv"
save "/Users/sunnyshao/Documents/csv_pus (5)/b.dta"
clear all
import delimited "/Users/sunnyshao/Documents/csv_pus (5)/ss12pusc.csv"
save "/Users/sunnyshao/Documents/csv_pus (5)/c.dta"
clear all
import delimited "/Users/sunnyshao/Documents/csv_pus (5)/ss12pusd.csv"
save "/Users/sunnyshao/Documents/csv_pus (5)/d.dta"

append using "/Users/sunnyshao/Documents/csv_pus (5)/a.dta"
append using "/Users/sunnyshao/Documents/csv_pus (5)/b.dta"
append using "/Users/sunnyshao/Documents/csv_pus (5)/c.dta"
save "/Users/sunnyshao/Documents/csv_pus (5)/2012-5Y-pus.dta"
keep serialno st rt sporder pwgtp rac1p rac2p05 rac2p12 racasn racnhpi schl povpip agep cit yoep05 yoep12 fyoep lanx eng hicov priv pubcov nativity lanp05 lanp12 flanp
save "/Users/sunnyshao/Documents/csv_pus (5)/2012-5Y-pus-slim.dta"
keep st pwgtp racasn racnhpi cit nativity agep
save "/Users/sunnyshao/Documents/csv_pus (5)/2012-5Y-pus-cvap.dta"













//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
clear
set more off
use "/Users/sunnyshao/Documents/2018 5-YEAR pus/2018-5Y-pus-slim.dta"
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

keep serialno st rt sporder pwgtp rac1p rac2p racasn racnh racpi schl povpip agep cit yoep lanx eng hicov privcov nativity lanp
///set up survey weight
svyset [pweight = pwgtp]
///groups
*asian Americans & NHPI , US total
recode rac2p (1/37=.) (40=.) (47=.)  ///
(51/52 =.) (55=.) (58/59=.) (63/64=.) (66/68=.) ///
(38 = 1 "Indian") (39=2 "Bangladeshi") (41 = 3 "Burmese") ///
(42 = 4 "Cambodian") (43 = 5 "Chinese") (44=16 "Taiwanese")  ///
(45 = 7 "Filipino") (46 = 8 "Hmong") (48 = 9 "Japanese") ///
(49 = 10 "Korean") (50 = 11 "Laotian") (53 = 13 "Nepalese") ///
(54 = 14 "Pakistani") (56 = 17 "Thai")  (57 = 19 "Vietnamese") ///
 (60=12 "Native Hawaiian") (61=15 "Samoan") ///
(62=18 "Tongan") (65=6 "Fijian"), gen(detailed_group)

///racial groups
gen nhpi = 0 
replace nhpi = 1 if racnh==1 | racpi==1
gen aa = 0
replace aa = 1 if racasn == 1

gen aapi = 0
replace aapi = 1 if aa == 1 | nhpi == 1

cd "/Users/sunnyshao/Documents/community_facts/nationaldata_2018/PUMS"

//EDUCATIONAL ATTAINMENT
*less than hs
*hs or ged
*ba or higher
recode schl (1/15=1 "less HS") (16/17=2 "HS or GED") (21/24=3 "ba higher") (18/20=4), gen(edu)
*detailed AAPI
svy:tab detailed_group edu if agep>24, row


*Poverty rate
//report missing amount
//is the missing rate by race 
*gen missing_pov = 1 if povpip==.
*replace missing_pov = 0 if povpip!=.
/////////////
//missing data in all
*tab missing_pov
*tab missing_pov if racasn==1
//missing data among detailed asian ethnic groups
*tab missing_pov detailed_asian, col

///record income-poverty ratio
/// 0-100% below poverty
/// 101-501% above poverty
recode povpip (0/100 = 1) (101/501 = 0), gen(pov)
*overall share of poverty
// 1 = below poverty 0=above poverty
svy:tab detailed_group pov, row

*share of child poverty
// 1 = below poverty 0=above poverty
//svy:tab pov if agep<18 & rac1p==6
//svy:tab pov if agep<18 & rac1p==7
svy:tab detailed_group pov if agep<18, row
*share of senior poverty
// 1 = below poverty 0=above poverty
svy:tab detailed_group pov if agep>64, row

//langauge diversity
recode lanx (2=0), gen(other_lang)
*Speak language other than english at home （lanx)
//1=speak other languages 2=only speak English
svy:tab detailed_group other_lang if agep >= 5, row

*LEP (ENG)
/// 1=speak English less than very well; 0=speak English very well
gen LEP = 0
replace LEP = 1 if eng >=2 & eng <= 4

//LEP rate = among those who speak other langauges that speak English less than very well
svy:tab detailed_group LEP if agep >= 5, row


*most common languages i.e. "Mon-Kher/Cambodian, spoken by xxx,xxx people"
//lanp
gen language = lanp if racasn==1

////national level
keep pwgtp lanp
svyset[pw=pwgtp]
estpost svy:tab lanp
esttab using "svy_lang.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

///for asian alone
estpost svy:tab lanp if racasn==1
esttab using "svy_lang_aa.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

///for NHPI alone
estpost svy:tab lanp if nhpi==1
esttab using "svy_lang_pi.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

estpost svy:tab language if detailed_group==1

esttab using "svy_lang_asndetail1.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

estpost svy:tab language if detailed_group==2

esttab using "svy_lang_asndetail2.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

estpost svy:tab language if detailed_group==3

esttab using "svy_lang_asndetail3.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

estpost svy:tab language if detailed_group==4

esttab using "svy_lang_asndetail4.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

estpost svy:tab language if detailed_group==5

esttab using "svy_lang_asndetail5.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

estpost svy:tab language if detailed_group==6

esttab using "svy_lang_asndetail6.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

estpost svy:tab language if detailed_group==7

esttab using "svy_lang_asndetail7.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

estpost svy:tab language if detailed_group==8

esttab using "svy_lang_asndetail8.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

estpost svy:tab language if detailed_group==9

esttab using "svy_lang_asndetail9.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

estpost svy:tab language if detailed_group==10

esttab using "svy_lang_asndetail10.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

estpost svy:tab language if detailed_group==11

esttab using "svy_lang_asndetail11.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

estpost svy:tab language if detailed_group==12

esttab using "svy_lang_asndetail12.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

estpost svy:tab language if detailed_group==13

esttab using "svy_lang_asndetail13.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

estpost svy:tab language if detailed_group==14

esttab using "svy_lang_asndetail14.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

estpost svy:tab language if detailed_group==15

esttab using "svy_lang_asndetail15.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

estpost svy:tab language if detailed_group==16

esttab using "svy_lang_asndetail16.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

estpost svy:tab language if detailed_group==17

esttab using "svy_lang_asndetail17.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

estpost svy:tab language if detailed_group==18

esttab using "svy_lang_asndetail18.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain

estpost svy:tab language if detailed_group==19

esttab using "svy_lang_asndetail19.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs  label plain


//health insurance
*share without health insurance
recode hicov (2=1) (1=0), gen(no_insurance)
//hicov: 1= without health insurance 0 = with health insurance
svy:tab detailed_group no_insurance, row

*share with private health insurance
recode privcov (2=0), gen(pri_insurance)
//privcov 1=with private health insurance, 0=without private health insurance
svy:tab rac1p pri_insurance
svy:tab detailed_group pri_insurance, row

///age group
gen age_group = 1 if agep < 18
replace age_group = 2 if agep >=18 & agep <= 49
replace age_group = 3 if agep >=50 & agep <= 64
replace age_group = 4 if agep >=65

svy:tab detailed_group age_group, row


///immigration-related factors
recode nativity (2=1) (1=0), gen(foreign_born)
* % of pop that is foreign born (nativity 1=native 2=foreign)
svy:tab foreign_born
svy:tab foreign_born if racasn==1
svy:tab foreign_born if nhpi==1
svy:tab detailed_group foreign_born , row

* % of foreign born that is citizen
recode cit (1/4=1) (5=0), gen(citizen)
// cit (1/4 citizen; 5= not citizen)
svy:tab citizen if foreign_born==1
svy:tab citizen if foreign_born==1 & racasn==1
svy:tab citizen if foreign_born==1 & nhpi==1
svy:tab detailed_group citizen if foreign_born==1, row

* % of pop that is citizen
svy:tab citizen
svy:tab citizen if racasn==1
svy:tab citizen if nhpi==1
svy:tab detailed_group citizen , row

* % of pop that is citizen adult
gen cit_adult = 1 if citizen == 1 & agep > 17
replace cit_adult = 0 if citizen == 0 | agep < 18

svy:tab cit_adult
svy:tab cit_adult if racasn==1
svy:tab cit_adult if nhpi==1
svy:tab detailed_group cit_adult , row ci

* % of foreign born arrived after 2000
gen entry = 0
replace entry = 1 if yoep>=2000

svy:tab entry if foreign_born==1
svy:tab entry if foreign_born==1 & racasn==1
svy:tab entry if foreign_born==1 & nhpi==1
svy:tab detailed_group entry if foreign_born==1, row




cd "/Users/sunnyshao/Desktop"
estpost svy:tab detailed_group cit_adult , row
esttab using "cvap_detailed_2016_1YR.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs label plain

estpost svy:tab cit_adult if racasn==1
esttab using "cvap_asn.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs label plain

estpost svy:tab cit_adult if nhpi==1
esttab using "cvap_nhpi.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs label plain

estpost svy:tab cit_adult
esttab using "cvap_tot.csv", replace cells("b  ci_l ci_u") varlabels(`e(rowtitiles)') noobs label plain





///homeownership
egen max1 = max(rac2p), by(serialno)
*smallest detailed group code
egen min1 = min(rac2p), by(serialno)
*if only one detailed group exist, then diff == 0
gen diff1 = max1-min1
recode diff1 (0=1) (2/99=0) (.=0), gen(group_house)

egen max2 = max(racasn), by(serialno)
*smallest AA group code
egen min2 = min(racasn), by(serialno)
*if only one AA group exist, then diff == 0
gen diff2 = max2-min2
recode diff2 (0=1) (2/99=0) (.=0), gen(asn_house)

egen max3 = max(racasn), by(serialno)
*smallest NHPI group code
egen min3 = min(racasn), by(serialno)
*if only one NHPI group exist, then diff == 0
gen diff3 = max3-min3
recode diff3 (0=1) (2/99=0) (.=0), gen(nhpi_house)

///save individual-level file
keep group_house asn_house nhpi_house serialno rt sporder max1
save "/Users/sunnyshao/Dropbox/AAPIData HQ/Projects/ethnic_group_factsheets/update/income_homeownership/16_indiv_slim.dta"

use "/Users/sunnyshao/Dropbox/AAPIData HQ/Projects/ethnic_group_factsheets/update/income_homeownership/16_house_slim.dta", replace
merge 1:m serialno using "/Users/sunnyshao/Dropbox/AAPIData HQ/Projects/ethnic_group_factsheets/update/income_homeownership/16_indiv_slim.dta"
svyset[pw=wgtp]

recode max1 (1/37=.) (40=.) (47=.)  ///
(51/52 =.) (55=.) (58/59=.) (63/64=.) (66/68=.) ///
(38 = 1 "Indian") (39=2 "Bangladeshi") (41 = 3 "Burmese") ///
(42 = 4 "Cambodian") (43 = 5 "Chinese") (44=16 "Taiwanese")  ///
(45 = 7 "Filipino") (46 = 8 "Hmong") (48 = 9 "Japanese") ///
(49 = 10 "Korean") (50 = 11 "Laotian") (53 = 13 "Nepalese") ///
(54 = 14 "Pakistani") (56 = 17 "Thai")  (57 = 19 "Vietnamese") ///
 (60=12 "Native Hawaiian") (61=15 "Samoan") ///
(62=18 "Tongan") (65=6 "Fijian"), gen(detailed_group)


recode ten (1/2=1 "owned") (3=2 "rented") (4=3 "no payment"), gen(home)
svy:tab home
svy:tab home if asn_house==1
svy:tab home if nhpi_house==1
svy:tab detailed_group home if group_house==1, row





