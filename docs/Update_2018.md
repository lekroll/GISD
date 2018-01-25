# Info on GISD Revision 2018
This directory contains the updated GISD for 2018 as of 25.01.2018. The Reference Date for all Regional Definitions ("Gebietsstand") is 31.12.2014.

## Changes
* We changed the definition of the Educational Component to be also based on factor analysis. Now, it is defined by the following z-standardized proportions: 
  * *School leavers with German "Abitur" or equivalent*
  * *School leavers without a degree*
  * *Employees at place of residence with university degree*
  * *Employees at place of residence with university degree*
* The whole code was rewritten in R to faciliate future updates.
* We now provide annual data.
* We now provide Quantiles with Reference to Germany as a whole (*Bund*) as well as with Reference to the German States (*Bundesländer*)
* Postcodes habe been removed, but may be added in a future update.

## Contents
* **Bund** GISD for levels Gemeinde (values are based on Gemeindeverband), Gemeindeverband, Kreis, Raumordnungsregion, NUTS2
* **Bundesland** GISD for levels Gemeindeverband and Kreis for all States except Berlin, Bremen and Hamburg with Quantiles referencing inner state disparities.
* **Files** We provide tables of comma seperated by regional level for each year as well as a Stata _.dta_-file in long format with all years included for each level.

## Variables
  * **GISD_Score** 
    Raw Scores of GISD normalized for each Year (Jahr) 
  * **GISD_5**
    Quintiles of GISD with Reference to Germany (Bund) or to the corresponding state (Bundesland) for each Year (Jahr)
  * **GISD_10**
    Deciles of GISD with Reference to Germany (Bund) or to the corresponding state (Bundesland) for each Year (Jahr)
  * **GISD_k**
   Groups based on the Quintiles of GISD (1= 1th Quintile, 2= 2th to 4th Quintile, 3= 5th Quintile) with Reference to Germany (Bund) or to the corresponding state (Bundesland) for each Year (Jahr)

# Download
[Link to the files of the 2018 Update](https://github.com/lekroll/GISD/tree/master/Revisions/2018)
