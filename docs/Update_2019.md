---
layout: default
title: Info on GISD Revision 2019
---
[<back](index)

GISD was updated for 2018 as of 21.01.2019. The Reference Date for all Regional Definitions ("Gebietsstand") is 31.12.2015. The update includes data for Years 1998 to 2015. The Reference Paper can be accessed [here](http://edoc.rki.de/docviews/abstract.php?lang=ger&id=5130).

## Changes
* Minor code changes in comparison to Revision 2018.
* Removal of the annual csv in favor of a long-format csv. So filtering is needed but number of files greately reduced.

## Contents
* **Bund** GISD for levels Gemeinde (values are based on Gemeindeverband), Gemeindeverband, Kreis, Raumordnungsregion, NUTS2, PLZ (German Zipcodes)
* **Bundesland** GISD for levels Gemeindeverband and Kreis for all States except Berlin, Bremen and Hamburg with Quantiles referencing inner state disparities.
* **Files** We provide tables of comma seperated by regional level as well as a Stata _.dta_-file in long format with all years included for each level.

## Variables
  * **GISD_Score** 
    Raw Scores of GISD normalized to range 0 to 1 for each level with Reference to Germany (Bund) or to the corresponding state (Bundesland) for each Year (Jahr)
  * **GISD_5**
    Quintiles of GISD with Reference to Germany (Bund) or to the corresponding state (Bundesland) for each Year (Jahr)
  * **GISD_10**
    Deciles of GISD with Reference to Germany (Bund) or to the corresponding state (Bundesland) for each Year (Jahr)
  * **GISD_k**
   Groups based on the Quintiles of GISD (1= 1th Quintile, 2= 2th to 4th Quintile, 3= 5th Quintile) with Reference to Germany (Bund) or to the corresponding state (Bundesland) for each Year (Jahr)

# Download
[Files of the 2019 Update](https://github.com/lekroll/GISD/tree/master/Revisions/2019)
