
# Sozioökonomische Deprivation als Prädiktor für das Steueraufkommen der Gemeinden zwischen 1998 und 2014
## Ziel
Mit diesem Notebook wird analysiert, inwiefern sich sozioökonomische Deprivation als Prädiktor für das Steueraufkommen der Gemeinden eignet. Um die [Nachvollziehbarkeit](http://ropensci.github.io/reproducibility-guide/sections/introduction/) der Einschätzung zur verbessern das Ergebnis als Notebook bereitgestellt.

## Statistische Analysen
Die Analysen werden mit dem freien Programmpaket [R](https://www.r-project.org/) durchgeführt. Alle Schritte werden erläutert.


```R
# Working Directory
setwd("~/Notebooks")

# Libraries provided for reproducability
library("tidyverse",quiet=T)
library("haven")
library("viridis")

# Options for Graphic Output
options(repr.plot.width=6, repr.plot.height=3.375)
```

# Datenquellen
## Gemeindefinanzen: INKAR
Die verwendeten Daten zur Gemeindefinanzierung stammen aus der [INKAR-Online Datenbank](http://www.inkar.de) des Bundesinstitut für Bau-, Stadt- und Raumforschung (BBSR) im Bundesamt für Bauwesen und Raumordnung (BBR). Sie sind frei zugänglich und können auf Ebene der Gemeindeverbände für den Zeitraum 1995 bis 2015 *(Stand: 5.6.2018)* bezogen werden.


```R
# Finanzen
options(warn=-1)
Gemeindefinanzen <- suppressMessages(read_delim("data/Gemeindefinanzen_1995_2015.csv",";", escape_double = FALSE, locale = locale(date_names = "de",
decimal_mark = ","), trim_ws = TRUE, skip = 1))
Gemeindefinanzen <- Gemeindefinanzen %>%  rename(Kennziffer_Gemeindeverband=X1,Name=X2,Typ=X3) %>% gather(Jahr,Steuereinnahmen_pro_EW,4:24) %>% 
  mutate(Jahr=as.numeric(Jahr), Kennziffer_Gemeindeverband=as.numeric(Kennziffer_Gemeindeverband))
options(warn=0)
head(Gemeindefinanzen)
```


<table>
<thead><tr><th scope=col>Kennziffer_Gemeindeverband</th><th scope=col>Name</th><th scope=col>Typ</th><th scope=col>Jahr</th><th scope=col>Steuereinnahmen_pro_EW</th></tr></thead>
<tbody>
	<tr><td>1001000               </td><td>Flensburg, Stadt      </td><td>Gemeinde              </td><td>1995                  </td><td>469.0                 </td></tr>
	<tr><td>1002000               </td><td>Kiel, Landeshauptstadt</td><td>Gemeinde              </td><td>1995                  </td><td>476.2                 </td></tr>
	<tr><td>1003000               </td><td>Lübeck, Hansestadt    </td><td>Gemeinde              </td><td>1995                  </td><td>471.9                 </td></tr>
	<tr><td>1004000               </td><td>Neumünster, Stadt     </td><td>Gemeinde              </td><td>1995                  </td><td>447.9                 </td></tr>
	<tr><td>1051011               </td><td>Brunsbüttel, Stadt    </td><td>Gemeinde              </td><td>1995                  </td><td>606.5                 </td></tr>
	<tr><td>1051044               </td><td>Heide, Stadt          </td><td>Gemeinde              </td><td>1995                  </td><td>467.3                 </td></tr>
</tbody>
</table>



## Sozioökonomische Deprivation: German Index auf Deprivation (GISD)
Für Deutschland sind regionale Unterschiede für verschiedene Gesundheitsindikatoren dokumentiert, die auch mit sozioökonomischen Unterschieden assoziiert sind. Der „German Index of Socioeconomic Deprivation“ (GISD) wird auf Ebene der Gemeindeverbände, der Landkreise und kreisfreien Städte sowie der Regierungsbezirke ab 1998 generiert. Er ist mit Aggregatdaten sowie Individualdaten der Studie Gesundheit in Deutschland assoziiert und wird für die Nutzung in der Forschung und Gesundheitsberichterstattung des Bundes und der Länder bereitgestellt. Er soll dazu beitragen, neue Datenquellen für die Analyse des Zusammenhangs von sozialer Ungleichheit und Gesundheit zu erschließen. Der Index kann [hier heruntergeladen werden](http://lekroll.github.io/GISD), nähere Informationen zur Indexkonstruktion sind [hier](https://edoc.rki.de/handle/176904/2648) verfügbar.


```R
# GISD in Stata long format dataset
download.file("https://github.com/lekroll/GISD/blob/master/Revisions/2018/Bund/Gemeindeverband/Gemeindeverband_long.dta?raw=true",
              "data/Gemeindeverband_long.dta",mode = "wb")
GISD <- read_dta("data/Gemeindeverband_long.dta") %>%
        mutate(Kennziffer_Gemeindeverband= as.numeric(as.character(Kennziffer_Gemeindeverband)), Jahr=as.numeric(as.character(Jahr)))
head(GISD)
```


<table>
<thead><tr><th scope=col>Kennziffer_Gemeindeverband</th><th scope=col>Name_Gemeindeverband</th><th scope=col>Jahr</th><th scope=col>Bevoelkerung</th><th scope=col>GISD_Score</th><th scope=col>GISD_5</th><th scope=col>GISD_10</th><th scope=col>GISD_k</th></tr></thead>
<tbody>
	<tr><td>1001000         </td><td>Flensburg, Stadt</td><td>1998            </td><td>84694           </td><td>0.856489        </td><td>5               </td><td>10              </td><td>3               </td></tr>
	<tr><td>1001000         </td><td>Flensburg, Stadt</td><td>1999            </td><td>84694           </td><td>0.834273        </td><td>5               </td><td>10              </td><td>3               </td></tr>
	<tr><td>1001000         </td><td>Flensburg, Stadt</td><td>2000            </td><td>84694           </td><td>0.764017        </td><td>5               </td><td> 9              </td><td>3               </td></tr>
	<tr><td>1001000         </td><td>Flensburg, Stadt</td><td>2001            </td><td>84694           </td><td>0.713020        </td><td>5               </td><td> 9              </td><td>3               </td></tr>
	<tr><td>1001000         </td><td>Flensburg, Stadt</td><td>2002            </td><td>84694           </td><td>0.769248        </td><td>5               </td><td> 9              </td><td>3               </td></tr>
	<tr><td>1001000         </td><td>Flensburg, Stadt</td><td>2003            </td><td>84694           </td><td>0.742066        </td><td>5               </td><td> 9              </td><td>3               </td></tr>
</tbody>
</table>



## Zusammenspielen der Datensätze
Die Daten von INKAR lassen sich mit dem GISD anhand der Zugehörigen Regionalkennziffer zusammenführen. Hierfür müssen die Bezeichnung der Schlüsselvariable in beiden Datensätzen und der Datentyp der Schlüsselvariable (numerisch oder Zeichen) ggf. vereinheitlicht werden.


```R
Comparison <- left_join(Gemeindefinanzen,GISD,by=c("Kennziffer_Gemeindeverband","Jahr")) %>% filter(!is.na(GISD_Score))
Comparison <- Comparison %>% filter(!is.na(GISD_Score) & !is.na(Steuereinnahmen_pro_EW))
head(Comparison)
```


<table>
<thead><tr><th scope=col>Kennziffer_Gemeindeverband</th><th scope=col>Name</th><th scope=col>Typ</th><th scope=col>Jahr</th><th scope=col>Steuereinnahmen_pro_EW</th><th scope=col>Name_Gemeindeverband</th><th scope=col>Bevoelkerung</th><th scope=col>GISD_Score</th><th scope=col>GISD_5</th><th scope=col>GISD_10</th><th scope=col>GISD_k</th></tr></thead>
<tbody>
	<tr><td>1001000               </td><td>Flensburg, Stadt      </td><td>Gemeinde              </td><td>1998                  </td><td> 524.7                </td><td>Flensburg, Stadt      </td><td> 84694                </td><td>0.856489              </td><td>5                     </td><td>10                    </td><td>3                     </td></tr>
	<tr><td>1002000               </td><td>Kiel, Landeshauptstadt</td><td>Gemeinde              </td><td>1998                  </td><td> 588.0                </td><td>Kiel, Landeshauptstadt</td><td>243148                </td><td>0.729595              </td><td>5                     </td><td> 9                    </td><td>3                     </td></tr>
	<tr><td>1003000               </td><td>Lübeck, Hansestadt    </td><td>Gemeinde              </td><td>1998                  </td><td> 473.5                </td><td>Lübeck, Hansestadt    </td><td>214420                </td><td>0.845004              </td><td>5                     </td><td>10                    </td><td>3                     </td></tr>
	<tr><td>1004000               </td><td>Neumünster, Stadt     </td><td>Gemeinde              </td><td>1998                  </td><td> 495.6                </td><td>Neumünster, Stadt     </td><td> 77588                </td><td>0.940591              </td><td>5                     </td><td>10                    </td><td>3                     </td></tr>
	<tr><td>1051011               </td><td>Brunsbüttel, Stadt    </td><td>Gemeinde              </td><td>1998                  </td><td>1049.7                </td><td>Brunsbüttel, Stadt    </td><td> 12642                </td><td>0.790742              </td><td>5                     </td><td> 9                    </td><td>3                     </td></tr>
	<tr><td>1051044               </td><td>Heide, Stadt          </td><td>Gemeinde              </td><td>1998                  </td><td> 545.1                </td><td>Heide, Stadt          </td><td> 21303                </td><td>0.833469              </td><td>5                     </td><td>10                    </td><td>3                     </td></tr>
</tbody>
</table>



# Vergleich von GISD und Gemeindefinanzen
Im Folgenden erfolgt zuerst ein graphischer und anschließend ein regressionsbasierter Vergleich.
### Graphisch


```R
# Comparison 
ggplot(Comparison,aes(x=GISD_Score,y=Steuereinnahmen_pro_EW, group=Jahr, colour=Jahr)) + 
geom_smooth(method="gam") + scale_colour_viridis() + theme_minimal() +
labs(title="Einnahmen pro Einw. (EUR)", y="", x="Regionale Deprivation")
```




![png](output_9_1.png)


## Korrelation


```R
ggplot(Comparison %>% group_by(Jahr) %>% summarise(Korrelation = cor(Steuereinnahmen_pro_EW,GISD_Score)),
       aes(x=Jahr,y=Korrelation, colour=Jahr)) + geom_smooth(method="loess",se=F,colour="grey80") +
geom_point()+ scale_colour_viridis() + scale_y_continuous(limits=c(-1,0)) + theme_minimal() +
labs(y="",title="Korrelationskoeffizient \n nach Pearson r=")
```




![png](output_11_1.png)


# Anpassungsgüte


```R
ggplot(Comparison %>% group_by(Jahr) %>% summarise(Varianzaufklärung = (cor(Steuereinnahmen_pro_EW,GISD_Score)^2)*100),
       aes(x=Jahr,y=Varianzaufklärung, colour=Jahr)) +
geom_smooth(method="loess",se=F,colour="grey80") + labs(y="", title="Varianzaufklärung in %") +
geom_point()+ scale_colour_viridis() + theme_minimal() + scale_y_continuous(limits=c(0,100))
```




![png](output_13_1.png)


# Fazit
Zwischen den Steuereinnahmen der Gemeindeverbände und der über den GISD approximierten Sozialstruktur der Gemeinden besteht ein starker Zusammenhang, allerdings ist die Varianzaufklärung der Gemeindefinanzen durch den GISD nur mäßig. Gründe dafür dass GISD die Steuereinnahmen nicht besser erklärt sind etwa:
* **Konjunktureffekte** 

    Das Steueraufkommen varriert in Abhängigkeit von der konjunkturellen Lage. Tendenziell ist anzunehmen, dass die Bedeutung der Einkommenssteuer und damit der Anteil der Löhne am gesamten Steueraufkommen in Phasen der Rezession höher ist als in konjunkturellen Hochphasen, in denen auch eine stärkere Bautätigkeit Treiber der Einnahmen und höhere Gewerbeeinnahmen vorliegen. 
    
    
* **Komponenten der Gemeindefinanzen** 

    Die Steuereinnahmen der Gemeinden speisen sich aus folgenden Quellen: Grundsteuer A und B + Gewerbesteuer + Einkommensteuer + Umsatzsteuer + sonstige Gemeindesteuer (Vergnügungssteuer, Hundesteuer, Getränkesteuer usw.) - Gewerbesteuerumlage. Lediglich die Einkommenssteuer ist dabei direkt mit der Sozialstruktur assoziiert und geht auch in die Konstruktion des GISD ein. Die übrigen Steuerarten weisen dagegen nur indirekte Verbindungen zur Sozialstruktur auf.


```R

```
