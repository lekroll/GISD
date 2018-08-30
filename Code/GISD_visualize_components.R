#==================================================================================================
# Project: 
# Author: 
# Date:
#==================================================================================================

# Sources
#=======
# source("http://raw.githubusercontent.com/lekroll/R/master/files/r_install.R")
source("http://raw.githubusercontent.com/lekroll/R/master/files/r_rki_setup.R")

# Paths
#======
setwd("~/../Desktop/GISD/Data/Other/")


# Additional Libraries
#====================
require("ggalluvial")

# Load Data 
#===========


# Recode
#========
load("GISD_Komponents.RData")
GISD_Komponents$Variable <- as.character(GISD_Komponents$Variable)
GISD_Komponents$Variable[GISD_Komponents$Variable=="BeschaeftigtemitakadAbschluss"] <- "Beschaeftigte\nakadem. Abschluss"
GISD_Komponents$Variable[GISD_Komponents$Variable=="BeschaeftigteohneAbschluss"] <- "Beschaeftigte o. Abschluss"
GISD_Komponents$Variable[GISD_Komponents$Variable=="SchulabgaengerohneAbschluss"] <- "Schulabgänger\no. Abschluss"
GISD_Komponents$Variable[GISD_Komponents$Variable=="SchulabgaengerohneAbschluss"] <- "Schulabgänger\no. Abschluss"
GISD_Komponents$Variable[GISD_Komponents$Variable=="SchulabgaengerohneAbschluss"] <- "Schulabgänger\no. Abschluss"
GISD_Komponents$Variable[GISD_Komponents$Variable=="Haushaltseinkommen"] <- "Haushalts-\neinkommen"
GISD_Komponents$Variable[GISD_Komponents$Variable=="Schuldnerquote"] <- "Schuldner-\nquote"
GISD_Komponents$Variable[GISD_Komponents$Variable=="Beschäftigtenquote"] <- "Beschäftigten-\nquote"
# Analysis
#==========
ggplot(GISD_Komponents, aes(weight = Anteil, axis1 = GISD, axis2 = Dimension, axis3=Variable)) + 
  geom_alluvium(aes(fill = Richtung), width = 1/9, alpha=.6,colour="White") +
  geom_stratum(alpha=1, width = 1/9, colour="White", fill=rki$rkihc1) +
  geom_text(stat = "stratum", colour="White", label.strata = TRUE, family=rki$rkifont_bold, size=rel(1.8)) + 
  scale_fill_manual(values=c(rki$rkihc7,rki$rkihc11)) + rki$rkitheme_void +
  theme(legend.position = "bottom", legend.title = element_blank()) + coord_flip()
ggsave(file="Alluval_RII.png", width=8 , height=6, dpi=300)

ggplot(GISD_Komponents, aes(weight = Anteil, axis1 = GISD, axis2 = Dimension, axis3=Variable)) + 
  geom_alluvium(aes(fill = Richtung), width = 1/9, alpha=.6,colour="White") +
  geom_stratum(alpha=1, width = 1/3, colour="White", fill=rki$rkihc1) +
  geom_text(stat = "stratum", colour="White", label.strata = TRUE, family=rki$rkifont_bold, size=rel(1.8)) + 
  scale_fill_manual(values=c(rki$rkihc7,rki$rkihc11)) + rki$rkitheme_void +
  theme(legend.position = "bottom", legend.title = element_blank()) 
ggsave(file="Alluval_RII_noflip.png", width=8 , height=6, dpi=300)
