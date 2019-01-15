#==================================================================================================
# Project: 
# Author: 
# Date:
#==================================================================================================

# Libraries
#==========
require("tidyverse")
require("ggalluvial")

# Load Data 
#===========


# Recode
#========
load("Revisions/Other/GISD_Komponents_2018.RData")
# Analysis
#==========
ggplot(GISD_Komponents, aes(weight = Anteil, axis1 = GISD, axis2 = Dimension, axis3=Variable)) + 
  geom_alluvium(aes(fill = Richtung), width = 1/3, alpha=.6,colour="White") +
  geom_stratum(alpha=.7, width = 1/2) +
  geom_text(stat = "stratum",  label.strata = TRUE, size=rel(2.5)) +
  theme_void()  + theme(legend.position = "bottom") 
ggsave(file="Revisions/2018/Other/Alluval_RII.png", width=20 , height=14.5, dpi=300, unit="cm")
