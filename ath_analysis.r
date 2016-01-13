library(googlesheets)
library(dplyr)
library(vegan)

g_auth(new_user=TRUE)

ath_sheet = gs_title("Athrotaxis_.xlsx")
ath_main = gs_read(ath_sheet,1)
ath_xtab = gs_read(ath_sheet,2)
ath_wha = gs_read(ath_sheet,3)
ath_tasveg = gs_read(ath_sheet,4)

ath_main = left_join(ath_main,ath_xtab)

# Ordinate xtab table

ord_frame <- ath_xtab [,3:20]
rownames(ord_frame) <- ath_xtab$Location
colnames(ord_frame) <- substr(colnames(ord_frame),12,100)

ord <- metaMDS(ord_frame)
plot(ord,type="t")
text(ord,display="sites")
# Variables to summarize;
# Elevation
# top_range
# fire_GE_log
