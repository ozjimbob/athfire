library(googlesheets)
library(dplyr)

g_auth(new_user=TRUE)

ath_sheet=gs_title("Athrotaxis_.xlsx")
ath_main = gs_read(ath_sheet,1)
ath_xtab = gs_read(ath_sheet,2)
ath_wha = gs_read(ath_sheet,3)
ath_tasveg = gs_read(ath_sheet,4)

ath_main = left_join(ath_main,ath_xtab)
