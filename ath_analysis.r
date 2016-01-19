library(googlesheets)
library(dplyr)
library(vegan)

gs_auth(new_user=TRUE)

ath_sheet = gs_title("Athrotaxis_.xlsx")
ath_main = gs_read(ath_sheet,1)
ath_xtab = gs_read(ath_sheet,2)
ath_wha = gs_read(ath_sheet,3)
ath_tasveg = gs_read(ath_sheet,4)

ath_main = left_join(ath_main,ath_xtab)

ath_g = group_by(ath_main,Location)
ath_sumr = summarize(ath_g,ele=mean(Elevation),top=mean(top_range))
# Ordinate xtab table

ord_frame <- ath_xtab [,3:20]
rownames(ord_frame) <- ath_xtab$Location
colnames(ord_frame) <- substr(colnames(ord_frame),12,100)

ord <- metaMDS(ord_frame)
plot(ord,type="t")
ord.fit <- envfit(ord~ele+top, data=ath_sumr, perm=999)
plot(ord.fit)


cca_ord <- cca(ord_frame ~ ele+top, data=ath_sumr)
plot(cca_ord,scaling=3)

head(summary(cca_ord),3)
anova(cca_ord)
anova(cca_ord,by="terms")
anova(cca_ord,by="axis")


# Variables to summarize;
# Elevation
# top_range
# fire_GE_log
