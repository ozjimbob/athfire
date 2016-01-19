library(googlesheets)
library(dplyr)
library(vegan)

gs_auth()

fire_sheet = gs_title("Fire history for Supp Materials 5.xlsx")
ath_sheet = gs_title("Athrotaxis_.xlsx")
veg_sheet = gs_title("veg_conv")
veg_main = gs_read(veg_sheet,1)
fire_main = gs_read(fire_sheet,1)
ath_main = gs_read(ath_sheet,1)
ath_xtab = gs_read(ath_sheet,2)
ath_wha = gs_read(ath_sheet,3)
ath_tasveg = gs_read(ath_sheet,4)


ath_main = left_join(ath_main,ath_xtab)


ath_g = group_by(ath_main,Location)
ath_sumr = summarize(ath_g,ele=mean(Elevation),top=mean(top_range))
# Ordinate xtab table
fire_main = select(fire_main,Location,FKPencilWithin,FKillAnyAdjacent,FProtected,Expansion,NothoPresent)
ath_sumr2 = left_join(ath_sumr,fire_main)
ath_sumr <- ath_sumr2
comb_frame = left_join(ath_xtab,ath_sumr)

comb_frame = filter(comb_frame,!is.na(FKillAnyAdjacent))



ord_frame <- comb_frame[,3:66]
env_frame <- comb_frame[,67:73]
rownames(ord_frame) <- comb_frame$Location
rownames(env_frame) <- comb_frame$Location
colnames(ord_frame) <- substr(colnames(ord_frame),12,100)

#ord <- metaMDS(ord_frame)
#plot(ord,type="t")
#ord.fit <- envfit(ord~ele+top, data=ath_sumr, perm=999)
#plot(ord.fit)

uvlist = unique(veg_main$Group)
ord_frame2=matrix(0,nrow=dim(ord_frame)[1],ncol=length(uvlist))
colnames(ord_frame2)=uvlist
rownames(ord_frame2)=rownames(ord_frame)
for(i in seq_along(uvlist)){
  this_group = uvlist[i]
  sub_types = veg_main$Veg[veg_main$Group==this_group]
  sub_ord = ord_frame[,sub_types]
  sub_sum=apply(sub_ord,1,sum)
  ord_frame2[,this_group]=sub_sum
}

ord_frame<-ord_frame2

cca_ord <- cca(ord_frame ~ ele+top+FKPencilWithin+FProtected, data=env_frame)
plot(cca_ord,scaling=3)


head(summary(cca_ord),3)
anova(cca_ord)
anova(cca_ord,by="terms")
anova(cca_ord,by="axis")


# Variables to summarize;
# Elevation
# top_range
# fire_GE_log
