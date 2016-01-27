library(googlesheets,quietly = TRUE,warn.conflicts = FALSE)
library(dplyr,quietly = TRUE,warn.conflicts = FALSE)
library(vegan,quietly = TRUE,warn.conflicts = FALSE)

# Authenticate with Google Drive
gs_auth()

# Sheets stored on Google Drive, access via ID keys
#fire_sheet = gs_title("Fire history for Supp Materials 5.xlsx")
fire_sheet = gs_key("1_Se1zQTIrSR5PmcHmuJRn7l2rUajxAKluAtkU6aSaGs")
#ath_sheet = gs_title("Athrotaxis_.xlsx")
ath_sheet = gs_key("1wCzmVXWudZzBYFadAArSUVm-hY_xhsfQEw8cqkwMZYk")
#dis_sheet = gs_title("Diselma_.xlsx")
dis_sheet = gs_key("1OAmcVGgRFji-xZi8DVICbklEL-5Ya7Oi2k2TCZQvzbg")
#veg_sheet = gs_title("veg_conv")
veg_sheet = gs_key("17K2yN_AZkq6lvfKD4COOIqCse2Ky25unu4CZNz6GXZY")

# Read google drive sheets into data frames
veg_main = gs_read(veg_sheet,1)
fire_main = gs_read(fire_sheet,1)
ath_main = gs_read(ath_sheet,1)
ath_xtab = gs_read(ath_sheet,2)
dis_main = gs_read(dis_sheet,1)
dis_xtab = gs_read(dis_sheet,2)
ath_wha = gs_read(ath_sheet,3)
ath_tasveg = gs_read(ath_sheet,4)


an = "Athrotaxis"
#an = "Diselma"

# Perform analysis function
if(an=="Athrotaxis"){
  print("Athrotaxis Analysis")
  ath_main = left_join(ath_main,ath_xtab)
}else{
  print("Diselma Analysis")
  ath_main = left_join(dis_main,dis_xtab)
}
# Group the data by location and summarize topographic variables by mean per location
ath_g = group_by(ath_main,Location)
ath_sumr = summarize(ath_g,ele=mean(Elevation),top=mean(top_range))

# Isolate the variables of interest from the fire effect sheet, join them to the location sheet
fire_main = select(fire_main,Location,FKPencilWithin,FKillAnyAdjacent,FProtected,Expansion,NothoPresent)
ath_sumr2 = left_join(ath_sumr,fire_main)
ath_sumr <- ath_sumr2

# Join this sheet to the x-tab surrounding vegetation sheet
comb_frame = left_join(ath_xtab,ath_sumr)

# Remove any locations that are missing fire data
comb_frame = filter(comb_frame,!is.na(FKillAnyAdjacent))

# Separate out into an ordination frame and an environmental frame, keep locations as row names
ord_frame <- comb_frame[,3:66]
env_frame <- comb_frame[,67:73]
rownames(ord_frame) <- comb_frame$Location
rownames(env_frame) <- comb_frame$Location

# Simplify the vegetation column names
colnames(ord_frame) <- substr(colnames(ord_frame),12,100)

# Use the veg_conv sheet to simplify vegetation categories
# Look up each unique vegetation group, assign the narrow types to each group
# In a new table
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

# Simplify the NothoPresent variable to binary
env_frame$NothoPresent[env_frame$NothoPresent==0.5]=1

# Convert the environmental variables to factors
env_frame$FProtected = factor(env_frame$FProtected,levels=c(0,1),labels=c("-","+"))
env_frame$FKPencilWithin = factor(env_frame$FKPencilWithin,levels=c(0,1),c("-","+"))
env_frame$FKillAnyAdjacent = factor(env_frame$FKillAnyAdjacent,levels=c(0,1),c("-","+"))
env_frame$Expansion = factor(env_frame$Expansion,levels=c(0,1),c("-","+"))
env_frame$NothoPresent = factor(env_frame$NothoPresent,levels=c(0,1),c("-","+"))

# Perform CCA (Constrained Correspondence Analysis) with all environmental variables
# on the species ordination frame
cca_ord <- cca(ord_frame ~ ele+top+FKPencilWithin+FProtected+FKillAnyAdjacent+Expansion+NothoPresent, data=env_frame)

# Plot the ordination
plot(cca_ord,scaling=3,main=an)

#head(summary(cca_ord),3)

# Anova for CCA model as a whole
anova(cca_ord)

# Anova for individual environmental variable terms
anova(cca_ord,by="terms")

# Anova for the significance of ordination axes
anova(cca_ord,by="axis")

