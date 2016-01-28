## Simplified rainforest ordination
library(googlesheets,quietly = TRUE,warn.conflicts = FALSE)
library(dplyr,quietly = TRUE,warn.conflicts = FALSE)
library(vegan,quietly = TRUE,warn.conflicts = FALSE)
library(grDevices)
library(sp)
library(maptools)
library(raster)

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


an = "Diselma"
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

# Get bioclim data
ath_loc = summarize(ath_g,lat=mean(Latitude),lon=mean(Longitude))
ath_loc2 = data.frame(ath_loc)
coordinates(ath_loc2)=c("lon","lat")

MATr = raster("E:\\geodata\\bio_30s_esri\\bio\\bio_1")
MAPr = raster("E:\\geodata\\bio_30s_esri\\bio\\bio_12")
MAXTr = raster("E:\\geodata\\bio_30s_esri\\bio\\bio_5")

MATv = extract(MATr,ath_loc2)/10
MAPv = extract(MAPr,ath_loc2)
MAXTv = extract(MAXTr,ath_loc2)/10

ath_loc$MAT = MATv
ath_loc$MAP = MAPv
ath_loc$MAXT = MAXTv

ath_loc$lat = NULL
ath_loc$lon = NULL
# Isolate the variables of interest from the fire effect sheet, join them to the location sheet

# Join this sheet to the x-tab surrounding vegetation sheet
comb_frame = left_join(ath_xtab,ath_sumr)
comb_frame = subset(comb_frame,!is.na(ele))
comb_frame = left_join(comb_frame,ath_loc)
# Remove any locations that are missing fire data


# Separate out into an ordination frame and an environmental frame, keep locations as row names
ord_frame <- comb_frame[,3:66]
env_frame <- comb_frame[,67:71]
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
rainforest <- ord_frame[,8]
#ord_frame <- ord_frame[,-8]

# Perform CCA (Constrained Correspondence Analysis) with all environmental variables
# on the species ordination frame
#cca_ord <- cca(ord_frame ~ ele+top+FKPencilWithin+FProtected+FKillAnyAdjacent+Expansion+NothoPresent, data=env_frame)
cca_ord <- cca(ord_frame ~ ele+top+MAT+MAP+MAXT, data=env_frame)


# Plot the ordination
palette(colorRampPalette(c("#FF0000","#00FF00"))(40))
plot(cca_ord,type="none",main=an,scaling=3)
text(cca_ord,col = rainforest+1,scaling=3)
points(cca_ord,display="bp",scaling=3)
text(cca_ord,display="bp",scaling=3)

plot(cca_ord,scaling=3,main=an)

#head(summary(cca_ord),3)

# Anova for CCA model as a whole
anova(cca_ord)

# Anova for individual environmental variable terms
anova(cca_ord,by="terms")

# Anova for the significance of ordination axes
anova(cca_ord,by="axis")
