#this file makes some additional data cleaning to the long format clean BeeFlat data file
#it also creates the plot metadata file and makes a wide data set of that has total species hits within species column

library(readr)
library(tidyverse)

BeeFlat <- read_csv("C:/Users/llari/Dropbox/R2R Summer Institute/BeeFlat_Clean/CleanBeeFlat08062021.csv")

BeeFlat$ProjectPhase[is.na(BeeFlat$ProjectPhase)==T]<-"CONTROL"

#subsetting Data to just belt and transect hits
BeeFlat2=BeeFlat%>%
  filter(DataType%in%c("T.PI", "A.Belt"))%>%
  mutate(count=1)

#also some plant data columns have inconsistencies so dropping them this round
BeeFlat3<-BeeFlat2%>%
  select(-c("Abiotc.Biotic","ScientificName","Native.Non.Native", "Functional.Group"))

#some species code were recorded as unknown in the field but later categorized to a species ID
#LOLMUL == FESPER


#need to sum observations by species
#slope isn't always entered so need to drop and remerge in
BeeFlat3b<-BeeFlat3%>%
  mutate(SpeciesCode = recode(SpeciesCode,"LOLMUL"="FESPER"))%>%
  group_by(Site, Year, Year_of_restoration, StageRestoration, ProjectPhase,  
           polygon.transect.ID,Polygon.ID, Transect, habitat, restoration.type, SpeciesCode)%>%
  summarize(Count2=sum(count, na.rm=T))%>%
  pivot_wider(names_from = SpeciesCode, values_from = Count2,values_fill = 0 )%>%
  mutate(ProjectPhase=ifelse(ProjectPhase==1, "P1", ifelse(ProjectPhase==2, "P2", ProjectPhase)))
 

#need to drop the weedy control 
BeeFlat3b<-BeeFlat3b[-which(BeeFlat3b$Polygon.ID=='Weedy Control'),] 

#and will create problems for the plot ID
BeeFlat3b$Plot.key=paste(BeeFlat3b$ProjectPhase,BeeFlat3b$Year,BeeFlat3b$Polygon.ID,
                         BeeFlat3b$Transect,BeeFlat3b$habitat, BeeFlat3b$restoration.type, sep="_")

#trying to create plot metadata file
BeeFlat_plot<-BeeFlat3b%>%
  select(Plot.key,Site, Year, Year_of_restoration, StageRestoration, ProjectPhase,  
         polygon.transect.ID,Polygon.ID, Transect, habitat, restoration.type)%>%
  unique()%>%
  mutate(Year=as.factor(Year))%>%
  mutate(Year_of_restoration=as.factor(Year_of_restoration))

#working to create a clean slope file
aspect<-BeeFlat3%>%
  select(polygon.transect.ID, Slope.Aspect)%>%
  unique()%>%
  na.omit()

aspect<-aspect[-which(aspect$polygon.transect.ID=="Control SW_1"& aspect$Slope.Aspect=="SE"),]
aspect<-aspect[-which(aspect$polygon.transect.ID=="Control SE_1"& aspect$Slope.Aspect=="SW"),]

#merge plot info and aspect into a single metadata file
plot_metadata<-BeeFlat_plot%>%
  left_join(aspect, by="polygon.transect.ID")

#a couple other NAs were present
plot_metadata$StageRestoration[is.na(plot_metadata$StageRestoration)==T]<-"control"
plot_metadata$StageRestoration[plot_metadata$ProjectPhase=="CONTROL"&plot_metadata$StageRestoration=="maintenance"]<-"control"

#is there a perennial grassland control?

write.csv(plot_metadata, file="BeeFlat_plotmetadata.csv", row.names=F)


#try to remove columns from species cover data file
#Plot.key can be used to merge data between wide data set and plot metadata

new.dat=BeeFlat3b%>%
  ungroup()%>%
  select(c(Plot.key, AVESPP:LUPALB))

write.csv(new.dat, "CleanBeeFlat08062021_wide.csv", row.names=F)
