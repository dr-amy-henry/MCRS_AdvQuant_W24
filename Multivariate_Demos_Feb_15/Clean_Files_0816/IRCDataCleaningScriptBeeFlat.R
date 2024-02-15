# IRC Data analysis for R2R summer institute 2021
# Original version by Celia Symons created on June 28
############################################################
############################################################
######################### BEE FLAT##########################
rm(list=ls())
library(tidyverse)

#Dilys' working directory
setwd("C:/Users/Dilys Vela/Documents/UCI/SummerInstitute/DataSets/AllFiles")

# BeeFlat
Bee <-read.csv("ALLVEGBEEFLATcombineddataRobert.csv")#This file is the raw file that I 
# removed from the drive to reduce confusion.

names(Bee)
glimpse(Bee)
unique(Bee$Full.Scientific.Name)

# Habitat lumping as suggested by Robert
# Chaparral was lump with CSS, Oak woodland and Weedy were remove
# Perennial Grasslands did not change.
Bee %>% 
  count(Desired.Habitat)

Bee2<-Bee %>%
  mutate(habitat = fct_recode(Desired.Habitat,"CSS" = "Chaparral"))%>%
  filter(habitat!="Oak Woodland") %>%
  filter(habitat!="OaK Woodland") 


Bee2 %>% 
  count(habitat)

# Make Native and Non-Native codes standard
Bee2 %>% 
  count(Native.Non.Native)

Bee2<-Bee2 %>%
  mutate(Native.Non.Native = fct_recode(Native.Non.Native, 
                                        "Native"=" Native",
                                        "Non-native"=" Non-native",
                                        "Non-native"="Non-Native")) 

# This code standardize Data.Type codes and rename Data.Type
Bee2 %>% 
  count(Data.Type)

Bee2<-Bee2 %>%
  mutate(DataType = fct_recode(Data.Type, 
                               "T.Can"="T.CAN" ,
                               "A.Belt"="A.BELT",
                               "T.Height"="T.HEIGHT",
                               "T.GC"="T. GC")) 

Bee2 %>% 
  count(DataType)

#This code lumps the full and partial restoration areas as active
Bee2 %>% 
  count(Restoration.Type..FULL..PARTIAL..PASSIVE..CONTROL.)

Bee2<-Bee2 %>%
  mutate(restoration.type = fct_recode(Restoration.Type..FULL..PARTIAL..PASSIVE..CONTROL., 
                                       "active"="FULL",
                                       "active"="FULL " ,
                                       "active"= "PARTIAL",
                                       "active"="PARTIAL ",
                                       "passive"="PASSIVE")) 
Bee2 %>% 
  count(restoration.type)

# Filter out those codes that are in Species code that are not relevant

Bee2<-Bee2 %>%
  filter(Abiotc.Biotic!= "abiotic")%>%
  mutate(SpeciesCode = fct_recode(Species..Ground.Cover..Transect.Height..or.Total.Hits))%>%
  filter(SpeciesCode!= ".")%>%
  filter(SpeciesCode!= "UNKNOWN") %>%
  filter(SpeciesCode!= "NoAdditional")%>%
  filter(SpeciesCode!= "NR")%>%
  filter(SpeciesCode!= "NNG")%>%
  filter(SpeciesCode!= "NNF")%>%
  filter(!str_detect(SpeciesCode,'UNK'))%>% #filter out codes that start wit UNK
  rename(StageRestoration = Stage.of.Restoration..baseline..site.prep..planted..control.)%>%
  rename(ProjectPhase = Project.Phase..1..2..control.)%>%
  rename(PinNumb = Pin...1.50.)%>%
  rename(ScientificName = Full.Scientific.Name)                                                

head(Bee2)

# Select columns that will be use for clean file and analyses
Bee2<-Bee2 %>%
    select(c(Site,Year,Year_of_restoration,StageRestoration,ProjectPhase,                                    
    Slope.Aspect,Polygon.ID,Transect,PinNumb,Abiotc.Biotic,
    ScientificName,Native.Non.Native,Functional.Group,habitat,DataType,
    restoration.type,SpeciesCode))

head(Bee2)


sort(unique(Bee2$Polygon.ID))
##Species list and checking if species have more than one code associated to
##a full species name
Bee3<- Bee2 %>%
  select(c(SpeciesCode, ScientificName))%>%
  distinct() %>%
  arrange(SpeciesCode)%>%
  unique()

Bee3 %>% 
  count(ScientificName)%>%
  filter(n>1)

#Join Polygons and Transect to create an unique sample.
#I did not apply this code earlier because we need unique polygon IDs
#to calculate absolute covers
Bee2<-Bee2 %>% 
  unite('polygon.transect.ID',Polygon.ID:Transect, remove = FALSE)

#there is multiple information in the Functional.Group that should be separated
#one column for life form - annual/perennial
#one column for functional group - grass, forb, shrub
#one column for phenology - early, late, other
#creating individual columns based on functional groups column only
Bee2<-Bee2%>%
  mutate(FunGroup=ifelse(Functional.Group%in%c("Annual Forb","Annual Forb - Late Flowering","Annual Forb - Other", "Forb",
                                               "Perennial Forb - Late Flowering", "Perennial Forb - Other"), "Forb",
                         ifelse(Functional.Group%in%c("Grass","Perennial Grass - Other"), "Grass",
                                ifelse(Functional.Group%in%c("Large Shrub","Shrub - Late Flowering","Shrub - Other"), "Shrub",
                                       ifelse(Functional.Group=="Tree", "Tree", Functional.Group)))))%>%
  mutate(LifeForm=ifelse(Functional.Group%in%c("Annual Forb","Annual Forb - Late Flowering","Annual Forb - Other"), "Annual",
                         ifelse(Functional.Group%in%c("Perennial Grass - Other", "Perennial Forb - Other",
                                                      "Perennial Forb - Late Flowering","Tree", "Shrub - Other",
                                                      "Shrub - Late Flowering", "Large Shrub"), "Perennial", Functional.Group)))%>%
 mutate(Phenology=ifelse(Functional.Group%in%c("Perennial Grass - Other","Perennial Forb - Other","Annual Forb - Other", 
                                                "Shrub - Other"), "Other",
                       ifelse(Functional.Group%in%c("Perennial Forb - Late Flowering","Shrub - Late Flowering", "Annual Forb - Late Flowering"), "Late",Functional.Group)))%>%
 mutate(LifeForm = ifelse(LifeForm%in%c("Grass", "Forb"), "NA",LifeForm))%>%
 mutate(Phenology = ifelse(Phenology%in%c("Large Shrub","Tree","Grass","Forb", "Annual Forb"), "NA",Phenology))
         
#write.table(Bee2,"CleanBeeFlat08042021.csv",quote=F,row.names=F,sep=",",na="")

       
# You can use this code if you would like to have a metadata
# file for the plot
metadata <- Bee2%>%
select(c(polygon.transect.ID,Site,Year,Year_of_restoration ,
         restoration.type,ProjectPhase))%>%
  arrange(polygon.transect.ID,Year,Year_of_restoration )%>%
  unique()



