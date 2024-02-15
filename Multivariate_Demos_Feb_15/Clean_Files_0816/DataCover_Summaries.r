##########################
#### Calculate species cover including based on occurrence in Belt and 
#### transect data
##########################

## Calculate the total species cover for the site
# Native species
AbsCoverNat <- Bee2 %>%
  filter(Native.Non.Native =="Native" & DataType =="T.PI") %>%
  group_by(Polygon.ID)%>% # Groups by polygon ID
  count(SpeciesCode)%>% #Count occurences per Polygon
  mutate(AbsCoverNonNatPer = n/50 * 100) #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 

# Native species based on year to report
# remember that 0 corresponds to initial year and subsequent numbers
# correspond to progression on the restoration, with 5 being the year 
# a restoration is finalized. Also Control sites have calendar years for
# this column

## Absolute cover of natives for the beginning of restoration (e.g.
## 0 year.for.report),Active, Native, T.PI
# Note: if you want to do these with controls, then you need to use calendar years
AbsCoverNatZero <- Bee2 %>%
  filter(Native.Non.Native =="Native" & restoration.type == "active" & DataType=="T.PI" & Year_of_restoration == 0) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNat = round(sppnumb/50 * 100))%>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration= 0) 

## Absolute cover of natives for the beginning of restoration (e.g.
## 1 year.for.report)
AbsCoverNatFirst <- Bee2 %>%
  filter(Native.Non.Native =="Native" & restoration.type == "active" & DataType=="T.PI" & Year_of_restoration == 1) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNat = round(sppnumb/50 * 100))%>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration = 1) 

## Absolute cover of natives for the beginning of restoration (e.g.
## 3 year.for.report)
AbsCoverNatTrd <- Bee2 %>%
  filter(Native.Non.Native =="Native"  & restoration.type == "active" & DataType=="T.PI" & Year_of_restoration == 3) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNat = round(sppnumb/50 * 100)) %>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration = 3)

## Absolute cover of natives for the beginning of restoration (e.g.
## 5 year.for.report)
AbsCoverNatFive <- Bee2 %>%
  filter(Native.Non.Native =="Native"  & restoration.type == "active" & DataType=="T.PI" & Year_of_restoration == 5) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNat = round(sppnumb/50 * 100))%>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration= 5)

## Absolute cover of natives for the beginning of restoration (e.g.
## 7 year.for.report)
AbsCoverNatSeven <- Bee2 %>%
  filter(Native.Non.Native =="Native"  & restoration.type == "active" & DataType=="T.PI" & Year_of_restoration == 7) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNat = round(sppnumb/50 * 100))%>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration = 7)

AbsCoverNat<- bind_rows(AbsCoverNatZero,AbsCoverNatFirst,AbsCoverNatTrd,
                        AbsCoverNatFive,AbsCoverNatSeven) 

#write.table(AbsCoverNat,"AbsCoverNatBee.csv",quote=F,row.names=F,sep=",",na="")


## Absolute cover of natives for the beginning of restoration (e.g.
## 0 year.for.report)
AbsCoverNonNatZero <- Bee2 %>%
  filter(Native.Non.Native =="Non-native" & restoration.type == "active" & DataType=="T.PI" & Year_of_restoration== 0) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNonNat = round(sppnumb/50 * 100)) %>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration = 0) 

## Absolute cover of natives for the beginning of restoration (e.g.
## 1 year.for.report)
AbsCoverNonNatFirst <- Bee2 %>%
  filter(Native.Non.Native =="Non-native" & restoration.type == "active" & DataType=="T.PI" & Year_of_restoration == 1) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNonNat = round(sppnumb/50 * 100))%>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration = 1) 

## Absolute cover of natives for the beginning of restoration (e.g.
## 3 year.for.report)
AbsCoverNonNatTrd <- Bee2 %>%
  filter(Native.Non.Native =="Non-native" & restoration.type == "active" & DataType=="T.PI" & Year_of_restoration == 3) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNonNat = round(sppnumb/50 * 100)) %>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration = 3)

## Absolute cover of natives for the beginning of restoration (e.g.
## 5 year.for.report)
AbsCoverNonNatFive <- Bee2 %>%
  filter(Native.Non.Native =="Non-native"  & restoration.type == "active" & DataType=="T.PI" & Year_of_restoration == 5) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNonNat = round(sppnumb/50 * 100)) %>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration = 5)

## Absolute cover of natives for the beginning of restoration (e.g.
## 7 year.for.report)
AbsCoverNonNatSeven <- Bee2 %>%
  filter(Native.Non.Native =="Non-native"  & restoration.type == "active" & DataType=="T.PI" & Year_of_restoration == 7) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNonNat = round(sppnumb/50 * 100)) %>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration = 7)

AbsCoverNonNat<- bind_rows(AbsCoverNonNatZero,AbsCoverNonNatFirst,
                           AbsCoverNonNatTrd,AbsCoverNonNatFive,AbsCoverNonNatSeven) 

#write.table(AbsCoverNonNat,"AbsCoverNonNatBeeFlat.csv",quote=F,row.names=F,sep=",",na="")

###PASSIVE RESTORATION
## Absolute cover of natives for the beginning of restoration (e.g.
## 0 year.for.report),Passive, Native, T.PI
# Note: if you want to do these with controls, then you need to use calendar years
AbsCoverNatZero <- Bee2 %>%
  filter(Native.Non.Native =="Native" & restoration.type == "passive" & DataType=="T.PI" & Year_of_restoration == 0) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNat = round(sppnumb/50 * 100))%>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration= 0) 

## Absolute cover of natives for the beginning of restoration (e.g.
## 1 year.for.report)
AbsCoverNatFirst <- Bee2 %>%
  filter(Native.Non.Native =="Native" & restoration.type == "passive" & DataType=="T.PI" & Year_of_restoration == 1) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNat = round(sppnumb/50 * 100))%>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration = 1) 

## Absolute cover of natives for the beginning of restoration (e.g.
## 3 year.for.report)
AbsCoverNatTrd <- Bee2 %>%
  filter(Native.Non.Native =="Native"  & restoration.type == "passive" & DataType=="T.PI" & Year_of_restoration == 3) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNat = round(sppnumb/50 * 100)) %>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration = 3)

## Absolute cover of natives for the beginning of restoration (e.g.
## 5 year.for.report)
AbsCoverNatFive <- Bee2 %>%
  filter(Native.Non.Native =="Native"  & restoration.type == "passive" & DataType=="T.PI" & Year_of_restoration == 5) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNat = round(sppnumb/50 * 100))%>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration= 5)

## Absolute cover of natives for the beginning of restoration (e.g.
## 7 year.for.report)
AbsCoverNatSeven <- Bee2 %>%
  filter(Native.Non.Native =="Native"  & restoration.type == "passive" & DataType=="T.PI" & Year_of_restoration == 7) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNat = round(sppnumb/50 * 100))%>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration = 7)

AbsCoverNatpassive<- bind_rows(AbsCoverNatZero,AbsCoverNatFirst,AbsCoverNatTrd,
                        AbsCoverNatFive,AbsCoverNatSeven) 

#write.table(AbsCoverNatpassive,"AbsCoverNatBeepassive.csv",quote=F,row.names=F,sep=",",na="")


## Absolute cover of natives for the beginning of restoration (e.g.
## 0 year.for.report)
AbsCoverNonNatZero <- Bee2 %>%
  filter(Native.Non.Native =="Non-native" & restoration.type == "passive" & DataType=="T.PI" & Year_of_restoration== 0) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNonNat = round(sppnumb/50 * 100)) %>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration = 0) 

## Absolute cover of natives for the beginning of restoration (e.g.
## 1 year.for.report)
AbsCoverNonNatFirst <- Bee2 %>%
  filter(Native.Non.Native =="Non-native" & restoration.type == "passive" & DataType=="T.PI" & Year_of_restoration == 1) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNonNat = round(sppnumb/50 * 100))%>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration = 1) 

## Absolute cover of natives for the beginning of restoration (e.g.
## 3 year.for.report)
AbsCoverNonNatTrd <- Bee2 %>%
  filter(Native.Non.Native =="Non-native" & restoration.type == "passive" & DataType=="T.PI" & Year_of_restoration == 3) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNonNat = round(sppnumb/50 * 100)) %>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration = 3)

## Absolute cover of natives for the beginning of restoration (e.g.
## 5 year.for.report)
AbsCoverNonNatFive <- Bee2 %>%
  filter(Native.Non.Native =="Non-native"  & restoration.type == "passive" & DataType=="T.PI" & Year_of_restoration == 5) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNonNat = round(sppnumb/50 * 100)) %>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration = 5)

## Absolute cover of natives for the beginning of restoration (e.g.
## 7 year.for.report)
AbsCoverNonNatSeven <- Bee2 %>%
  filter(Native.Non.Native =="Non-native"  & restoration.type == "passive" & DataType=="T.PI" & Year_of_restoration == 7) %>%
  group_by(polygon.transect.ID) %>%
  summarise(sppnumb = n_distinct(SpeciesCode))%>%
  mutate(AbsCoverNonNat = round(sppnumb/50 * 100)) %>% #calculates the absolute cover using the species counts divided by the total number of pins and multiply by 100 to get percentage 
  mutate(Units ="Percetage")%>%
  mutate(Year_of_restoration = 7)

AbsCoverNonNatpassive<- bind_rows(AbsCoverNonNatZero,AbsCoverNonNatFirst,
                           AbsCoverNonNatTrd,AbsCoverNonNatFive,AbsCoverNonNatSeven) 

# This code splits Bee2 file in a species code and location and another file with
# additional plant data.

#Species code file
spCode<-Bee2 %>%
  select(SpeciesCode)%>%
  distinct()%>%
  arrange(SpeciesCode)

#write.table(spCode,"SppCodeBeeFlat.csv",quote=F,row.names=F,sep=",",na="")

#Additional plant data code file
PlantData<-Bee2 %>%
  select(c(SpeciesCode, ScientificName, Native.Non.Native, Functional.Group,
           FunGroup, LifeForm,Phenology))%>%
  distinct() %>%
  arrange(SpeciesCode)#the number of rows may not match because a unique combination with the other row values

#write.table(PlantData,"PlantDataBeeFlat.csv",quote=F,row.names=F,sep=",",na="")

# Loralee's code
# checking data entry via pivot tables of sorts

#subset just count data
BeeFlat2=BeeFlat%>%
  filter(DataType%in%c("T.PI", "A.Belt"))%>%
  mutate(count=1)#this will get used for calculating abundances later on


#subsetting just the active restoration plots
BeeFlat_plot<-BeeFlat2%>%
  select("Site" ,"Year", Year_of_restoration, restoration.type,"StageRestoration", "ProjectPhase","polygon.transect.ID" )%>%
  unique()%>%
  mutate(Year=as.factor(Year))%>%
  mutate(Year_of_restoration=as.factor(Year_of_restoration))%>%
  filter(restoration.type=="active")
#gives a tally of the number of transects across Restoration stages
table(Year=BeeFlat_plot$Year_of_restoration, score=BeeFlat_plot$StageRestoration)
#numbers vary by year need to check the individual phases to see where there might be an error
