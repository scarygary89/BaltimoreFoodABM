library(rgdal)
library(ggplot2)
library(dplyr)
library(data.table)
library(viridis)

mydir <- setwd('C:/Users/aadam/Desktop/TestABM')
source("RScriptsPlot/ImportFunctions.R")

# Import Data from all Scenarios
BaselineData <- ImportDataForScenarios(paste0(mydir,'/Output'), "Baseline")
MeatlessMondayData <- ImportDataForScenarios(paste0(mydir,'/Output'), "MeatlessMonday")
PriceSurgeData <- ImportDataForScenarios(paste0(mydir,'/Output'), "PriceSurge")
SupplyShockData <- ImportDataForScenarios(paste0(mydir,'/Output'), "SupplyShock")
COVIDData <- ImportDataForScenarios(paste0(mydir,'/Output'), "COVID")
MoreMeatlessOptionsData <- ImportDataForScenarios(paste0(mydir,'/Output'), "MoreMeatlessOptions")
ComprehensiveMarketingData <- ImportDataForScenarios(paste0(mydir,'/Output'), "ComprehensiveMarketing")

BaseLong_Race <- createLongData(BaselineData,'Race')
BaseLong_Poverty <- createLongData(BaselineData,'Poverty')
BaseLong_Income <- createLongData(BaselineData,'Income')

MeatlessMondayLong_Race <- createLongData(MeatlessMondayData,'Race')
MeatlessMondayLong_Poverty <- createLongData(MeatlessMondayData,'Poverty')
MeatlessMondayLong_Income <- createLongData(MeatlessMondayData,'Income')

PriceSurgeLong_Race <- createLongData(PriceSurgeData,'Race')
PriceSurgeLong_Poverty <- createLongData(PriceSurgeData,'Poverty')
PriceSurgeLong_Income <- createLongData(PriceSurgeData,'Income')

SupplyShockLong_Race <- createLongData(SupplyShockData,'Race')
SupplyShockLong_Poverty <- createLongData(SupplyShockData,'Poverty')
SupplyShockLong_Income <- createLongData(SupplyShockData,'Income')

COVIDLong_Race <- createLongData(COVIDData,'Race')
COVIDLong_Poverty <- createLongData(COVIDData,'Poverty')
COVIDLong_Income <- createLongData(COVIDData,'Income')

MoreMeatlessOptionsLong_Race <- createLongData(MoreMeatlessOptionsData,'Race')
MoreMeatlessOptionsLong_Poverty <- createLongData(MoreMeatlessOptionsData,'Poverty')
MoreMeatlessOptionsLong_Income <- createLongData(MoreMeatlessOptionsData,'Income')

ComprehensiveMarketingLong_Race <- createLongData(ComprehensiveMarketingData,'Race')
ComprehensiveMarketingLong_Poverty <- createLongData(ComprehensiveMarketingData,'Poverty')
ComprehensiveMarketingLong_Income <- createLongData(ComprehensiveMarketingData,'Income')

# Calculate Mean
BaseMeatConsumption_Race <- BaseLong_Race %>% group_by(race, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
BaseMeatConsumption_Poverty <- BaseLong_Poverty %>% group_by(poverty, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
BaseMeatConsumption_Income <- BaseLong_Income %>% group_by(income, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
BaseMeatConsumption_Zipcode <- BaseLong_Income %>% group_by(zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)

MeatlessMondayMeatConsumption_Race <- MeatlessMondayLong_Race %>% group_by(race, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
MeatlessMondayMeatConsumption_Poverty <- MeatlessMondayLong_Poverty %>% group_by(poverty, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
MeatlessMondayMeatConsumption_Income <- MeatlessMondayLong_Income %>% group_by(income, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
MeatlessMondayMeatConsumption_Zipcode <- MeatlessMondayLong_Income %>% group_by(zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)

PriceSurgeMeatConsumption_Race <- PriceSurgeLong_Race %>% group_by(race, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
PriceSurgeMeatConsumption_Poverty <- PriceSurgeLong_Poverty %>% group_by(poverty, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
PriceSurgeMeatConsumption_Income <- PriceSurgeLong_Income %>% group_by(income, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
PriceSurgeMeatConsumption_Zipcode <- PriceSurgeLong_Income %>% group_by(zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)

SupplyShockMeatConsumption_Race <- SupplyShockLong_Race %>% group_by(race, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
SupplyShockMeatConsumption_Poverty <- SupplyShockLong_Poverty %>% group_by(poverty, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
SupplyShockMeatConsumption_Income <- SupplyShockLong_Income %>% group_by(income, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
SupplyShockMeatConsumption_Zipcode <- SupplyShockLong_Income %>% group_by(zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)

COVIDMeatConsumption_Race <- COVIDLong_Race %>% group_by(race, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
COVIDMeatConsumption_Poverty <- COVIDLong_Poverty %>% group_by(poverty, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
COVIDMeatConsumption_Income <- COVIDLong_Income %>% group_by(income, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
COVIDMeatConsumption_Zipcode <- COVIDLong_Income %>% group_by(zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)

MoreMeatMeatConsumption_Race <- MoreMeatlessOptionsLong_Race %>% group_by(race, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
MoreMeatMeatConsumption_Poverty <- MoreMeatlessOptionsLong_Poverty %>% group_by(poverty, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
MoreMeatMeatConsumption_Income <- MoreMeatlessOptionsLong_Income %>% group_by(income, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
MoreMeatMeatConsumption_Zipcode <- MoreMeatlessOptionsLong_Income %>% group_by(zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)

ComprehensiveMarketingMeatConsumption_Race <- ComprehensiveMarketingLong_Race %>% group_by(race, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
ComprehensiveMarketingMeatConsumption_Poverty <- ComprehensiveMarketingLong_Poverty %>% group_by(poverty, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
ComprehensiveMarketingMeatConsumption_Income <- ComprehensiveMarketingLong_Income %>% group_by(income, zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
ComprehensiveMarketingMeatConsumption_Zipcode <- ComprehensiveMarketingLong_Income %>% group_by(zipcode) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)

BaltShape <- readOGR("BaltCity_Zipcode/BaltCity_Zipcode.shp")   # Load Shape File
BaltShape@data$id <- rownames(BaltShape@data)                   # Assign unique ID to merge
BaltShape@data <- BaltShape@data %>%
  left_join(BaseMeatConsumption_Zipcode, by=c("AREA_NMBR" = "zipcode"))
BaltData <- fortify(BaltShape) %>% left_join(BaltShape@data)     # Convert shape file object to data.frame for ggplot
