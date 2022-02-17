library(rgdal)
library(ggplot2)
library(dplyr)
library(data.table)
library(viridis)
library(fishualize)

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

# Create long data that can be processed into plot data
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

# Calculate meat consumption by social groups
BaseMeatConsumption_Race <- BaseLong_Race %>% group_by(race, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
BaseMeatConsumption_Poverty <- BaseLong_Poverty %>% group_by(poverty, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
BaseMeatConsumption_Income <- BaseLong_Income %>% group_by(income, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)

MeatlessMondayMeatConsumption_Race <- MeatlessMondayLong_Race %>% group_by(race, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
MeatlessMondayMeatConsumption_Poverty <- MeatlessMondayLong_Poverty %>% group_by(poverty, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
MeatlessMondayMeatConsumption_Income <- MeatlessMondayLong_Income %>% group_by(income, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)

PriceSurgeMeatConsumption_Race <- PriceSurgeLong_Race %>% group_by(race, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
PriceSurgeMeatConsumption_Poverty <- PriceSurgeLong_Poverty %>% group_by(poverty, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
PriceSurgeMeatConsumption_Income <- PriceSurgeLong_Income %>% group_by(income, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)

SupplyShockMeatConsumption_Race <- SupplyShockLong_Race %>% group_by(race, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
SupplyShockMeatConsumption_Poverty <- SupplyShockLong_Poverty %>% group_by(poverty, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
SupplyShockMeatConsumption_Income <- SupplyShockLong_Income %>% group_by(income, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)

COVIDMeatConsumption_Race <- COVIDLong_Race %>% group_by(race, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
COVIDMeatConsumption_Poverty <- COVIDLong_Poverty %>% group_by(poverty, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
COVIDMeatConsumption_Income <- COVIDLong_Income %>% group_by(income, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)

MoreMeatMeatConsumption_Race <- MoreMeatlessOptionsLong_Race %>% group_by(race, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
MoreMeatMeatConsumption_Poverty <- MoreMeatlessOptionsLong_Poverty %>% group_by(poverty, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
MoreMeatMeatConsumption_Income <- MoreMeatlessOptionsLong_Income %>% group_by(income, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)

ComprehensiveMarketingMeatConsumption_Race <- ComprehensiveMarketingLong_Race %>% group_by(race, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
ComprehensiveMarketingMeatConsumption_Poverty <- ComprehensiveMarketingLong_Poverty %>% group_by(poverty, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)
ComprehensiveMarketingMeatConsumption_Income <- ComprehensiveMarketingLong_Income %>% group_by(income, Meal) %>% summarize(Meat = mean(Meat), Population = mean(Population)) %>% mutate(MeatPC = Meat/Population)

# Calculate average meat consumption for each scenario
BaseAvgTSdata <- BaseMeatConsumption_Poverty %>% 
    mutate(weightedMeat = Meat * Population) %>%
    group_by(Meal) %>%
    summarize(weightedMeat = sum(weightedMeat, na.rm = T), sumPopulation = sum(Population, na.rm =T), Population = mean(Population)) %>%
    mutate(MeatPerc = weightedMeat/sumPopulation/Population/100)

MeatlessMondayAvgTSdata <- MeatlessMondayMeatConsumption_Poverty %>% 
    mutate(weightedMeat = Meat * Population) %>%
    group_by(Meal) %>%
    summarize(weightedMeat = sum(weightedMeat, na.rm = T), sumPopulation = sum(Population, na.rm =T), Population = mean(Population)) %>%
    mutate(MeatPerc = weightedMeat/sumPopulation/Population/100)

PriceSurgeAvgTSdata <- PriceSurgeMeatConsumption_Poverty %>% 
    mutate(weightedMeat = Meat * Population) %>%
    group_by(Meal) %>%
    summarize(weightedMeat = sum(weightedMeat, na.rm = T), sumPopulation = sum(Population, na.rm =T), Population = mean(Population)) %>%
    mutate(MeatPerc = weightedMeat/sumPopulation/Population/100)

SupplyShockAvgTSdata <- SupplyShockMeatConsumption_Poverty %>% 
    mutate(weightedMeat = Meat * Population) %>%
    group_by(Meal) %>%
    summarize(weightedMeat = sum(weightedMeat, na.rm = T), sumPopulation = sum(Population, na.rm =T), Population = mean(Population)) %>%
    mutate(MeatPerc = weightedMeat/sumPopulation/Population/100)

COVIDAvgTSdata <- COVIDMeatConsumption_Poverty %>% 
    mutate(weightedMeat = Meat * Population) %>%
    group_by(Meal) %>%
    summarize(weightedMeat = sum(weightedMeat, na.rm = T), sumPopulation = sum(Population, na.rm =T), Population = mean(Population)) %>%
    mutate(MeatPerc = weightedMeat/sumPopulation/Population/100)

MoreMeatAvgTSdata <- MoreMeatMeatConsumption_Poverty %>% 
    mutate(weightedMeat = Meat * Population) %>%
    group_by(Meal) %>%
    summarize(weightedMeat = sum(weightedMeat, na.rm = T), sumPopulation = sum(Population, na.rm =T), Population = mean(Population)) %>%
    mutate(MeatPerc = weightedMeat/sumPopulation/Population/100)

ComprehensiveMarketingAvgTSdata <- ComprehensiveMarketingMeatConsumption_Poverty %>% 
    mutate(weightedMeat = Meat * Population) %>%
    group_by(Meal) %>%
    summarize(weightedMeat = sum(weightedMeat, na.rm = T), sumPopulation = sum(Population, na.rm =T), Population = mean(Population)) %>%
    mutate(MeatPerc = weightedMeat/sumPopulation/Population/100)

TSPlotData <- rbind(
    cbind(BaseAvgTSdata, Scenario = "No Change (Baseline)"),
    cbind(MeatlessMondayAvgTSdata, Scenario = "Meatless Marketing (Scenario 1)"),
    cbind(PriceSurgeAvgTSdata, Scenario = "Meat Price Surge (Scenario 2)"),
    # cbind(SupplyShockAvgTSdata, Scenario = "SupplyShock"),
    cbind(MoreMeatAvgTSdata, Scenario = "Increase in Meatless Selection (Scenario 3)"),
    cbind(ComprehensiveMarketingAvgTSdata, Scenario = "Comprehensive Marketing (Scenario 4)"),
    cbind(COVIDAvgTSdata, Scenario = "COVID-19 (Scenario 5)"))  %>%
    mutate(Scenario = factor(Scenario, 
        levels = c(
            "No Change (Baseline)",
            'Meatless Marketing (Scenario 1)',
            'Meat Price Surge (Scenario 2)',
            'Increase in Meatless Selection (Scenario 3)',
            'Comprehensive Marketing (Scenario 4)',
            'COVID-19 (Scenario 5)')))


tplotAvg <- ggplot(TSPlotData,
    aes(x = Meal, y = MeatPerc, group = Scenario, color = Scenario)) +
    geom_point(aes(color = Scenario)) +
    geom_line(aes(color = Scenario)) +
    # stat_smooth(aes(color = Scenario, fill = Scenario), method = "loess") +
    geom_vline(color = 'red', xintercept = 50, linetype = "longdash") +
    theme_bw() +
    scale_color_fish_d(option="Cirrhilabrus_solorensis") + 
    ylab("Average Meat Consumption") +
    xlab("Day (Dinner #)") +
    scale_y_continuous(labels = scales::percent)

pdf(file="RScriptsPlot/OutputPlots/ConsumptionTS.pdf", width = 15, height = 12)
print(tplotAvg)
dev.off()
