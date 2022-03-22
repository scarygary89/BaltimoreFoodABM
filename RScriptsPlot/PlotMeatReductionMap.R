library(rgdal)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(viridis)
library(ggpubr)
library(stringr)
library(reshape2)

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

BaseLong_Poverty <- createLongData(BaselineData,'Poverty')
MeatlessMondayLong_Poverty <- createLongData(MeatlessMondayData,'Poverty')
PriceSurgeLong_Poverty <- createLongData(PriceSurgeData,'Poverty')
SupplyShockLong_Poverty <- createLongData(SupplyShockData,'Poverty')
COVIDLong_Poverty <- createLongData(COVIDData,'Poverty')
MoreMeatlessOptionsLong_Poverty <- createLongData(MoreMeatlessOptionsData,'Poverty')
ComprehensiveMarketingLong_Poverty <- createLongData(ComprehensiveMarketingData,'Poverty')

# Import zip data
zipdata <- fread('data/SocioDemZip.csv')

# Calculate Mean
BaseMeatConsumption_Zipcode <- BaseLong_Poverty %>% 
    group_by(zipcode) %>%
    summarize(BaseMeat = mean(Meat), Population = mean(Population)) %>%
    mutate(BaseMeatPC = BaseMeat/Population)
MeatlessMondayMeatConsumption_Zipcode <- MeatlessMondayLong_Poverty %>% 
    group_by(zipcode) %>%
    summarize(Meat = mean(Meat), Population = mean(Population)) %>%
    left_join(BaseMeatConsumption_Zipcode) %>%
    mutate(MeatPC = Meat/Population, ReductionMeatPC = (BaseMeatPC- MeatPC)/BaseMeatPC)
PriceSurgeMeatConsumption_Zipcode <- PriceSurgeLong_Poverty %>% 
    group_by(zipcode) %>%
    summarize(Meat = mean(Meat), Population = mean(Population)) %>%
    left_join(BaseMeatConsumption_Zipcode) %>%
    mutate(MeatPC = Meat/Population, ReductionMeatPC = (BaseMeatPC- MeatPC)/BaseMeatPC) 
SupplyShockMeatConsumption_Zipcode <- SupplyShockLong_Poverty %>% 
    group_by(zipcode) %>%
    summarize(Meat = mean(Meat), Population = mean(Population)) %>%
    left_join(BaseMeatConsumption_Zipcode) %>%
    mutate(MeatPC = Meat/Population, ReductionMeatPC = (BaseMeatPC- MeatPC)/BaseMeatPC)
COVIDMeatConsumption_Zipcode <- COVIDLong_Poverty %>% 
    group_by(zipcode) %>%
    summarize(Meat = mean(Meat), Population = mean(Population)) %>%
    left_join(BaseMeatConsumption_Zipcode) %>%
    mutate(MeatPC = Meat/Population, ReductionMeatPC = (BaseMeatPC- MeatPC)/BaseMeatPC)
MoreMeatMeatlessConsumption_Zipcode <- MoreMeatlessOptionsLong_Poverty %>% 
    group_by(zipcode) %>%
    summarize(Meat = mean(Meat), Population = mean(Population)) %>%
    left_join(BaseMeatConsumption_Zipcode) %>%
    mutate(MeatPC = Meat/Population, ReductionMeatPC = (BaseMeatPC- MeatPC)/BaseMeatPC)
ComprehensiveMarketingMeatConsumption_Zipcode <- ComprehensiveMarketingLong_Poverty %>% 
    group_by(zipcode) %>%
    summarize(Meat = mean(Meat), Population = mean(Population)) %>%
    left_join(BaseMeatConsumption_Zipcode) %>%
    mutate(MeatPC = Meat/Population, ReductionMeatPC = (BaseMeatPC- MeatPC)/BaseMeatPC)

LongData <- rbind(
    cbind(MeatlessMondayMeatConsumption_Zipcode, Scenario = "MeatlessMondayMeat"),
    cbind(PriceSurgeMeatConsumption_Zipcode, Scenario = "PriceSurgeMeat"),
    cbind(SupplyShockMeatConsumption_Zipcode, Scenario = "SupplyShockMeat"),
    cbind(COVIDMeatConsumption_Zipcode, Scenario = "COVIDMeat"),
    cbind(MoreMeatMeatlessConsumption_Zipcode, Scenario = "MoreMeatMeatless"),
    cbind(ComprehensiveMarketingMeatConsumption_Zipcode, Scenario = "ComprehensiveMarketingMeat")
)
WideData <- LongData %>% select(zipcode, ReductionMeatPC, Scenario) %>% spread(Scenario, ReductionMeatPC)

BaltShape <- readOGR("BaltCity_Zipcode/BaltCity_Zipcode.shp")   # Load Shape File
BaltShape@data$id <- rownames(BaltShape@data)                   # Assign unique ID to merge
BaltShape@data <- BaltShape@data %>%
  full_join(WideData, by=c("AREA_NMBR" = "zipcode"))
BaltData <- fortify(BaltShape) %>% left_join(BaltShape@data)     # Convert shape file object to data.frame for ggplot
labelData <- BaltData %>% group_by(AREA_NMBR) %>% summarize(medLat = mean(lat), medLong =mean(long))
BaltData <- BaltData %>% left_join(labelData)
BaltData$medLat[BaltData$AREA_NMBR == '21209'] <- BaltData$medLat[BaltData$AREA_NMBR == '21209'] *1.008
BaltData$medLong[BaltData$AREA_NMBR == '21224'] <- BaltData$medLong[BaltData$AREA_NMBR == '21224'] *1.003
BaltData$medLong[BaltData$AREA_NMBR == '21230'] <- BaltData$medLong[BaltData$AREA_NMBR == '21230'] *.9962

MeatlessMondayMap <- ggplot() + 
  geom_polygon(data = BaltData, aes(long, y=lat, group=group, fill=MeatlessMondayMeat), color="white") +
  coord_fixed() +
  geom_text(data = BaltData %>% filter(AREA_NMBR %in% WideData$zipcode), 
    aes(x=medLong, y=medLat, label=AREA_NMBR), color="white", size=2.5, alpha=0.6) +
  scale_fill_viridis(option="D", name = "Reduction \nin meat consumption \ncompared to Baseline", labels = scales::percent) +
  ggtitle( "Meatless Marketing (Scenario 1)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

PriceSurgeMap <- ggplot() + 
  geom_polygon(data = BaltData, aes(long, y=lat, group=group, fill=PriceSurgeMeat), color="white") +
  coord_fixed() +
  geom_text(data = BaltData %>% filter(AREA_NMBR %in% WideData$zipcode), 
    aes(x=medLong, y=medLat, label=AREA_NMBR), color="white", size=2.5, alpha=0.6) +
  scale_fill_viridis(option="D", name = "Reduction \nin meat consumption \ncompared to Baseline", labels = scales::percent) +
  ggtitle( "Meat Price Surge (Scenario 2)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

SupplyShockMap <- ggplot() + 
  geom_polygon(data = BaltData, aes(long, y=lat, group=group, fill=SupplyShockMeat), color="white") +
  coord_fixed() +
  geom_text(data = BaltData %>% filter(AREA_NMBR %in% WideData$zipcode), 
    aes(x=medLong, y=medLat, label=AREA_NMBR), color="white", size=2.5, alpha=0.6) +
  scale_fill_viridis(option="D", name = "Reduction \nin meat consumption \ncompared to Baseline", labels = scales::percent) +
  ggtitle( "Supply Shock") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

COVIDMap <- ggplot() + 
  geom_polygon(data = BaltData, aes(long, y=lat, group=group, fill=COVIDMeat), color="white") +
  coord_fixed() +
  geom_text(data = BaltData %>% filter(AREA_NMBR %in% WideData$zipcode), 
    aes(x=medLong, y=medLat, label=AREA_NMBR), color="white", size=2.5, alpha=0.6) +
  scale_fill_viridis(option="B", name = "Reduction \nin meat consumption \ncompared to Baseline", labels = scales::percent) +
  ggtitle( "COVID-19 (Scenario 5)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

MoreMeatMeatlessMap <- ggplot() + 
  geom_polygon(data = BaltData, aes(long, y=lat, group=group, fill=MoreMeatMeatless), color="white") +
  coord_fixed() +
  geom_text(data = BaltData %>% filter(AREA_NMBR %in% WideData$zipcode), 
    aes(x=medLong, y=medLat, label=AREA_NMBR), color="white", size=2.5, alpha=0.6) +
  scale_fill_viridis(option="D", name = "Reduction \nin meat consumption \ncompared to Baseline", labels = scales::percent) +
  ggtitle( "Increase in Meatless Option (Scenario 3)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

ComprehensiveMarketingMeatMap <- ggplot() + 
  geom_polygon(data = BaltData, aes(long, y=lat, group=group, fill=ComprehensiveMarketingMeat), color="white") +
  coord_fixed() +
  geom_text(data = BaltData %>% filter(AREA_NMBR %in% WideData$zipcode), 
    aes(x=medLong, y=medLat, label=AREA_NMBR), color="white", size=2.5, alpha=0.6) +
  scale_fill_viridis(option="D", name = "Reduction \nin meat consumption \ncompared to Baseline", labels = scales::percent) +
  ggtitle( "Comprehensive Marketing (Scenario 4)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

ReductionMap <- ggarrange(
    MeatlessMondayMap,
    PriceSurgeMap,
    MoreMeatMeatlessMap,
    ComprehensiveMarketingMeatMap,
    # COVIDMap,
    ncol = 2, nrow = 2)

# windows()
# print(ReductionMap)

pdf(file="RScriptsPlot/OutputPlots/ReductionMap.pdf", width = 16, height =12)
print(ReductionMap)
dev.off()

summaryzipdata <- BaltShape@data %>% left_join(zipdata %>% mutate(Zip = as.character(Zip)), by=c("AREA_NMBR" = "Zip"))
summaryzipdata <- summaryzipdata %>% mutate(
    LowIncRatio = (`$25k - $55k` + `Less than $25k`) / (`$55k - $75k` + `More than $75k`),
    BlackProp = `Non-Hispanic Black` / (`Poverty` + `Not In Poverty`)
  ) %>% 
  select(AREA_NMBR, LowIncRatio, BlackProp, ComprehensiveMarketingMeat, MeatlessMondayMeat, MoreMeatMeatless, PriceSurgeMeat)

mscatdata <- melt(summaryzipdata , id.vars = c("AREA_NMBR", "LowIncRatio", "BlackProp")) %>% na.omit()
mscatdata$variable <- as.character(mscatdata$variable)
mscatdata$variable[mscatdata$variable == 'MeatlessMondayMeat'] <- "Meatless Marketing (Scenario 1)"
mscatdata$variable[mscatdata$variable == 'PriceSurgeMeat'] <- "Meat Price Surge (Scenario 2)"
mscatdata$variable[mscatdata$variable == 'MoreMeatMeatless'] <- "Increase in Meatless Option (Scenario 3)"
mscatdata$variable[mscatdata$variable == 'ComprehensiveMarketingMeat'] <- "Comprehensive Marketing (Scenario 4)"
mscatdata$variable <- factor(mscatdata$variable, levels = c(
  "Meatless Marketing (Scenario 1)",
  "Meat Price Surge (Scenario 2)",
  "Increase in Meatless Option (Scenario 3)",
  "Comprehensive Marketing (Scenario 4)"))

scatincome <- ggplot(mscatdata, aes(x = LowIncRatio, y = value, color = variable, group = variable)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x="Income less than 75k / Income more than 75k", y = "Reduction in meat consumption") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.title=element_blank())

scatblack <- ggplot(mscatdata, aes(x = BlackProp, y = value, color = variable, group = variable)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x="Non-Hispanic Black Fraction", y = "Reduction in meat consumption") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) + 
  theme(legend.title=element_blank())

ScatCombo <- ggarrange(
    scatincome,
    scatblack,
    ncol = 1, nrow = 2,
    common.legend = T,
    legend = "right",
    labels = c("A", "B"))

pdf(file="RScriptsPlot/OutputPlots/ScatterPlot.pdf", width = 8, height = 8)
print(ScatCombo)
dev.off()

write.csv(summaryzipdata, file = "RScriptsPlot/OutputPlots/ZipData.csv")
