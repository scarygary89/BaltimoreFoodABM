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
    summarize(BaseMeat = mean(Meat + Poultry), Population = mean(Population)) %>%
    mutate(BaseMeatPC = BaseMeat/Population)
MeatlessMondayMeatConsumption_Zipcode <- MeatlessMondayLong_Poverty %>% 
    group_by(zipcode) %>%
    summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>%
    left_join(BaseMeatConsumption_Zipcode) %>%
    mutate(MeatPC = Meat/Population, ReductionMeatPC = (BaseMeatPC- MeatPC)/BaseMeatPC)
PriceSurgeMeatConsumption_Zipcode <- PriceSurgeLong_Poverty %>%
    group_by(zipcode) %>%
    summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>%
    left_join(BaseMeatConsumption_Zipcode) %>%
    mutate(MeatPC = Meat/Population, ReductionMeatPC = (BaseMeatPC- MeatPC)/BaseMeatPC) 
SupplyShockMeatConsumption_Zipcode <- SupplyShockLong_Poverty %>% 
    group_by(zipcode) %>%
    summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>%
    left_join(BaseMeatConsumption_Zipcode) %>%
    mutate(MeatPC = Meat/Population, ReductionMeatPC = (BaseMeatPC- MeatPC)/BaseMeatPC)
COVIDMeatConsumption_Zipcode <- COVIDLong_Poverty %>% 
    group_by(zipcode) %>%
    summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>%
    left_join(BaseMeatConsumption_Zipcode) %>%
    mutate(MeatPC = Meat/Population, ReductionMeatPC = (BaseMeatPC- MeatPC)/BaseMeatPC)
MoreMeatMeatlessConsumption_Zipcode <- MoreMeatlessOptionsLong_Poverty %>% 
    group_by(zipcode) %>%
    summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>%
    left_join(BaseMeatConsumption_Zipcode) %>%
    mutate(MeatPC = Meat/Population, ReductionMeatPC = (BaseMeatPC- MeatPC)/BaseMeatPC)
ComprehensiveMarketingMeatConsumption_Zipcode <- ComprehensiveMarketingLong_Poverty %>% 
    group_by(zipcode) %>%
    summarize(Meat = mean(Meat + Poultry), Population = mean(Population)) %>%
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

# Attach population information
BaltShape@data <- BaltShape@data %>% left_join(zipdata %>% mutate(Zip = as.character(Zip)), by=c("AREA_NMBR" = "Zip"))
BaltShape@data <- BaltShape@data %>% mutate(
    LowIncRatio = (`$25k - $55k` + `Less than $25k`) / (`$55k - $75k` + `More than $75k`),
    BlackProp = `Non-Hispanic Black` / (`Poverty` + `Not In Poverty`)
  )
BaltData <- fortify(BaltShape) %>% left_join(BaltShape@data)     # Convert shape file object to data.frame for ggplot
labelData <- BaltData %>% group_by(AREA_NMBR) %>% summarize(medLat = mean(lat), medLong =mean(long))
BaltData <- BaltData %>% left_join(labelData)
BaltData$medLat[BaltData$AREA_NMBR == '21209'] <- BaltData$medLat[BaltData$AREA_NMBR == '21209'] *1.008
BaltData$medLong[BaltData$AREA_NMBR == '21224'] <- BaltData$medLong[BaltData$AREA_NMBR == '21224'] *1.003
BaltData$medLong[BaltData$AREA_NMBR == '21230'] <- BaltData$medLong[BaltData$AREA_NMBR == '21230'] *.9962

toprange <- .09
MeatlessMondayMap <- ggplot() + 
  geom_polygon(data = BaltData, aes(long, y=lat, group=group, fill=MeatlessMondayMeat), color="white") +
  coord_fixed() +
  geom_text(data = BaltData %>% filter(AREA_NMBR %in% WideData$zipcode), 
    aes(x=medLong, y=medLat, label=AREA_NMBR), color="white", size=2.5, alpha=0.6) +
  scale_fill_viridis(option="rocket", name = "Reduction \nin meat consumption \ncompared to Baseline", labels = scales::percent, limits = c(0, toprange)) +
  ggtitle( "Non-meat marketing campaign (Scenario 1)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

PriceSurgeMap <- ggplot() + 
  geom_polygon(data = BaltData, aes(long, y=lat, group=group, fill=PriceSurgeMeat), color="white") +
  coord_fixed() +
  geom_text(data = BaltData %>% filter(AREA_NMBR %in% WideData$zipcode), 
    aes(x=medLong, y=medLat, label=AREA_NMBR), color="white", size=2.5, alpha=0.6) +
  scale_fill_viridis(option="rocket", name = "Reduction \nin meat consumption \ncompared to Baseline", labels = scales::percent, limits = c(0, toprange)) +
  ggtitle( "Increase in meat pricing (Scenario 2)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

SupplyShockMap <- ggplot() + 
  geom_polygon(data = BaltData, aes(long, y=lat, group=group, fill=SupplyShockMeat), color="white") +
  coord_fixed() +
  geom_text(data = BaltData %>% filter(AREA_NMBR %in% WideData$zipcode), 
    aes(x=medLong, y=medLat, label=AREA_NMBR), color="white", size=2.5, alpha=0.6) +
  scale_fill_viridis(option="rocket", name = "Reduction \nin meat consumption \ncompared to Baseline", labels = scales::percent, limits = c(0, toprange)) +
  ggtitle( "Supply Shock") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

COVIDMap <- ggplot() + 
  geom_polygon(data = BaltData, aes(long, y=lat, group=group, fill=COVIDMeat), color="white") +
  coord_fixed() +
  geom_text(data = BaltData %>% filter(AREA_NMBR %in% WideData$zipcode), 
    aes(x=medLong, y=medLat, label=AREA_NMBR), color="white", size=2.5, alpha=0.6) +
  scale_fill_viridis(option="rocket", name = "Reduction \nin meat consumption \ncompared to Baseline", labels = scales::percent, limits = c(0, toprange)) +
  ggtitle("COVID-19 (Scenario 5)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

MoreMeatMeatlessMap <- ggplot() + 
  geom_polygon(data = BaltData, aes(long, y=lat, group=group, fill=MoreMeatMeatless), color="white") +
  coord_fixed() +
  geom_text(data = BaltData %>% filter(AREA_NMBR %in% WideData$zipcode), 
    aes(x=medLong, y=medLat, label=AREA_NMBR), color="white", size=2.5, alpha=0.6) +
  scale_fill_viridis(option="rocket", name = "Reduction \nin meat consumption \ncompared to Baseline", labels = scales::percent, limits = c(0, toprange)) +
  ggtitle( "Increase in non-meat options (Scenario 3)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

ComprehensiveMarketingMeatMap <- ggplot() + 
  geom_polygon(data = BaltData, aes(long, y=lat, group=group, fill=ComprehensiveMarketingMeat), color="white") +
  coord_fixed() +
  geom_text(data = BaltData %>% filter(AREA_NMBR %in% WideData$zipcode), 
    aes(x=medLong, y=medLat, label=AREA_NMBR), color="white", size=2.5, alpha=0.6) +
  scale_fill_viridis(option="rocket", name = "Reduction \nin meat consumption \ncompared to Baseline", labels = scales::percent, limits = c(0, toprange)) +
  ggtitle( "Combined non-meat push (Scenario 4)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

BlackFracMap <- ggplot() + 
  geom_polygon(data = BaltData, aes(long, y=lat, group=group, fill=BlackProp), color="white") +
  coord_fixed() +
  geom_text(data = BaltData %>% filter(AREA_NMBR %in% WideData$zipcode), 
    aes(x=medLong, y=medLat, label=AREA_NMBR), color="white", size=2.5, alpha=0.6) +
  scale_fill_viridis(option="mako", name = "Fraction that are \n non-Hispanic Black", labels = scales::percent) +
  ggtitle( "Non-Hispanic Black Population") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

LowIncMap <- ggplot() +
  geom_polygon(data = BaltData, aes(long, y=lat, group=group, fill=LowIncRatio), color="white") +
  coord_fixed() +
  geom_text(data = BaltData %>% filter(AREA_NMBR %in% WideData$zipcode), 
    aes(x=medLong, y=medLat, label=AREA_NMBR), color="white", size=2.5, alpha=0.6) +
  scale_fill_viridis(option="mako", name = "Low Income (<$55k) \n versus High Income") +
  ggtitle( "Low Income to High Income Ratio") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


ScenarioMap <- ggarrange(
    MeatlessMondayMap,
    PriceSurgeMap,
    MoreMeatMeatlessMap,
    ComprehensiveMarketingMeatMap,
    # COVIDMap,
    common.legend = T,
    legend = "right",
    ncol = 2, nrow = 2)

PopulationMap <- ggarrange(
    BlackFracMap,
    LowIncMap,
    # COVIDMap,
    ncol = 1, nrow = 2)

ReductionMap <- ggarrange(
  ScenarioMap,
  PopulationMap,
  labels = c("A","B"),
  ncol = 2, nrow = 1,
  widths = c(2,1),
    font.label = list(size = 30)
)

# windows()
# print(ReductionMap)

pdf(file="RScriptsPlot/OutputPlots/ReductionMap.pdf", width = 18, height = 12)
print(ReductionMap)
dev.off()

summaryzipdata <- BaltShape@data %>% 
  select(AREA_NMBR, LowIncRatio, BlackProp, ComprehensiveMarketingMeat, MeatlessMondayMeat, MoreMeatMeatless, PriceSurgeMeat)

mscatdata <- melt(summaryzipdata , id.vars = c("AREA_NMBR", "LowIncRatio", "BlackProp")) %>% na.omit()
mscatdata$variable <- as.character(mscatdata$variable)
mscatdata$variable[mscatdata$variable == 'MeatlessMondayMeat'] <- "Non-meat marketing campaign (Scenario 1)"
mscatdata$variable[mscatdata$variable == 'PriceSurgeMeat'] <- "Increase in meat pricing (Scenario 2)"
mscatdata$variable[mscatdata$variable == 'MoreMeatMeatless'] <- "Increase in non-meat options (Scenario 3)"
mscatdata$variable[mscatdata$variable == 'ComprehensiveMarketingMeat'] <- "Combined non-meat push (Scenario 4)"
mscatdata$variable <- factor(mscatdata$variable, levels = c(
  "Non-meat marketing campaign (Scenario 1)",
  "Increase in meat pricing (Scenario 2)",
  "Increase in non-meat options (Scenario 3)",
  "Combined non-meat push (Scenario 4)"))

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

# Correlation Study
summaryzipdata_naomit <- na.omit(summaryzipdata)
write.csv(summaryzipdata_naomit, file = "RScriptsPlot/OutputPlots/ZipData.csv", row.names=F)

correlationList <- data.frame(Scenario = seq(1,4))
correlationList$BlackProp[1] <- cor.test(summaryzipdata_naomit %>% pull(BlackProp), summaryzipdata_naomit %>% pull(MeatlessMondayMeat))$estimate
correlationList$BlackProp[2] <- cor.test(summaryzipdata_naomit %>% pull(BlackProp), summaryzipdata_naomit %>% pull(PriceSurgeMeat))$estimate
correlationList$BlackProp[3] <- cor.test(summaryzipdata_naomit %>% pull(BlackProp), summaryzipdata_naomit %>% pull(MoreMeatMeatless))$estimate
correlationList$BlackProp[4] <- cor.test(summaryzipdata_naomit %>% pull(BlackProp), summaryzipdata_naomit %>% pull(ComprehensiveMarketingMeat))$estimate

correlationList$LowIncRatio[1] <- cor.test(summaryzipdata_naomit %>% pull(LowIncRatio), summaryzipdata_naomit %>% pull(MeatlessMondayMeat))$estimate
correlationList$LowIncRatio[2] <- cor.test(summaryzipdata_naomit %>% pull(LowIncRatio), summaryzipdata_naomit %>% pull(PriceSurgeMeat))$estimate
correlationList$LowIncRatio[3] <- cor.test(summaryzipdata_naomit %>% pull(LowIncRatio), summaryzipdata_naomit %>% pull(MoreMeatMeatless))$estimate
correlationList$LowIncRatio[4] <- cor.test(summaryzipdata_naomit %>% pull(LowIncRatio), summaryzipdata_naomit %>% pull(ComprehensiveMarketingMeat))$estimate

correlationList$BlackProp_pvalue[1] <- cor.test(summaryzipdata_naomit %>% pull(BlackProp), summaryzipdata_naomit %>% pull(MeatlessMondayMeat))$p.value
correlationList$BlackProp_pvalue[2] <- cor.test(summaryzipdata_naomit %>% pull(BlackProp), summaryzipdata_naomit %>% pull(PriceSurgeMeat))$p.value
correlationList$BlackProp_pvalue[3] <- cor.test(summaryzipdata_naomit %>% pull(BlackProp), summaryzipdata_naomit %>% pull(MoreMeatMeatless))$p.value
correlationList$BlackProp_pvalue[4] <- cor.test(summaryzipdata_naomit %>% pull(BlackProp), summaryzipdata_naomit %>% pull(ComprehensiveMarketingMeat))$p.value

correlationList$LowIncRatio_pvalue[1] <- cor.test(summaryzipdata_naomit %>% pull(LowIncRatio), summaryzipdata_naomit %>% pull(MeatlessMondayMeat))$p.value
correlationList$LowIncRatio_pvalue[2] <- cor.test(summaryzipdata_naomit %>% pull(LowIncRatio), summaryzipdata_naomit %>% pull(PriceSurgeMeat))$p.value
correlationList$LowIncRatio_pvalue[3] <- cor.test(summaryzipdata_naomit %>% pull(LowIncRatio), summaryzipdata_naomit %>% pull(MoreMeatMeatless))$p.value
correlationList$LowIncRatio_pvalue[4] <- cor.test(summaryzipdata_naomit %>% pull(LowIncRatio), summaryzipdata_naomit %>% pull(ComprehensiveMarketingMeat))$p.value

write.csv(correlationList, file = "RScriptsPlot/OutputPlots/ZCTAAnalysis.csv", row.names=F)
