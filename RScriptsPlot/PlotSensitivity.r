library(rgdal)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(viridis)
library(fishualize)
library(reshape2)

mydir <- setwd('C:/Users/aadam/Desktop/TestABM')
source("RScriptsPlot/ImportFunctions.R")

MarketingAvailabilityData <- DetectAndImportSensitivity(paste0(mydir,"/Output_Sensitivity"), '_MarketingSensitivity', combo = TRUE)
PriceAvailiabilityData <- DetectAndImportSensitivity(paste0(mydir,"/Output_Sensitivity"), '_PricingSensitivity', combo = TRUE)
PriceData <- DetectAndImportSensitivity(paste0(mydir,"/Output_Sensitivity"), 'PricingSensitivity')
SupplyData <- DetectAndImportSensitivity(paste0(mydir,"/Output_Sensitivity"), 'SupplySensitivity')
AvailabilityData <- DetectAndImportSensitivity(paste0(mydir,"/Output_Sensitivity"), 'AvailabilitySensitivity')
MarketingData <- DetectAndImportSensitivity(paste0(mydir,"/Output_Sensitivity"), 'MarketingSensitivity')

MarketingAvailabilityKey <- createSensitivityFileKey("Availability", "Marketing")
PriceAvailabilityKey <- createSensitivityFileKey("Availability", "Pricing")
PriceKey <- createSensitivityFileKey('Pricing', "none")
SupplyKey <- createSensitivityFileKey('Supply', "none")
AvailabilityKey <- createSensitivityFileKey('Availability', "none")
MarketingKey <- createSensitivityFileKey('Marketing', "none")



CalculateWeightedAverages <- function(longDataSet, stratifyingFeature) {
    
    FoodCons <- longDataSet %>% group_by(!!!rlang::syms(c(stratifyingFeature, "Scenario", "zipcode"))) %>%
        filter(Meal >= 50) %>%
        summarize(
            Meat = sum(Meat),
            Veg = sum(Veg),
            Poultry = sum(Poultry),
            Fish = sum(Fish),
            LegumesBeansNuts = sum(LegumesBeansNuts),
            EggCheese = sum(EggCheese),
            Grains = sum(Grains),
            Population = mean(Population)
        ) %>%
        group_by(!!!rlang::syms(c(stratifyingFeature, "zipcode"))) %>%
        summarize(
            Meat = mean(Meat),
            Veg = mean(Veg),
            Poultry = mean(Poultry),
            Fish = mean(Fish),
            LegumesBeansNuts = mean(LegumesBeansNuts),
            EggCheese = mean(EggCheese),
            Grains = mean(Grains),
            Population = mean(Population)
        ) %>%
        mutate(
            WeightedMeat = Meat * Population,
            WeightedVeg = Veg * Population,
            WeightedPoultry = Poultry * Population,
            WeightedFish = Fish * Population,
            WeightedLegumesBeansNuts = LegumesBeansNuts * Population,
            WeightedEggCheese = EggCheese * Population,
            WeightedGrains = Grains * Population
        ) %>% 
        group_by(!!!rlang::syms(stratifyingFeature)) %>%
        summarize(
            Meat = sum(WeightedMeat) / sum(Population),
            Veg = sum(WeightedVeg) / sum(Population),
            Poultry = sum(WeightedPoultry) / sum(Population),
            Fish = sum(WeightedFish) / sum(Population),
            LegumesBeansNuts = sum(WeightedLegumesBeansNuts) / sum(Population),
            EggCheese = sum(WeightedEggCheese) / sum(Population),
            Grains = sum(WeightedGrains) / sum(Population),
            Population = sum(Population)
        )
    return(FoodCons)
}


# Average timeseries stratified by demographics
# Comprehensive Marketing
mAvgmat_cm <- MarketingAvailabilityMat[['Racemat']] %>%
    group_by(variable, Availability, Marketing) %>%
    summarize(value = sum(weightedValue)/sum(Population))

heat_Avg_cm <- ggplot(mAvgmat_cm %>% filter(variable == 'MeatPerc'), 
    aes(Availability, Marketing, fill= value)) + 
    geom_tile() + 
    scale_fill_fish(option="Hypsypops_rubicundus") +
    theme_bw()

windows()
print(heat_Avg_cm)

mAvgRacemat_cm <- MarketingAvailabilityMat[['Racemat']] %>%
    group_by(race, variable, Availability, Marketing) %>%
    summarize(value = sum(weightedValue)/sum(Population))

heat_Race_cm <- ggplot(mAvgRacemat_cm %>% filter(variable == 'MeatPerc'), aes(Availability, Marketing, fill= value)) + 
    geom_tile() + 
    scale_fill_fish(option="Hypsypops_rubicundus") +
    facet_wrap(race~.) +
    theme_bw()
windows()
print(heat_Race_cm)


mAvgIncomemat_cm <- MarketingAvailabilityMat[['Incomemat']] %>%
    group_by(income, variable, Availability, Marketing) %>% 
    summarize(value = sum(weightedValue)/sum(Population))

heat_Income_cm <- ggplot(mAvgIncomemat_cm %>% filter(variable == 'MeatPerc'), aes(Availability, Marketing, fill= value)) + 
    geom_tile() + 
    scale_fill_fish(option="Hypsypops_rubicundus") +
    facet_wrap(income~.) +
    theme_bw()
windows()
print(heat_Income_cm)



mAvgPovertymat_cm <- MarketingAvailabilityMat[['Povertymat']] %>%
    group_by(poverty, variable, Availability, Marketing) %>% 
    summarize(value = sum(weightedValue)/sum(Population))

heat_Poverty_cm <- ggplot(mAvgPovertymat_cm %>% filter(variable == 'MeatPerc'), aes(Availability, Marketing, fill= value)) + 
    geom_tile() + 
    scale_fill_fish(option="Hypsypops_rubicundus") +
    facet_wrap(poverty~.) +
    theme_bw()
windows()
print(heat_Poverty_cm)


# COVID 
mAvgmat_covid <- PriceAvailabilityMat[['Racemat']] %>%
    group_by(variable, Availability, Pricing) %>%
    summarize(value = sum(weightedValue)/sum(Population))

heat_Avg_covid <- ggplot(mAvgmat_covid %>% filter(variable == 'MeatPerc'), 
    aes(Availability, Pricing, fill= value)) + 
    geom_tile() + 
    scale_fill_fish(option="Hypsypops_rubicundus") +
    theme_bw()

windows()
print(heat_Avg_covid)


mAvgRacemat_covid <- PriceAvailabilityMat[['Racemat']] %>%
    group_by(race, variable, Availability, Pricing) %>%
    summarize(value = sum(weightedValue)/sum(Population))

heat_Race_covid <- ggplot(mAvgRacemat_covid %>% filter(variable == 'MeatPerc'), aes(Availability, Pricing, fill= value)) + 
    geom_tile() + 
    scale_fill_fish(option="Hypsypops_rubicundus") +
    facet_wrap(race~.) +
    theme_bw()
windows()
print(heat_Race_covid)


mAvgIncomemat_covid <- PriceAvailabilityMat[['Incomemat']] %>%
    group_by(income, variable, Availability, Pricing) %>% 
    summarize(value = sum(weightedValue)/sum(Population))

heat_Income_covid <- ggplot(mAvgIncomemat_covid %>% filter(variable == 'MeatPerc'), aes(Availability, Pricing, fill= value)) + 
    geom_tile() + 
    scale_fill_fish(option="Hypsypops_rubicundus") +
    facet_wrap(income~.) +
    theme_bw()
windows()
print(heat_Income_covid)



mAvgPovertymat_covid <- PriceAvailabilityMat[['Povertymat']] %>%
    group_by(poverty, variable, Availability, Pricing) %>% 
    summarize(value = sum(weightedValue)/sum(Population))

heat_Poverty_covid <- ggplot(mAvgPovertymat_covid %>% filter(variable == 'MeatPerc'), aes(Availability, Pricing, fill= value)) + 
    geom_tile() + 
    scale_fill_fish(option="Hypsypops_rubicundus") +
    facet_wrap(poverty~.) +
    theme_bw()
windows()
print(heat_Poverty_covid)


#######################################################################
# Time series plot
# Create data import function
ImportData <- function(filePath){
    setwd(filePath)
    filesList <- dir()
    alldata <- lapply(filesList, function(x) fread(x))
    names(alldata) <- filesList

    racedata <- alldata[grepl("Race",names(alldata))]
    races <- sub(".csv", "",sub(".*Race_", "", names(racedata)))
    racezips <- substring(sub(".*FoodConsumption", "", names(racedata)),1,5)
    Racemat <- do.call(rbind, lapply(1:length(racedata), function(x) {
        df <- racedata[[x]]
        df <- cbind(df, race = races[x], zipcode = racezips[x])
        colnames(df)[1] <- "Meal"
        return(df)
    }))

    incomedata <- alldata[grepl("Income",names(alldata))]
    incomes <- sub(".csv", "",sub(".*Income_", "", names(incomedata)))
    incomezips <- substring(sub(".*FoodConsumption", "", names(incomedata)),1,5)
    Incomemat <- do.call(rbind, lapply(1:length(incomedata), function(x) {
        df <- incomedata[[x]]
        df <- cbind(df, income = incomes[x], zipcode = incomezips[x])
        colnames(df)[1] <- "Meal"
        return(df)
    }))

    povertydata <- alldata[grepl("Poverty",names(alldata))]
    povertys <- sub(".csv", "",sub(".*Poverty_", "", names(povertydata)))
    povertyzips <- substring(sub(".*FoodConsumption", "", names(povertydata)),1,5)
    Povertymat <- do.call(rbind, lapply(1:length(povertydata), function(x) {
        df <- povertydata[[x]]
        df <- cbind(df, poverty = povertys[x], zipcode = povertyzips[x])
        colnames(df)[1] <- "Meal"
        return(df)
    }))
    return(list(Race = Racemat, Income = Incomemat, Poverty = Povertymat))
}

# Import Data from all Scenarios
BaselineData <- ImportData(paste0(mydir, "/Baseline"))
BaselineDatamat <- cbind(BaselineData[['Poverty']], scenario = "Baseline") %>%
    # rbind(cbind(MeatlessMondayData[['Combo']], scenario = "Combo")) %>%
    mutate(
        # Total = Veg + Meat + Poultry + Fish + LegumesBeansNuts + Grains + Other,
        Total = Veg + Meat + Poultry + Fish + LegumesBeansNuts + EggCheese + Grains,
        VegPerc = Veg / Total,
        MeatPerc = Meat / Total,
        PoultryPerc = Poultry / Total,
        FishPerc = Fish / Total,
        LegumesBeansNutsPerc = LegumesBeansNuts / Total,
        EggCheesePerc = EggCheese / Total,
        GrainsPerc = Grains / Total
        # OtherPerc = Other / Total
    )  %>%
    select(
        VegPerc,
        MeatPerc,
        PoultryPerc,
        FishPerc,
        LegumesBeansNutsPerc,
        GrainsPerc,
        EggCheesePerc,
        Population,
        poverty,
        zipcode,
        Meal,
        scenario
    )
BaselineMat <- melt(BaselineDatamat, id.vars = c('Meal','poverty','Population'))  %>% 
            mutate(
                value=as.numeric(value),
                weightedValue = Population * value
            )

mAvgmat <- cbind(BaselineMat %>% 
        select("Meal", "poverty", "Population", "variable", "weightedValue"), 
        Scenario = "No Change (Baseline)") %>%
    rbind(cbind(MarketingAvailabilityMat[['Povertymat']] %>% 
        select("Meal", "poverty", "Population", "variable", "weightedValue"), 
        Scenario = 'Comprehensive Marketing (Scenario 4)')) %>%
    rbind(cbind(PriceAvailabilityMat[['Povertymat']] %>% 
        select("Meal", "poverty", "Population", "variable", "weightedValue"), 
        Scenario = 'COVID-19 (Scenario 5)')) %>%
    rbind(cbind(PriceMat[['Povertymat']] %>% 
        select("Meal", "poverty", "Population", "variable", "weightedValue"), 
        Scenario = 'Meat Price Surge (Scenario 2)')) %>%
    # rbind(cbind(SupplyMat[['Povertymat']] %>% 
    #     select("Meal", "poverty", "Population", "variable", "weightedValue"), 
    #     Scenario = 'Supply Shortage')) %>%
    rbind(cbind(AvailabilityMat[['Povertymat']] %>% 
        select("Meal", "poverty", "Population", "variable", "weightedValue"), 
        Scenario = 'Increase in Meatless Selection (Scenario 3)')) %>%
    rbind(cbind(MarketingMat[['Povertymat']] %>% 
        select("Meal", "poverty", "Population", "variable", "weightedValue"), 
        Scenario = 'Meatless Marketing (Scenario 1)')) %>%
    group_by(variable, Scenario, Meal) %>%
    summarize(value = sum(weightedValue)/sum(Population)) %>% 
    mutate(Scenario = factor(Scenario, 
        levels = c(
            "No Change (Baseline)",
            'Meatless Marketing (Scenario 1)',
            'Meat Price Surge (Scenario 2)',
            'Increase in Meatless Selection (Scenario 3)',
            'Comprehensive Marketing (Scenario 4)',
            'COVID-19 (Scenario 5)')),variable = as.character(variable))
            # 'Supply Shortage'


tplotAvg <- ggplot(mAvgmat %>% filter(variable == 'MeatPerc'),
    aes(x = Meal, y = value, group = Scenario, color = Scenario)) +
    geom_point(aes(color = Scenario)) +
    geom_line(aes(color = Scenario)) +
    # stat_smooth(aes(color = Scenario, fill = Scenario), method = "loess") +
    geom_vline(color = 'red', xintercept = 50, linetype = "longdash") +
    theme_bw() +
    scale_color_fish_d(option="Cirrhilabrus_solorensis") + 
    ylab("Average Meat Consumption") +
    xlab("Day (Dinner #)")  +
    scale_y_continuous(labels = scales::percent) 

windows()
print(tplotAvg)

mAvgmat$variable[mAvgmat$variable == "MeatPerc"] <- "Red Meat"
mAvgmat$variable[mAvgmat$variable == "PoultryPerc"] <- "Poultry"
mAvgmat$variable[mAvgmat$variable == "FishPerc"] <- "Fish"
mAvgmat$variable[mAvgmat$variable == "VegPerc"] <- "Vegetables"
mAvgmat$variable[mAvgmat$variable == "LegumesBeansNutsPerc"] <- "Legumes, Beans, & Nuts"
mAvgmat$variable[mAvgmat$variable == "EggCheesePerc"] <- "Eggs & Cheese"
mAvgmat$variable[mAvgmat$variable == "GrainsPerc"] <- "Grains"

mAvgmat_bar <- mAvgmat %>% ungroup() %>%
    filter(!variable %in% c("zipcode", "scenario")) %>%
    group_by(variable, Scenario) %>%
    summarize(value = mean(value))  
    
mAvgmat_bar$variable = factor(mAvgmat_bar$variable, 
        levels = c(
            "Red Meat",
            "Poultry",
            "Fish",
            "Vegetables",
            "Legumes, Beans, & Nuts",
            "Eggs & Cheese",
            "Grains"))

barchart <- ggplot(mAvgmat_bar, 
    aes(x=Scenario, y=value, fill = variable, group = variable)) +
	geom_bar(stat="identity", color="black", position="stack") +
    coord_cartesian() + 
    coord_flip() + 
    theme_minimal() +
    # facet_wrap(variable~.) +
    xlab("") +
    ylab("Average Dinner Composition") +
    labs(fill="Food Groups") +
    # scale_color_fish_d(palette="Hypsypops_rubicundus") + 
    scale_fill_viridis(discrete = T) +
    theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1)) +
    scale_y_continuous(labels = scales::percent) 


windows()
print(barchart)


# Single Interventiosn
mPriceMat <- PriceMat[['Povertymat']] %>%
    # filter(Meal >= 50) %>%
    group_by(variable, Pricing) %>% 
    summarize(value = sum(weightedValue)/sum(Population)) %>%
    mutate(Parameter = 'Pricing') %>% 
    rename(sensitivity = Pricing)

mSupplyMat <- SupplyMat[['Povertymat']] %>%
    # filter(Meal >= 50) %>%
    group_by(variable, Supply) %>% 
    summarize(value = sum(weightedValue)/sum(Population)) %>%
    mutate(Parameter = 'Supply') %>% 
    rename(sensitivity = Supply)

mAvailabilityMat <- AvailabilityMat[['Povertymat']] %>%
    # filter(Meal >= 50) %>%
    group_by(variable, Availability) %>% 
    summarize(value = sum(weightedValue)/sum(Population)) %>%
    mutate(Parameter = 'Availability') %>% 
    rename(sensitivity = Availability)

mMarketingMat <- MarketingMat[['Povertymat']] %>%
    # filter(Meal >= 50) %>%
    group_by(variable, Marketing) %>% 
    summarize(value = sum(weightedValue)/sum(Population)) %>%
    mutate(Parameter = 'Marketing')  %>% 
    rename(sensitivity = Marketing)

TornadoPlotData <- rbind(mPriceMat, mSupplyMat, mAvailabilityMat, mMarketingMat) %>%
    mutate(variable = as.character(variable))

TornadoPlotData$variable[TornadoPlotData$variable == "MeatPerc"] <- "Red Meat"
TornadoPlotData$variable[TornadoPlotData$variable == "PoultryPerc"] <- "Poultry"
TornadoPlotData$variable[TornadoPlotData$variable == "FishPerc"] <- "Fish"
TornadoPlotData$variable[TornadoPlotData$variable == "VegPerc"] <- "Vegetables"
TornadoPlotData$variable[TornadoPlotData$variable == "LegumesBeansNutsPerc"] <- "Legumes, Beans, & Nuts"
TornadoPlotData$variable[TornadoPlotData$variable == "EggCheesePerc"] <- "Eggs & Cheese"
TornadoPlotData$variable[TornadoPlotData$variable == "GrainsPerc"] <- "Grains"

TornadoPlotData <- TornadoPlotData %>% 
    left_join(mAvgmat_bar %>% filter(Scenario == "No Change (Baseline)") %>% 
    rename(baseValue = value)) %>%
    mutate(delta = (value - baseValue)/baseValue)

TornadoPlot <- ggplot(TornadoPlotData %>% filter(variable != 'zipcode') %>% arrange(sensitivity), 
    aes(x=Parameter, y=delta, fill = sensitivity)) +
	geom_bar(stat="identity", color="black", position="dodge") +
    coord_cartesian() + 
    coord_flip() + 
    theme_minimal() +
    facet_wrap(variable~.) +
    xlab("") +
    ylab("Change compared to baseline scenario") +
    labs(fill="Parameter Values") +
    scale_fill_viridis(discrete = T) +
    theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1)) +
    scale_y_continuous(labels = scales::percent) 

windows()
print(TornadoPlot)
