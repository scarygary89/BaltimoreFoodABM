library(dplyr)
library(data.table)
library(ggplot2)
library(scales)
library(fishualize)
library(forcats)
library(stringr)
library(viridis)


# Import Sensitivity
mydir <- 'C:/Users/aadam/Desktop/TestABM/OldOutput/Output_Jan28'

DetectAndImportSensitivity <- function(filePath, sensParmsList, combo = FALSE){
    outputfiles <- dir(filePath)
    if (combo == TRUE){
        importFiles <- outputfiles[str_detect(outputfiles, sensParmsList)]
    } else {
        importFiles <- outputfiles[str_detect(outputfiles, sensParmsList) & !str_detect(outputfiles, c("_"))]
    }
    sensitivityList <- list()
    for(i in importFiles){
        print(i)
        sensitivityList[[i]] <- ImportData(paste0(filePath,'/',i))
    }
    return(sensitivityList)
}

# Create data import function
ImportData <- function(filePath) {
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

processComboData <- function(term1, term2, dataList) {
    Ranges1 = c(0.25,0.5, 0.75, 1, 1.25, 1.5, 1.75)
    if (term2 != "none"){
        Ranges2 = c(0.25,0.5, 0.75, 1, 1.25, 1.5, 1.75)
    } else {
        Ranges2 = c()
    }
    
    counti <- 0
    if (term2 != "none") {
        for (i in Ranges1) {
            for (j in Ranges2){
                tl <- paste0(term1, "Sensitivity", i, "_", term2, "Sensitivity", j)
                if (counti > 0){
                    tempList <- rbind(tempList, cbind(file = tl, i = i, j = j))
                } else {
                    tempList <- cbind(file = tl, i = i, j = j)
                }
                counti <- counti + 1
            }
        }
    } else {
        for (i in Ranges1) {
            tl <- paste0(term1, "Sensitivity", i)
            if (counti > 0){
                tempList <- rbind(tempList, cbind(file = tl, i = i))
            } else {
                tempList <- cbind(file = tl, i = i)
            }
            counti <- counti + 1
        }
    }

    counti <- 0
    for (temp in tempList[,1]){
        print(temp)
        tdf <- dataList[[temp]]

        RacematTemp <- tdf[['Race']] %>%
            mutate(
                # Total = Veg + Meat + Poultry + Fish + LegumesBeansNuts + Grains + Other,
                Total = Veg + Meat + Poultry + Fish + LegumesBeansNuts + EggCheese + Grains,
                VegPerc = Veg / Total,
                MeatPerc = Meat / Total,
                PoultryPerc = Poultry / Total,
                FishPerc = Fish / Total,
                LegumesBeansNutsPerc = LegumesBeansNuts / Total,
                EggCheesePerc = EggCheese / Total,
                GrainsPerc = Grains / Total) %>%
            select(
                VegPerc,
                MeatPerc,
                PoultryPerc,
                FishPerc,
                LegumesBeansNutsPerc,
                GrainsPerc,
                EggCheesePerc,
                # OtherPerc,
                Veg,
                Meat,
                Poultry,
                Fish,
                LegumesBeansNuts,
                Grains,
                EggCheese,
                race,
                zipcode,
                Meal,
                Population
            ) %>% 
            melt(id.vars = c('Meal','race','Population')) %>% 
            mutate(
                value=as.numeric(value),
                weightedValue = Population * value
            )
        IncomematTemp <- tdf[['Income']] %>%
            mutate(
                # Total = Veg + Meat + Poultry + Fish + LegumesBeansNuts + Grains + EggCheese Other,
                Total = Veg + Meat + Poultry + Fish + LegumesBeansNuts + EggCheese + Grains ,
                VegPerc = Veg / Total,
                MeatPerc = Meat / Total,
                PoultryPerc = Poultry / Total,
                FishPerc = Fish / Total,
                LegumesBeansNutsPerc = LegumesBeansNuts / Total,
                EggCheesePerc = EggCheese / Total,
                GrainsPerc = Grains / Total
                # OtherPerc = Other / Total
            )%>%
            select(
                VegPerc,
                MeatPerc,
                PoultryPerc,
                FishPerc,
                LegumesBeansNutsPerc,
                GrainsPerc,
                EggCheesePerc,
                # OtherPerc,
                Veg,
                Meat,
                Poultry,
                Fish,
                LegumesBeansNuts,
                Grains,
                EggCheese,

                income,
                zipcode,
                Meal,
                Population
            ) %>% 
            melt(id.vars = c('Meal','income','Population')) %>% 
            mutate(
                value=as.numeric(value),
                weightedValue = Population * value
            )
        PovertymatTemp <-  tdf[['Poverty']] %>%
            mutate(
                # Total = Veg + Meat + Poultry + Fish + LegumesBeansNuts + Grains + Other,
                Total = Veg + Meat + Poultry + Fish + LegumesBeansNuts + EggCheese + Grains ,
                VegPerc = Veg / Total,
                MeatPerc = Meat / Total,
                PoultryPerc = Poultry / Total,
                FishPerc = Fish / Total,
                LegumesBeansNutsPerc = LegumesBeansNuts / Total,
                EggCheesePerc = EggCheese / Total,
                GrainsPerc = Grains / Total
                # OtherPerc = Other / Total
            ) %>%
            select(
                VegPerc,
                MeatPerc,
                PoultryPerc,
                FishPerc,
                LegumesBeansNutsPerc,
                GrainsPerc,
                EggCheesePerc,
                # OtherPerc,
                Veg,
                Meat,
                Poultry,
                Fish,
                LegumesBeansNuts,
                Grains,
                EggCheese,
                poverty,
                zipcode,
                Meal,
                Population
            ) %>% 
            melt(id.vars = c('Meal','poverty','Population')) %>% 
            mutate(
                value=as.numeric(value),
                weightedValue = Population * value
            )
        if (term2 != "none"){
            RacematTemp <- cbind(RacematTemp, Variable1 = tempList[,2], Variable2 = tempList[,3])
            IncomematTemp <- cbind(IncomematTemp, Variable1 = tempList[,2], Variable2 = tempList[,3])
            PovertymatTemp <- cbind(PovertymatTemp, Variable1 = tempList[,2], Variable2 = tempList[,3])
        } else {
            RacematTemp <- cbind(RacematTemp, Variable1 = tempList[,2])
            IncomematTemp <- cbind(IncomematTemp, Variable1 = tempList[,2])
            PovertymatTemp <- cbind(PovertymatTemp, Variable1 = tempList[,2])
        }


        if (counti < 1) {
            Racemat <- RacematTemp
            Incomemat <- IncomematTemp
            Povertymat <- PovertymatTemp
        }
        else {
            Racemat <- rbind(Racemat, RacematTemp)
            Incomemat <- rbind(Incomemat, IncomematTemp)
            Povertymat <- rbind(Povertymat, PovertymatTemp)
        }
        counti <- counti + 1
        
    }
    if (term2 != "none"){
        Racemat <- Racemat %>% rename(!!term1 := "Variable1") %>% rename(!!term2 := "Variable2")
        Incomemat <- Incomemat %>% rename(!!term1 := "Variable1") %>% rename(!!term2 := "Variable2")
        Povertymat <- Povertymat %>% rename(!!term1 := "Variable1") %>% rename(!!term2 := "Variable2")
    } else {
        Racemat <- Racemat %>% rename(!!term1 := "Variable1")
        Incomemat <- Incomemat %>% rename(!!term1 := "Variable1")
        Povertymat <- Povertymat %>% rename(!!term1 := "Variable1")
    }
    return(list(Racemat = Racemat, Incomemat = Incomemat, Povertymat = Povertymat))
}

MarketingAvailabilityData <- DetectAndImportSensitivity(mydir, '_MarketingSensitivity', combo = TRUE)
PriceAvailiabilityData <- DetectAndImportSensitivity(mydir, '_PricingSensitivity', combo = TRUE)
PriceData <- DetectAndImportSensitivity(mydir, 'PricingSensitivity')
SupplyData <- DetectAndImportSensitivity(mydir, 'SupplySensitivity')
AvailabilityData <- DetectAndImportSensitivity(mydir, 'AvailabilitySensitivity')
MarketingData <- DetectAndImportSensitivity(mydir, 'MarketingSensitivity')

MarketingAvailabilityMat <- processComboData("Availability", "Marketing", MarketingAvailabilityData)
PriceAvailabilityMat <- processComboData("Availability", "Pricing", PriceAvailiabilityData)
PriceMat <- processComboData('Pricing', "none", PriceData)
SupplyMat <- processComboData('Supply', "none", SupplyData)
AvailabilityMat <- processComboData('Availability', "none", AvailabilityData)
MarketingMat <- processComboData('Marketing', "none", MarketingData)

# Average timeseries stratified by demographics
# Comprehensive Marketing
mAvgmat_cm <- MarketingAvailabilityMat[['Racemat']] %>%
    filter(variable %in% c("Veg", "Meat", "Poultry", "Fish", "LegumesBeansNuts", "Grains", "EggCheese")) %>%
    group_by(variable, Availability, Marketing) %>%
    summarize(value = sum(value)/sum(Population))

heat_Avg_cm <- ggplot(mAvgmat_cm %>% filter(variable == 'Meat'), 
    aes(Availability, Marketing, fill= value)) + 
    geom_tile() + 
    scale_fill_fish(option="Hypsypops_rubicundus") +
    theme_bw()

windows()
print(heat_Avg_cm)

mAvgRacemat_cm <- MarketingAvailabilityMat[['Racemat']] %>%
    filter(variable %in% c("Veg", "Meat", "Poultry", "Fish", "LegumesBeansNuts", "Grains", "EggCheese")) %>%
    group_by(race, variable, Availability, Marketing) %>%
    summarize(value = sum(value)/sum(Population))


heat_Race_cm <- ggplot(mAvgRacemat_cm %>% filter(variable == 'Meat'), aes(Availability, Marketing, fill= value)) + 
    geom_tile() + 
    scale_fill_fish(option="Hypsypops_rubicundus") +
    facet_wrap(race~.) +
    theme_bw()
windows()
print(heat_Race_cm)


mAvgIncomemat_cm <- MarketingAvailabilityMat[['Incomemat']] %>%
    filter(variable %in% c("Veg", "Meat", "Poultry", "Fish", "LegumesBeansNuts", "Grains", "EggCheese")) %>%
    group_by(income, variable, Availability, Marketing) %>% 
    summarize(value = sum(value)/sum(Population))


heat_Income_cm <- ggplot(mAvgIncomemat_cm %>% filter(variable == 'Meat'), aes(Availability, Marketing, fill= value)) + 
    geom_tile() + 
    scale_fill_fish(option="Hypsypops_rubicundus") +
    facet_wrap(income~.) +
    theme_bw()
windows()
print(heat_Income_cm)



mAvgPovertymat_cm <- MarketingAvailabilityMat[['Povertymat']] %>%
    filter(variable %in% c("Veg", "Meat", "Poultry", "Fish", "LegumesBeansNuts", "Grains", "EggCheese")) %>%
    group_by(poverty, variable, Availability, Marketing) %>% 
    summarize(value = sum(value)/sum(Population))


heat_Poverty_cm <- ggplot(mAvgPovertymat_cm %>% filter(variable == 'Meat'), aes(Availability, Marketing, fill= value)) + 
    geom_tile() + 
    scale_fill_fish(option="Hypsypops_rubicundus") +
    facet_wrap(poverty~.) +
    theme_bw()
windows()
print(heat_Poverty_cm)


# COVID 
mAvgmat_covid <- PriceAvailabilityMat[['Racemat']] %>%
    filter(variable %in% c("Veg", "Meat", "Poultry", "Fish", "LegumesBeansNuts", "Grains", "EggCheese")) %>%
    group_by(variable, Availability, Pricing) %>%
    summarize(value = sum(value)/sum(Population))


heat_Avg_covid <- ggplot(mAvgmat_covid %>% filter(variable == 'Meat'), 
    aes(Availability, Pricing, fill= value)) + 
    geom_tile() + 
    scale_fill_fish(option="Hypsypops_rubicundus") +
    theme_bw()

windows()
print(heat_Avg_covid)


mAvgRacemat_covid <- PriceAvailabilityMat[['Racemat']] %>%
    filter(variable %in% c("Veg", "Meat", "Poultry", "Fish", "LegumesBeansNuts", "Grains", "EggCheese")) %>%
    group_by(race, variable, Availability, Pricing) %>%
    summarize(value = sum(value)/sum(Population))


heat_Race_covid <- ggplot(mAvgRacemat_covid %>% filter(variable == 'Meat'), aes(Availability, Pricing, fill= value)) + 
    geom_tile() + 
    scale_fill_fish(option="Hypsypops_rubicundus") +
    facet_wrap(race~.) +
    theme_bw()
windows()
print(heat_Race_covid)


mAvgIncomemat_covid <- PriceAvailabilityMat[['Incomemat']] %>%
    filter(variable %in% c("Veg", "Meat", "Poultry", "Fish", "LegumesBeansNuts", "Grains", "EggCheese")) %>%
    group_by(income, variable, Availability, Pricing) %>% 
    summarize(value = sum(value)/sum(Population))


heat_Income_covid <- ggplot(mAvgIncomemat_covid %>% filter(variable == 'Meat'), aes(Availability, Pricing, fill= value)) + 
    geom_tile() + 
    scale_fill_fish(option="Hypsypops_rubicundus") +
    facet_wrap(income~.) +
    theme_bw()
windows()
print(heat_Income_covid)



mAvgPovertymat_covid <- PriceAvailabilityMat[['Povertymat']] %>%
    filter(variable %in% c("Veg", "Meat", "Poultry", "Fish", "LegumesBeansNuts", "Grains", "EggCheese")) %>%
    group_by(poverty, variable, Availability, Pricing) %>% 
    summarize(value = sum(value)/sum(Population))


heat_Poverty_covid <- ggplot(mAvgPovertymat_covid %>% filter(variable == 'Meat'), aes(Availability, Pricing, fill= value)) + 
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
                # OtherPerc,
                Veg,
                Meat,
                Poultry,
                Fish,
                LegumesBeansNuts,
                Grains,
                EggCheese,
                poverty,
                zipcode,
                Meal,
                Population
            )
BaselineMat <- melt(BaselineDatamat, id.vars = c('Meal','poverty','Population'))  %>% 
            mutate(
                value=as.numeric(value),
                weightedValue = Population * value
            )

mAvgmat <- cbind(BaselineMat %>% 
        select("Meal", "poverty", "Population", "variable", "weightedValue", "value"), 
        Scenario = "No Change (Baseline)") %>%
    rbind(cbind(MarketingAvailabilityMat[['Povertymat']] %>% 
        select("Meal", "poverty", "Population", "variable", "weightedValue", "value"), 
        Scenario = 'Comprehensive Marketing (Scenario 4)')) %>%
    rbind(cbind(PriceAvailabilityMat[['Povertymat']] %>% 
        select("Meal", "poverty", "Population", "variable", "weightedValue", "value"), 
        Scenario = 'COVID-19 (Scenario 5)')) %>%
    rbind(cbind(PriceMat[['Povertymat']] %>% 
        select("Meal", "poverty", "Population", "variable", "weightedValue", "value"), 
        Scenario = 'Meat Price Surge (Scenario 2)')) %>%
    # rbind(cbind(SupplyMat[['Povertymat']] %>% 
    #     select("Meal", "poverty", "Population", "variable", "weightedValue", "value"), 
    #     Scenario = 'Supply Shortage')) %>%
    rbind(cbind(AvailabilityMat[['Povertymat']] %>% 
        select("Meal", "poverty", "Population", "variable", "weightedValue", "value"), 
        Scenario = 'Increase in Meatless Selection (Scenario 3)')) %>%
    rbind(cbind(MarketingMat[['Povertymat']] %>% 
        select("Meal", "poverty", "Population", "variable", "weightedValue", "value"), 
        Scenario = 'Meatless Marketing (Scenario 1)')) %>%
    group_by(variable, Scenario, Meal) %>%
    summarize(
        sumValue = sum(value),
        value = sum(weightedValue)/sum(Population),
        Population = sum(Population)) %>%
    mutate(Scenario = factor(Scenario, 
        levels = c(
            "No Change (Baseline)",
            'Meatless Marketing (Scenario 1)',
            'Meat Price Surge (Scenario 2)',
            'Increase in Meatless Selection (Scenario 3)',
            'Comprehensive Marketing (Scenario 4)',
            'COVID-19 (Scenario 5)')),
            variable = as.character(variable))


tplotAvg <- ggplot(mAvgmat %>% filter(variable == 'MeatPerc'),
    aes(x = Meal, y = value, group = Scenario, color = Scenario)) +
    geom_point(aes(color = Scenario)) +
    geom_line(aes(color = Scenario)) +
    # stat_smooth(aes(color = Scenario, fill = Scenario), method = "loess") +
    geom_vline(color = 'red', xintercept = 50, linetype = "longdash") +
    theme_bw() +
    scale_color_fish_d(option="Cirrhilabrus_solorensis") + 
    ylab("Average Meat Consumption") +
    xlab("Day (Dinner #)") +
    scale_y_continuous(labels = scales::percent) 

windows()
print(tplotAvg)

mAvgmat_bar <- mAvgmat  %>%
    filter(variable %in% c(
                'VegPerc',
                'MeatPerc',
                'PoultryPerc',
                'FishPerc',
                'LegumesBeansNutsPerc',
                'GrainsPerc',
                'EggCheesePerc'))

mAvgmat_bar$variable[mAvgmat_bar$variable == "MeatPerc"] <- "Red Meat"
mAvgmat_bar$variable[mAvgmat_bar$variable == "PoultryPerc"] <- "Poultry"
mAvgmat_bar$variable[mAvgmat_bar$variable == "FishPerc"] <- "Fish"
mAvgmat_bar$variable[mAvgmat_bar$variable == "VegPerc"] <- "Vegetables"
mAvgmat_bar$variable[mAvgmat_bar$variable == "LegumesBeansNutsPerc"] <- "Legumes, Beans, & Nuts"
mAvgmat_bar$variable[mAvgmat_bar$variable == "EggCheesePerc"] <- "Eggs & Cheese"
mAvgmat_bar$variable[mAvgmat_bar$variable == "GrainsPerc"] <- "Grains"

mAvgmat_bar <- mAvgmat_bar %>% ungroup() %>%
    filter(!variable %in% c("zipcode", "scenario")) %>%
    group_by(variable, Scenario) %>%
    summarize(value = mean(value)) %>% 
        mutate(variable = factor(variable, 
        levels = c(
            "Red Meat",
            "Poultry",
            "Fish",
            "Vegetables",
            "Legumes, Beans, & Nuts",
            "Eggs & Cheese",
            "Grains")))

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


# Single Interventions ####################################
mAvgmat_tor <- mAvgmat  %>%
    filter(variable %in% c(
                'Veg',
                'Meat',
                'Poultry',
                'Fish',
                'LegumesBeansNuts',
                'Grains',
                'EggCheese')) %>%
    group_by(variable, Scenario) %>%
    filter(Scenario == "No Change (Baseline)") %>% 
        group_by(variable, Scenario) %>%
        summarize(baseValue = sum(value))
                
mAvgmat_tor$variable[mAvgmat_tor$variable == "Meat"] <- "Red Meat"
mAvgmat_tor$variable[mAvgmat_tor$variable == "Poultry"] <- "Poultry"
mAvgmat_tor$variable[mAvgmat_tor$variable == "Fish"] <- "Fish"
mAvgmat_tor$variable[mAvgmat_tor$variable == "Veg"] <- "Vegetables"
mAvgmat_tor$variable[mAvgmat_tor$variable == "LegumesBeansNuts"] <- "Legumes, Beans, & Nuts"
mAvgmat_tor$variable[mAvgmat_tor$variable == "EggCheese"] <- "Eggs & Cheese"
mAvgmat_tor$variable[mAvgmat_tor$variable == "Grains"] <- "Grains"



mPriceMat <- PriceMat[['Povertymat']] %>%
    filter(variable %in% 
                c('Veg',
                'Meat',
                'Poultry',
                'Fish',
                'LegumesBeansNuts',
                'Grains',
                'EggCheese')) %>%
    group_by(variable, Pricing) %>% 
    summarize(value = sum(value)) %>%
    mutate(Parameter = 'Pricing') %>% 
    rename(sensitivity = Pricing)

mSupplyMat <- SupplyMat[['Povertymat']] %>%
    filter(variable %in% 
                c('Veg',
                'Meat',
                'Poultry',
                'Fish',
                'LegumesBeansNuts',
                'Grains',
                'EggCheese')) %>%    
    group_by(variable, Supply) %>% 
    summarize(value = sum(value)) %>%
    mutate(Parameter = 'Supply') %>% 
    rename(sensitivity = Supply)

mAvailabilityMat <- AvailabilityMat[['Povertymat']] %>%
    filter(variable %in% 
                c('Veg',
                'Meat',
                'Poultry',
                'Fish',
                'LegumesBeansNuts',
                'Grains',
                'EggCheese')) %>%    
    group_by(variable, Availability) %>% 
    summarize(value = sum(value)) %>%
    mutate(Parameter = 'Availability') %>% 
    rename(sensitivity = Availability)

mMarketingMat <- MarketingMat[['Povertymat']] %>%
    filter(variable %in% 
                c('Veg',
                'Meat',
                'Poultry',
                'Fish',
                'LegumesBeansNuts',
                'Grains',
                'EggCheese')) %>%
    group_by(variable, Marketing) %>% 
    summarize(value = sum(value)) %>%
    mutate(Parameter = 'Marketing')  %>% 
    rename(sensitivity = Marketing)

TornadoPlotData <- rbind(mPriceMat, mSupplyMat, mAvailabilityMat, mMarketingMat) %>%
    mutate(variable = as.character(variable))

TornadoPlotData$variable[TornadoPlotData$variable == "Meat"] <- "Red Meat"
TornadoPlotData$variable[TornadoPlotData$variable == "Poultry"] <- "Poultry"
TornadoPlotData$variable[TornadoPlotData$variable == "Fish"] <- "Fish"
TornadoPlotData$variable[TornadoPlotData$variable == "Veg"] <- "Vegetables"
TornadoPlotData$variable[TornadoPlotData$variable == "LegumesBeansNuts"] <- "Legumes, Beans, & Nuts"
TornadoPlotData$variable[TornadoPlotData$variable == "EggCheese"] <- "Eggs & Cheese"
TornadoPlotData$variable[TornadoPlotData$variable == "Grains"] <- "Grains"


TornadoPlotData <- TornadoPlotData %>% 
    left_join(mAvgmat_tor) %>%
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
    theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1)) 

windows()
print(TornadoPlot)
