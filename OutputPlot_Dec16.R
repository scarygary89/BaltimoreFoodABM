library(dplyr)
library(data.table)
library(ggplot2)
library(scales)
library(fishualize)

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
BaselineData <- ImportData("C:/Users/GaryLin/Dropbox/MarylandFoodModel/ABM/TestABM/Output/Baseline")
MeatlessMondayData <- ImportData("C:/Users/GaryLin/Dropbox/MarylandFoodModel/ABM/TestABM/Output/MeatlessMonday")
PriceSurgeData <- ImportData("C:/Users/GaryLin/Dropbox/MarylandFoodModel/ABM/TestABM/Output/PriceSurge")
SupplyShockData <- ImportData("C:/Users/GaryLin/Dropbox/MarylandFoodModel/ABM/TestABM/Output/SupplyShock")


Racemat <- cbind(BaselineData[['Race']], scenario = "Baseline") %>%
    rbind(cbind(MeatlessMondayData[['Race']], scenario = "MeatlessMonday")) %>%
    rbind(cbind(PriceSurgeData[['Race']], scenario = "PriceSurge")) %>%
    rbind(cbind(SupplyShockData[['Race']], scenario = "SupplyShock")) %>%
    # rbind(cbind(MeatlessMondayData[['Combo']], scenario = "Combo")) %>%
    mutate(
        # Total = Veg + Meat + Poultry + Fish + LegumesBeansNuts + Grains + Other,
        Total = Veg + Meat + Poultry + Fish + LegumesBeansNuts + Grains,
        VegPerc = Veg / Total,
        MeatPerc = Meat / Total,
        PoultryPerc = Poultry / Total,
        FishPerc = Fish / Total,
        LegumesBeansNutsPerc = LegumesBeansNuts / Total,
        GrainsPerc = Grains / Total
        # OtherPerc = Other / Total
    )

Racemat_avg <- Racemat %>%
    select(
        VegPerc,
        MeatPerc,
        PoultryPerc,
        FishPerc,
        LegumesBeansNutsPerc,
        GrainsPerc,
        # OtherPerc,
        race,
        zipcode,
        Meal,
        scenario
    )

Racemat_sum <- Racemat %>%
    select(
        Veg,
        Meat,
        Poultry,
        Fish,
        LegumesBeansNuts,
        Grains,
        Other,
        race,
        zipcode,
        Meal,
        scenario
    )



Incomemat <- cbind(BaselineData[['Income']], scenario = "Baseline") %>%
    rbind(cbind(MeatlessMondayData[['Income']], scenario = "MeatlessMonday")) %>%
    rbind(cbind(PriceSurgeData[['Income']], scenario = "PriceSurge")) %>%
    rbind(cbind(SupplyShockData[['Income']], scenario = "SupplyShock")) %>%
    mutate(
        # Total = Veg + Meat + Poultry + Fish + LegumesBeansNuts + Grains + Other,
        Total = Veg + Meat + Poultry + Fish + LegumesBeansNuts + Grains ,
        VegPerc = Veg / Total,
        MeatPerc = Meat / Total,
        PoultryPerc = Poultry / Total,
        FishPerc = Fish / Total,
        LegumesBeansNutsPerc = LegumesBeansNuts / Total,
        GrainsPerc = Grains / Total
        # OtherPerc = Other / Total
    )
    
Incomemat_avg <- Incomemat %>%
    select(
        VegPerc,
        MeatPerc,
        PoultryPerc,
        FishPerc,
        LegumesBeansNutsPerc,
        GrainsPerc,
        # OtherPerc,
        income,
        zipcode,
        Meal,
        scenario
    )

Incomemat_sum <- Incomemat %>%
    select(
        Veg,
        Meat,
        Poultry,
        Fish,
        LegumesBeansNuts,
        Grains,
        Other,
        income,
        zipcode,
        Meal,
        scenario
    )

Povertymat <- cbind(BaselineData[['Poverty']], scenario = "Baseline") %>%
    rbind(cbind(MeatlessMondayData[['Poverty']], scenario = "MeatlessMonday")) %>%
    rbind(cbind(PriceSurgeData[['Poverty']], scenario = "PriceSurge")) %>%
    rbind(cbind(SupplyShockData[['Poverty']], scenario = "SupplyShock")) %>%
    # rbind(cbind(MeatlessMondayData[['Combo']], scenario = "Combo")) %>%
    mutate(
        # Total = Veg + Meat + Poultry + Fish + LegumesBeansNuts + Grains + Other,
        Total = Veg + Meat + Poultry + Fish + LegumesBeansNuts + Grains,
        VegPerc = Veg / Total,
        MeatPerc = Meat / Total,
        PoultryPerc = Poultry / Total,
        FishPerc = Fish / Total,
        LegumesBeansNutsPerc = LegumesBeansNuts / Total,
        GrainsPerc = Grains / Total
        # OtherPerc = Other / Total
    ) 
    
Povertymat_avg <- Povertymat %>%
    select(
        VegPerc,
        MeatPerc,
        PoultryPerc,
        FishPerc,
        LegumesBeansNutsPerc,
        GrainsPerc,
        # OtherPerc,
        poverty,
        zipcode,
        Meal,
        scenario
    )

  
Povertymat_sum <- Povertymat %>%
    select(
        Veg,
        Meat,
        Poultry,
        Fish,
        LegumesBeansNuts,
        Grains,
        Other,
        poverty,
        zipcode,
        Meal,
        scenario
    )

# Average timeseries
mRacemat_avg <- melt(Racemat_avg, id.vars = c('Meal','race','scenario')) %>% mutate(value=as.numeric(value))
mIncomemat_avg <- melt(Incomemat_avg, id.vars = c('Meal','income','scenario')) %>% mutate(value=as.numeric(value))
mPovertymat_avg <- melt(Povertymat_avg, id.vars = c('Meal','poverty','scenario')) %>% mutate(value=as.numeric(value))

tplotAvg_Race <- ggplot(mRacemat_avg %>% filter(variable == 'MeatPerc'),
    aes(x = Meal, y = value, group = scenario, color = scenario)) +
    geom_point(size = .1, alpha = .1) +
    stat_smooth(aes(color = scenario, fill = scenario), method = "loess") +
    facet_wrap(race~.) +
    geom_vline(color = 'red', xintercept = 50, linetype = "longdash") +
    theme_bw() +
    scale_y_continuous(labels = scales::percent, limits =c(.05, .15)) 


windows()
print(tplotAvg_Race)

tplotAvg_Income <- ggplot(mIncomemat_avg %>% filter(variable == 'MeatPerc'),
    aes(x = Meal, y = value, group = scenario, color = scenario)) +
    geom_point(size = .1, alpha = .1) +
    stat_smooth(aes(color = scenario, fill = scenario), method = "loess") +
    facet_wrap(income~.) +
    geom_vline(color = 'red', xintercept = 50, linetype = "longdash") +
    theme_bw() +
    scale_y_continuous(labels = scales::percent, limits =c(.05, .15)) 


windows()
print(tplotAvg_Income)

tplotAvg_Poverty <- ggplot(mPovertymat_avg %>% filter(variable == 'MeatPerc'),
    aes(x = Meal, y = value, group = scenario, color = scenario)) +
    geom_point(size = .1, alpha = .1) +
    stat_smooth(aes(color = scenario, fill = scenario), method = "loess") +
    facet_wrap(poverty~.) +
    geom_vline(color = 'red', xintercept = 50, linetype = "longdash") +
    theme_bw() +
    scale_y_continuous(labels = scales::percent, limits =c(.05, .15)) 

windows()
print(tplotAvg_Poverty)

# Sum time series
mRacemat_sum <- melt(Racemat_sum, id.vars = c('Meal','race','scenario')) %>% mutate(value=as.numeric(value))
mIncomemat_sum <- melt(Incomemat_sum, id.vars = c('Meal','income','scenario')) %>% mutate(value=as.numeric(value))
mPovertymat_sum <- melt(Povertymat_sum, id.vars = c('Meal','poverty','scenario')) %>% mutate(value=as.numeric(value))

tplotSum_Race <- ggplot(mRacemat_sum %>% filter(variable == 'Meat'),
    aes(x = Meal, y = value, group = scenario, color = scenario)) +
    geom_point(size = .1, alpha = .1) +
    stat_smooth(aes(color = scenario, fill = scenario), method = "loess") +
    facet_wrap(race~., scales='free') +
    geom_vline(color = 'red', xintercept = 50, linetype = "longdash") +
    theme_bw() 

windows()
print(tplotSum_Race)

tplotSum_Income <- ggplot(mIncomemat_sum %>% filter(variable == 'Meat'),
    aes(x = Meal, y = value, group = scenario, color = scenario)) +
    geom_point(size = .1, alpha = .1) +
    stat_smooth(aes(color = scenario, fill = scenario), method = "loess") +
    facet_wrap(income~., scales='free') +
    geom_vline(color = 'red', xintercept = 50, linetype = "longdash") +
    theme_bw() 

windows()
print(tplotSum_Income)

tplotSum_Poverty <- ggplot(mPovertymat_sum %>% filter(variable == 'Meat'),
    aes(x = Meal, y = value, group = scenario, color = scenario)) +
    geom_point(size = .1, alpha = .1) +
    stat_smooth(aes(color = scenario, fill = scenario), method = "loess") +
    facet_wrap(poverty~., scales='free') +
    geom_vline(color = 'red', xintercept = 50, linetype = "longdash") +
    theme_bw() 

windows()
print(tplotSum_Poverty)

# pie chart
mAvgRacemat <- mRacemat_avg %>% group_by(race, variable, scenario ) %>% summarize(value = mean(value))
mAvgIncomemat <- mIncomemat_avg %>% group_by(income, variable, scenario ) %>% summarize(value = mean(value))
mAvgPovertymat <- mPovertymat_avg %>% group_by(poverty, variable, scenario ) %>% summarize(value = mean(value))

mAvg <- mRacemat_avg %>% group_by(variable ) %>% summarize(value = mean(value))

piechart_Race <- ggplot(mAvgRacemat, aes(x="", y=value, fill=variable)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  # facet_grid(race~zipcode) +
  facet_wrap(scenario~race) +
  scale_color_fish_d(palette="Hypsypops_rubicundus")

windows()
print(piechart_Race)

piechart_Income <- ggplot(mAvgIncomemat, aes(x="", y=value, fill=variable)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  # facet_grid(income~zipcode) +
  facet_wrap(scenario~income) +
  scale_color_fish_d(palette="Hypsypops_rubicundus")

windows()
print(piechart_Income)


piechart_Poverty <- ggplot(mAvgPovertymat, aes(x="", y=value, fill=variable)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  # facet_grid(poverty~zipcode) +
  facet_wrap(scenario~poverty) +
  scale_color_fish_d(palette="Hypsypops_rubicundus")

windows()
print(piechart_Poverty)

# Bar plot
mRacemat_sum <- melt(Racemat_sum, id.vars = c('Meal','race','zipcode','scenario'))
mRacemat_avg <- melt(Racemat_avg, id.vars = c('Meal','race','scenario', 'zipcode')) %>% mutate(value=as.numeric(value))
mAvgRacemat <- mRacemat_avg %>% group_by(variable, race, scenario,zipcode ) %>% summarize(value = mean(value))
mAvgIncomemat <- mIncomemat_avg %>% group_by(income, zipcode, variable, scenario ) %>% summarize(value = sum(value))
mAvgPovertymat <- mPovertymat_avg %>% group_by(poverty, zipcode, variable, scenario ) %>% summarize(value = sum(value))

barchart_Race <- ggplot(mAvgRacemat %>% filter(variable == 'MeatPerc'), aes(x=race, y=value, fill = scenario)) +
    geom_bar(stat="identity", color="black", position=position_dodge())+
  coord_cartesian(ylim = c(.11,.12)) +
  theme_minimal() +
  facet_wrap(zipcode~.,ncol=6) +
  scale_color_fish_d(palette="Hypsypops_rubicundus") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

windows()
print(barchart_Race)

barchart_Income <- ggplot(Incomemat %>% filter(variable == 'MeatPerc'), aes(x=scenario, y=value, fill = income)) +
    geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() +
  facet_wrap(zipcode~.,ncol=6) +
  scale_color_fish_d(palette="Hypsypops_rubicundus") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

windows()
print(barchart_Income)

barchart_Poverty <- ggplot(Povertymat %>% filter(variable == 'MeatPerc'), aes(x=scenario, y=value, fill = poverty)) +
    geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() +
  facet_wrap(zipcode~.,ncol=6) +
  scale_color_fish_d(palette="Hypsypops_rubicundus") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

windows()
print(barchart_Poverty)

barchart_all <- ggplot(mAvg %>% filter(variable == 'MeatPerc'), aes(x=scenario, y=value)) +
    geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() +
  scale_color_fish_d(palette="Hypsypops_rubicundus") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

windows()
print(barchart_all)

