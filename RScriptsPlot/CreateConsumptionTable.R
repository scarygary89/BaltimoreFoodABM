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

BaseFoodCons_race <- CalculateWeightedAverages(BaseLong_Race, "race")
MeatlessMondayFoodCons_race <- CalculateWeightedAverages(MeatlessMondayLong_Race, "race") 
PriceSurgeFoodCons_race <- CalculateWeightedAverages(PriceSurgeLong_Race, "race")
SupplyShockFoodCons_race <- CalculateWeightedAverages(SupplyShockLong_Race, "race")
COVIDFoodCons_race <- CalculateWeightedAverages(COVIDLong_Race, "race")
MoreMeatlessOptionsFoodCons_race <- CalculateWeightedAverages(MoreMeatlessOptionsLong_Race, "race")
ComprehensiveMarketingFoodCons_race <- CalculateWeightedAverages(ComprehensiveMarketingLong_Race, "race")


BaseFoodCons_poverty <- CalculateWeightedAverages(BaseLong_Poverty, "poverty")
MeatlessMondayFoodCons_poverty <- CalculateWeightedAverages(MeatlessMondayLong_Poverty, "poverty") 
PriceSurgeFoodCons_poverty <- CalculateWeightedAverages(PriceSurgeLong_Poverty, "poverty")
SupplyShockFoodCons_poverty <- CalculateWeightedAverages(SupplyShockLong_Poverty, "poverty")
COVIDFoodCons_poverty <- CalculateWeightedAverages(COVIDLong_Poverty, "poverty")
MoreMeatlessOptionsFoodCons_poverty <- CalculateWeightedAverages(MoreMeatlessOptionsLong_Poverty, "poverty")
ComprehensiveMarketingFoodCons_poverty <- CalculateWeightedAverages(ComprehensiveMarketingLong_Poverty, "poverty")


BaseFoodCons_income <- CalculateWeightedAverages(BaseLong_Income, "income")
MeatlessMondayFoodCons_income <- CalculateWeightedAverages(MeatlessMondayLong_Income, "income") 
PriceSurgeFoodCons_income <- CalculateWeightedAverages(PriceSurgeLong_Income, "income")
SupplyShockFoodCons_income <- CalculateWeightedAverages(SupplyShockLong_Income, "income")
COVIDFoodCons_income <- CalculateWeightedAverages(COVIDLong_Income, "income")
MoreMeatlessOptionsFoodCons_income <- CalculateWeightedAverages(MoreMeatlessOptionsLong_Income, "income")
ComprehensiveMarketingFoodCons_income <- CalculateWeightedAverages(ComprehensiveMarketingLong_Income, "income")

CalculateFractionalBreakdown <- function(ConsData){
    FracData <- ConsData %>%
        mutate(
            Total = Meat + Veg + Poultry + Fish + LegumesBeansNuts + EggCheese + Grains,
            "Red Meat" = Meat / Total,
            "Vegetables" = Veg / Total,
            "Poultry" = Poultry / Total,
            "Fish" = Fish / Total,
            "Legumes, Beans, & Nuts" = LegumesBeansNuts / Total,
            "Eggs & Cheese" = EggCheese / Total,
            "Grains" = Grains / Total
        )
    return(FracData)
}

BaseFoodConsFrac_race <- CalculateFractionalBreakdown(BaseFoodCons_race)
MeatlessMondayFoodConsFrac_race  <- CalculateFractionalBreakdown(MeatlessMondayFoodCons_race)
PriceSurgeFoodConsFrac_race  <- CalculateFractionalBreakdown(PriceSurgeFoodCons_race)
SupplyShockFoodConsFrac_race  <- CalculateFractionalBreakdown(SupplyShockFoodCons_race)
COVIDFoodConsFrac_race  <- CalculateFractionalBreakdown(COVIDFoodCons_race)
MoreMeatlessOptionsFoodConsFrac_race  <- CalculateFractionalBreakdown(MoreMeatlessOptionsFoodCons_race)
ComprehensiveMarketingFoodConsFrac_race  <- CalculateFractionalBreakdown(ComprehensiveMarketingFoodCons_race)

BaseFoodConsFrac_poverty  <- CalculateFractionalBreakdown(BaseFoodCons_poverty)
MeatlessMondayFoodConsFrac_poverty  <- CalculateFractionalBreakdown(MeatlessMondayFoodCons_poverty)
PriceSurgeFoodConsFrac_poverty  <- CalculateFractionalBreakdown(PriceSurgeFoodCons_poverty)
SupplyShockFoodConsFrac_poverty  <- CalculateFractionalBreakdown(SupplyShockFoodCons_poverty)
COVIDFoodConsFrac_poverty  <- CalculateFractionalBreakdown(COVIDFoodCons_poverty)
MoreMeatlessOptionsFoodConsFrac_poverty  <- CalculateFractionalBreakdown(MoreMeatlessOptionsFoodCons_poverty)
ComprehensiveMarketingFoodConsFrac_poverty  <- CalculateFractionalBreakdown(ComprehensiveMarketingFoodCons_poverty)

BaseFoodConsFrac_income  <- CalculateFractionalBreakdown(BaseFoodCons_income)
MeatlessMondayFoodConsFrac_income  <- CalculateFractionalBreakdown(MeatlessMondayFoodCons_income)
PriceSurgeFoodConsFrac_income  <- CalculateFractionalBreakdown(PriceSurgeFoodCons_income)
SupplyShockFoodConsFrac_income  <- CalculateFractionalBreakdown(SupplyShockFoodCons_income)
COVIDFoodConsFrac_income  <- CalculateFractionalBreakdown(COVIDFoodCons_income)
MoreMeatlessOptionsFoodConsFrac_income  <- CalculateFractionalBreakdown(MoreMeatlessOptionsFoodCons_income)
ComprehensiveMarketingFoodConsFrac_income  <- CalculateFractionalBreakdown(ComprehensiveMarketingFoodCons_income)


BaseFoodConsFrac <- BaseFoodConsFrac_poverty %>% summarize(
    "Red Meat" =  sum(`Red Meat` * Population) / sum(Population),
    "Vegetables" = sum(`Vegetables` * Population) / sum(Population),
    "Poultry" = sum(`Poultry` * Population) / sum(Population),
    "Fish" = sum(`Fish` * Population) / sum(Population),
    "Legumes, Beans, & Nuts" =  sum(`Legumes, Beans, & Nuts` * Population) / sum(Population),
    "Eggs & Cheese" = sum(`Eggs & Cheese` * Population) / sum(Population),
    "Grains" = sum(`Grains` * Population) / sum(Population)
    )
MeatlessMondayFoodConsFrac <- MeatlessMondayFoodConsFrac_poverty %>% summarize(
    "Red Meat" =  sum(`Red Meat` * Population) / sum(Population),
    "Vegetables" = sum(`Vegetables` * Population) / sum(Population),
    "Poultry" = sum(`Poultry` * Population) / sum(Population),
    "Fish" = sum(`Fish` * Population) / sum(Population),
    "Legumes, Beans, & Nuts" =  sum(`Legumes, Beans, & Nuts` * Population) / sum(Population),
    "Eggs & Cheese" = sum(`Eggs & Cheese` * Population) / sum(Population),
    "Grains" = sum(`Grains` * Population) / sum(Population)
    )
PriceSurgeFoodConsFrac <- PriceSurgeFoodConsFrac_poverty %>% summarize(
    "Red Meat" =  sum(`Red Meat` * Population) / sum(Population),
    "Vegetables" = sum(`Vegetables` * Population) / sum(Population),
    "Poultry" = sum(`Poultry` * Population) / sum(Population),
    "Fish" = sum(`Fish` * Population) / sum(Population),
    "Legumes, Beans, & Nuts" =  sum(`Legumes, Beans, & Nuts` * Population) / sum(Population),
    "Eggs & Cheese" = sum(`Eggs & Cheese` * Population) / sum(Population),
    "Grains" = sum(`Grains` * Population) / sum(Population)
    )
SupplyShockFoodConsFrac <- SupplyShockFoodConsFrac_poverty %>% summarize(
    "Red Meat" =  sum(`Red Meat` * Population) / sum(Population),
    "Vegetables" = sum(`Vegetables` * Population) / sum(Population),
    "Poultry" = sum(`Poultry` * Population) / sum(Population),
    "Fish" = sum(`Fish` * Population) / sum(Population),
    "Legumes, Beans, & Nuts" =  sum(`Legumes, Beans, & Nuts` * Population) / sum(Population),
    "Eggs & Cheese" = sum(`Eggs & Cheese` * Population) / sum(Population),
    "Grains" = sum(`Grains` * Population) / sum(Population)
    )
COVIDFoodConsFrac <- COVIDFoodConsFrac_poverty %>% summarize(
    "Red Meat" =  sum(`Red Meat` * Population) / sum(Population),
    "Vegetables" = sum(`Vegetables` * Population) / sum(Population),
    "Poultry" = sum(`Poultry` * Population) / sum(Population),
    "Fish" = sum(`Fish` * Population) / sum(Population),
    "Legumes, Beans, & Nuts" =  sum(`Legumes, Beans, & Nuts` * Population) / sum(Population),
    "Eggs & Cheese" = sum(`Eggs & Cheese` * Population) / sum(Population),
    "Grains" = sum(`Grains` * Population) / sum(Population)
    )
MoreMeatlessOptionsFoodConsFrac <- MoreMeatlessOptionsFoodConsFrac_poverty %>% summarize(
    "Red Meat" =  sum(`Red Meat` * Population) / sum(Population),
    "Vegetables" = sum(`Vegetables` * Population) / sum(Population),
    "Poultry" = sum(`Poultry` * Population) / sum(Population),
    "Fish" = sum(`Fish` * Population) / sum(Population),
    "Legumes, Beans, & Nuts" =  sum(`Legumes, Beans, & Nuts` * Population) / sum(Population),
    "Eggs & Cheese" = sum(`Eggs & Cheese` * Population) / sum(Population),
    "Grains" = sum(`Grains` * Population) / sum(Population)
    )
ComprehensiveMarketingFoodConsFrac <- ComprehensiveMarketingFoodConsFrac_poverty %>% summarize(
    "Red Meat" =  sum(`Red Meat` * Population) / sum(Population),
    "Vegetables" = sum(`Vegetables` * Population) / sum(Population),
    "Poultry" = sum(`Poultry` * Population) / sum(Population),
    "Fish" = sum(`Fish` * Population) / sum(Population),
    "Legumes, Beans, & Nuts" =  sum(`Legumes, Beans, & Nuts` * Population) / sum(Population),
    "Eggs & Cheese" = sum(`Eggs & Cheese` * Population) / sum(Population),
    "Grains" = sum(`Grains` * Population) / sum(Population)
    )

FoodConsData_race <- rbind(
    cbind(BaseFoodConsFrac_race, Scenario = "No Change (Baseline)"),
    cbind(MeatlessMondayFoodConsFrac_race, Scenario = "Meatless Marketing (Scenario 1)"),
    cbind(PriceSurgeFoodConsFrac_race, Scenario = "Meat Price Surge (Scenario 2)"),
    # cbind(SupplyShockFoodConsFrac_race, Scenario = "SupplyShock"),
    cbind(MoreMeatlessOptionsFoodConsFrac_race, Scenario = "Increase in Meatless Selection (Scenario 3)"),
    cbind(ComprehensiveMarketingFoodConsFrac_race, Scenario = "Comprehensive Marketing (Scenario 4)")) %>%
    # cbind(COVIDFoodConsFrac_race, Scenario = "COVID-19 (Scenario 5)")) %>%
    mutate(Scenario = factor(Scenario, 
        levels = c(
            "No Change (Baseline)",
            'Meatless Marketing (Scenario 1)',
            'Meat Price Surge (Scenario 2)',
            'Increase in Meatless Selection (Scenario 3)',
            'Comprehensive Marketing (Scenario 4)')))
            # 'COVID-19 (Scenario 5)')))

FoodConsData_poverty <- rbind(
    cbind(BaseFoodConsFrac_poverty, Scenario = "No Change (Baseline)"),
    cbind(MeatlessMondayFoodConsFrac_poverty, Scenario = "Meatless Marketing (Scenario 1)"),
    cbind(PriceSurgeFoodConsFrac_poverty, Scenario = "Meat Price Surge (Scenario 2)"),
    # cbind(SupplyShockFoodConsFrac_poverty, Scenario = "SupplyShock"),
    cbind(MoreMeatlessOptionsFoodConsFrac_poverty, Scenario = "Increase in Meatless Selection (Scenario 3)"),
    cbind(ComprehensiveMarketingFoodConsFrac_poverty, Scenario = "Comprehensive Marketing (Scenario 4)")) %>%
    # cbind(COVIDFoodConsFrac_poverty, Scenario = "COVID-19 (Scenario 5)")) %>%
    mutate(Scenario = factor(Scenario, 
        levels = c(
            "No Change (Baseline)",
            'Meatless Marketing (Scenario 1)',
            'Meat Price Surge (Scenario 2)',
            'Increase in Meatless Selection (Scenario 3)',
            'Comprehensive Marketing (Scenario 4)')))
            # 'COVID-19 (Scenario 5)')))
    
FoodConsData_income <- rbind(
    cbind(BaseFoodConsFrac_income, Scenario = "No Change (Baseline)"),
    cbind(MeatlessMondayFoodConsFrac_income, Scenario = "Meatless Marketing (Scenario 1)"),
    cbind(PriceSurgeFoodConsFrac_income, Scenario = "Meat Price Surge (Scenario 2)"),
    # cbind(SupplyShockFoodConsFrac_income, Scenario = "SupplyShock"),
    cbind(MoreMeatlessOptionsFoodConsFrac_income, Scenario = "Increase in Meatless Selection (Scenario 3)"),
    cbind(ComprehensiveMarketingFoodConsFrac_income, Scenario = "Comprehensive Marketing (Scenario 4)")) %>%
    # cbind(COVIDFoodConsFrac_income, Scenario = "COVID-19 (Scenario 5)")) %>%
    mutate(Scenario = factor(Scenario, 
        levels = c(
            "No Change (Baseline)",
            'Meatless Marketing (Scenario 1)',
            'Meat Price Surge (Scenario 2)',
            'Increase in Meatless Selection (Scenario 3)',
            'Comprehensive Marketing (Scenario 4)')))
            # 'COVID-19 (Scenario 5)')))

FoodConsData <- rbind(
    cbind(BaseFoodConsFrac, Scenario = "No Change (Baseline)"),
    cbind(MeatlessMondayFoodConsFrac, Scenario = "Meatless Marketing (Scenario 1)"),
    cbind(PriceSurgeFoodConsFrac, Scenario = "Meat Price Surge (Scenario 2)"),
    # cbind(SupplyShockFoodConsFrac, Scenario = "SupplyShock"),
    cbind(MoreMeatlessOptionsFoodConsFrac, Scenario = "Increase in Meatless Selection (Scenario 3)"),
    cbind(ComprehensiveMarketingFoodConsFrac, Scenario = "Comprehensive Marketing (Scenario 4)")) %>%
    # cbind(COVIDFoodConsFrac, Scenario = "COVID-19 (Scenario 5)")) %>%
    mutate(Scenario = factor(Scenario,
        levels = c(
            "No Change (Baseline)",
            'Meatless Marketing (Scenario 1)',
            'Meat Price Surge (Scenario 2)',
            'Increase in Meatless Selection (Scenario 3)',
            'Comprehensive Marketing (Scenario 4)')))
            # 'COVID-19 (Scenario 5)')))

plotvars <- c("Red Meat", "Poultry", "Fish", "Vegetables", "Legumes, Beans, & Nuts", "Eggs & Cheese", "Grains")
mFoodConsData_race <- reshape2::melt(FoodConsData_race, id.vars=c("Scenario","race")) %>% 
    filter(variable %in% plotvars) %>% 
    mutate(
        variable = factor(variable, 
            levels = c(
                "Red Meat",
                "Poultry",
                "Fish",
                "Vegetables",
                "Legumes, Beans, & Nuts",
                "Eggs & Cheese",
                "Grains")))
mFoodConsData_poverty <- reshape2::melt(FoodConsData_poverty, id.vars=c("Scenario","poverty")) %>% 
    filter(variable %in% plotvars) %>% 
    mutate(variable = factor(variable, 
        levels = c(
            "Red Meat",
            "Poultry",
            "Fish",
            "Vegetables",
            "Legumes, Beans, & Nuts",
            "Eggs & Cheese",
            "Grains")))
mFoodConsData_poverty$poverty[mFoodConsData_poverty$poverty == 1] <- "In Poverty" 
mFoodConsData_poverty$poverty[mFoodConsData_poverty$poverty == 0] <- "Not in Poverty"
mFoodConsData_poverty$poverty <- factor(mFoodConsData_poverty$poverty, levels = c("In Poverty","Not in Poverty"))

mFoodConsData_income <- reshape2::melt(FoodConsData_income, id.vars=c("Scenario","income")) %>% 
    filter(variable %in% plotvars) %>% 
    mutate(
        variable = factor(variable, 
            levels = c(
                "Red Meat",
                "Poultry",
                "Fish",
                "Vegetables",
                "Legumes, Beans, & Nuts",
                "Eggs & Cheese",
                "Grains")),
        income = factor(income,
            levels = c("Less than $25k", "$25k - $55k", "$55k - $75k", "More than $75k")
        )
    ) 

mFoodConsData <- reshape2::melt(FoodConsData, id.vars=c("Scenario")) %>% 
    filter(variable %in% plotvars) %>%
    mutate(variable = factor(variable,
        levels = c(
            "Red Meat",
            "Poultry",
            "Fish",
            "Vegetables",
            "Legumes, Beans, & Nuts",
            "Eggs & Cheese",
            "Grains")))

barchart_race <- ggplot(mFoodConsData_race,
    aes(x=race, y=value, fill = variable, group = variable)) +
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
    facet_wrap(Scenario~.) +
    theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1)) +
    scale_y_continuous(labels = scales::percent, breaks = round(seq(0, 1, by = .1),1))

barchart_poverty <- ggplot(mFoodConsData_poverty,
    aes(x=poverty, y=value, fill = variable, group = variable)) +
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
    facet_wrap(Scenario~.) +
    theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1)) +
    scale_y_continuous(labels = scales::percent, breaks = round(seq(0, 1, by = .1),1))

barchart_income <- ggplot(mFoodConsData_income,
    aes(x=income, y=value, fill = variable, group = variable)) +
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
    facet_wrap(Scenario~.) +
    theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1)) +
    scale_y_continuous(labels = scales::percent, breaks = round(seq(0, 1, by = .1),1))

barchart <- ggplot(mFoodConsData,
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
    scale_y_continuous(labels = scales::percent, breaks = round(seq(0, 1, by = .1),1))


pdf(file="RScriptsPlot/OutputPlots/MealBreakdownByRace.pdf", width = 15, height = 10)
print(barchart_race)
dev.off()

pdf(file="RScriptsPlot/OutputPlots/MealBreakdownByIncome.pdf", width = 15, height = 10)
print(barchart_income)
dev.off()

pdf(file="RScriptsPlot/OutputPlots/MealBreakdownByPoverty.pdf", width = 15, height = 10)
print(barchart_poverty)
dev.off()

pdf(file="RScriptsPlot/OutputPlots/MealBreakdown.pdf", width = 10, height = 6)
print(barchart)
dev.off()

# Create Tables
tableData_race <- mFoodConsData_race %>% spread(Scenario,value)
tableData_income <- mFoodConsData_income %>% spread(Scenario,value)
tableData_poverty <- mFoodConsData_poverty %>% spread(Scenario,value)
tableData <- mFoodConsData %>% spread(Scenario,value)

write.csv(tableData_race, row.names =F, file ="RScriptsPlot/OutputPlots/MealBreakdownByRace.csv")
write.csv(tableData_income, row.names =F, file ="RScriptsPlot/OutputPlots/MealBreakdownByIncome.csv")
write.csv(tableData_poverty, row.names =F, file ="RScriptsPlot/OutputPlots/MealBreakdownByPoverty.csv")
write.csv(tableData, row.names = F, file ="RScriptsPlot/OutputPlots/MealBreakdown.csv")

meatRedTableData_race <- tableData_race %>%
    filter(variable %in% c("Red Meat", "Poultry")) %>%
    group_by(race) %>%
    summarize(
        "No Change (Baseline)" = sum(`No Change (Baseline)`),
        "Meatless Marketing (Scenario 1)" = sum(`Meatless Marketing (Scenario 1)`),
        "Meat Price Surge (Scenario 2)" = sum(`Meat Price Surge (Scenario 2)`),
        "Increase in Meatless Selection (Scenario 3)" = sum(`Increase in Meatless Selection (Scenario 3)`),
        "Comprehensive Marketing (Scenario 4)" = sum(`Comprehensive Marketing (Scenario 4)`)
    ) %>%
    mutate(
        "Meatless Marketing (Scenario 1)" = (`No Change (Baseline)` - `Meatless Marketing (Scenario 1)`)/`No Change (Baseline)`,
        'Meat Price Surge (Scenario 2)' = (`No Change (Baseline)` - `Meat Price Surge (Scenario 2)`)/`No Change (Baseline)`,
        'Increase in Meatless Selection (Scenario 3)' = (`No Change (Baseline)` - `Increase in Meatless Selection (Scenario 3)`)/`No Change (Baseline)`,
        'Comprehensive Marketing (Scenario 4)' = (`No Change (Baseline)` - `Comprehensive Marketing (Scenario 4)`)/`No Change (Baseline)`
        # 'COVID-19 (Scenario 5)' = (`No Change (Baseline)` - `COVID-19 (Scenario 5)`)/`No Change (Baseline)`
    ) %>% select(-c(`No Change (Baseline)`))

meatRedTableData_income <- tableData_income %>% 
    filter(variable %in% c("Red Meat", "Poultry")) %>%
    group_by(income) %>%
    summarize(
        "No Change (Baseline)" = sum(`No Change (Baseline)`),
        "Meatless Marketing (Scenario 1)" = sum(`Meatless Marketing (Scenario 1)`),
        "Meat Price Surge (Scenario 2)" = sum(`Meat Price Surge (Scenario 2)`),
        "Increase in Meatless Selection (Scenario 3)" = sum(`Increase in Meatless Selection (Scenario 3)`),
        "Comprehensive Marketing (Scenario 4)" = sum(`Comprehensive Marketing (Scenario 4)`)
    ) %>%
    mutate(
        "Meatless Marketing (Scenario 1)" = (`No Change (Baseline)` - `Meatless Marketing (Scenario 1)`)/`No Change (Baseline)`,
        'Meat Price Surge (Scenario 2)' = (`No Change (Baseline)` - `Meat Price Surge (Scenario 2)`)/`No Change (Baseline)`,
        'Increase in Meatless Selection (Scenario 3)' = (`No Change (Baseline)` - `Increase in Meatless Selection (Scenario 3)`)/`No Change (Baseline)`,
        'Comprehensive Marketing (Scenario 4)' = (`No Change (Baseline)` - `Comprehensive Marketing (Scenario 4)`)/`No Change (Baseline)`
        # 'COVID-19 (Scenario 5)' = (`No Change (Baseline)` - `COVID-19 (Scenario 5)`)/`No Change (Baseline)`
    ) %>% select(-c(`No Change (Baseline)`))

meatRedTableData_poverty <- tableData_poverty %>% 
    filter(variable %in% c("Red Meat", "Poultry")) %>%
    group_by(poverty) %>%
    summarize(
        "No Change (Baseline)" = sum(`No Change (Baseline)`),
        "Meatless Marketing (Scenario 1)" = sum(`Meatless Marketing (Scenario 1)`),
        "Meat Price Surge (Scenario 2)" = sum(`Meat Price Surge (Scenario 2)`),
        "Increase in Meatless Selection (Scenario 3)" = sum(`Increase in Meatless Selection (Scenario 3)`),
        "Comprehensive Marketing (Scenario 4)" = sum(`Comprehensive Marketing (Scenario 4)`)
    ) %>%
   mutate(
        "Meatless Marketing (Scenario 1)" = (`No Change (Baseline)` - `Meatless Marketing (Scenario 1)`)/`No Change (Baseline)`,
        'Meat Price Surge (Scenario 2)' = (`No Change (Baseline)` - `Meat Price Surge (Scenario 2)`)/`No Change (Baseline)`,
        'Increase in Meatless Selection (Scenario 3)' = (`No Change (Baseline)` - `Increase in Meatless Selection (Scenario 3)`)/`No Change (Baseline)`,
        'Comprehensive Marketing (Scenario 4)' = (`No Change (Baseline)` - `Comprehensive Marketing (Scenario 4)`)/`No Change (Baseline)`
        # 'COVID-19 (Scenario 5)' = (`No Change (Baseline)` - `COVID-19 (Scenario 5)`)/`No Change (Baseline)`
    ) %>% select(-c(`No Change (Baseline)`))


write.csv(meatRedTableData_race, row.names =F, file ="RScriptsPlot/OutputPlots/MeatReductionByRace.csv")
write.csv(meatRedTableData_income, row.names =F, file ="RScriptsPlot/OutputPlots/MeatReductionByIncome.csv")
write.csv(meatRedTableData_poverty, row.names =F, file ="RScriptsPlot/OutputPlots/MeatReductionByPoverty.csv")