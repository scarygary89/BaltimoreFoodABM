library(dplyr)
library(data.table)

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

ImportDataForScenarios <- function(OutDirPath,Scenario){
    dirfiles <- dir(OutDirPath)
    scendirfiles <- dirfiles[str_detect(dirfiles, Scenario)]
    scenDataList <- list()
    for (fn in scendirfiles){
        print(fn)
        pathf <- paste0(OutDirPath,'/',fn)
        scenDataList[[fn]] <- ImportData(pathf)
    }
    return(scenDataList)
}

# Create data import function
ImportData <- function(filePath) {
    filesList <- dir(filePath)
    alldata <- lapply(filesList, function(x) fread(paste0(filePath,'/',x)))
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

createLongData <- function(inputData, series){
    datalist <- lapply(names(inputData), function(x) {
        df <- inputData[[x]][[series]]
        df <- df %>% mutate(Scenario = x)
    })
    RunsCombo <- do.call(rbind, datalist)
    return(RunsCombo)
}
