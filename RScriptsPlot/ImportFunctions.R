library(dplyr)
library(data.table)
library(stringr)

# Import output data from a parameteric sensitivity analysis
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

# Import model output data from scenario runs
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

createSensitivityFileKey <- function(term1, term2) {
    Ranges1 = c(0.2,0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2)
    if (term2 != "none"){
        Ranges2 = c(0.2,0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2)
    } else {
        Ranges2 = c()
    }
    counti <- 0
    if (term2 != "none") {
        for (i in Ranges1) {
            for (j in Ranges2){
                tl <- paste0(term1, "Sensitivity", i, "_", term2, "Sensitivity", j)
                if (counti > 0){
                    tempList <- rbind(tempList, cbind(file = tl, term1 = i, term2 = j))
                } else {
                    tempList <- cbind(file = tl, term1 = i, term2 = j)
                }
                counti <- counti + 1
            }
        }
    } else {
        for (i in Ranges1) {
            tl <- paste0(term1, "Sensitivity", i)
            if (counti > 0){
                tempList <- rbind(tempList, cbind(file = tl, term1 = i))
            } else {
                tempList <- cbind(file = tl, term1 = i)
            }
            counti <- counti + 1
        }
    }
    return(tempList)
}

createLongDataSensitivtyAnalysis <- function(term1, term2, dataList, series){
    dataKey <- createSensitivityFileKey(term1, term2)
    listNames <- dataKey[,'file']
    if (term2 != "none") {
        datalist <- lapply(listNames, function(x) {
            df <- dataList[[x]][[series]]
            df <- df %>% mutate(term1 = dataKey[listNames == x,"term1"], term2 = dataKey[listNames == x,"term2"])
            names(df)[names(df) == 'term1'] <- term1
            names(df)[names(df) == 'term2'] <- term2
            return(df)
        })
    } else {
        datalist <- lapply(listNames, function(x) {
            df <- dataList[[x]][[series]]
            df <- df %>% mutate(term1 = dataKey[listNames == x,"term1"])
            names(df)[names(df) == 'term1'] <- term1
            return(df)
        })
    }
    RunsCombo <- do.call(rbind, datalist)
    return(RunsCombo)
}

createLongData <- function(inputData, series){
    datalist <- lapply(names(inputData), function(x) {
        df <- inputData[[x]][[series]]
        df <- df %>% mutate(Scenario = x)
    })
    RunsCombo <- do.call(rbind, datalist)
    return(RunsCombo)
}
