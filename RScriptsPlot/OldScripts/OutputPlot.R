library(dplyr)
library(data.table)
library(ggplot2)
library(scales)
library(fishualize)

# Baseline
setwd('C:/Users/GaryLin/Dropbox/MarylandFoodModel/ABM/TestABM/Output/Baseline')

filesList_base = dir()
alldata_base <- lapply(filesList_base, function(x) fread(x))
names(alldata_base) <- filesList_base


racedata_base <- alldata_base[grepl("Race",names(alldata_base))]
races_base <- sub(".csv", "",sub(".*Race_", "", names(racedata_base)))
racezips_base <- substring(sub(".*Baseline", "", names(racedata_base)),1,5)
Racemat_base <- do.call(rbind, lapply(1:length(racedata_base), function(x){
	df <- racedata_base[[x]]
	df <- cbind(df, race = races_base[x], zipcode = racezips_base[x])
	colnames(df)[1] <- 'Meal'
	return(df)
}))

incomedata_base <- alldata_base[grepl("Income",names(alldata_base))]
incomes_base <- sub(".csv", "",sub(".*Income_", "", names(incomedata_base)))
incomezips_base <- substring(sub(".*Baseline", "", names(incomedata_base)),1,5)
Incomemat_base <- do.call(rbind, lapply(1:length(incomedata_base), function(x){
	df <- incomedata_base[[x]]
	df <- cbind(df, income = incomes_base[x], zipcode = incomezips_base[x])
colnames(df)[1] <- 'Meal'
	return(df)
}))

povertydata_base <- alldata_base[grepl("Poverty",names(alldata_base))]
povertys_base <- sub(".csv", "",sub(".*Poverty_", "", names(povertydata_base)))
povertyzips_base <- substring(sub(".*Baseline", "", names(povertydata_base)),1,5)
Povertymat_base <- do.call(rbind, lapply(1:length(povertydata_base), function(x){
	df <- povertydata_base[[x]]
	df <- cbind(df, poverty = povertys_base[x], zipcode = povertyzips_base[x])
	colnames(df)[1] <- 'Meal'
	return(df)
}))

#Meatless Monday
setwd("C:/Users/GaryLin/Dropbox/MarylandFoodModel/ABM/TestABM/Output/MeatlessMonday")

filesList_meatless = dir()
alldata_meatless <- lapply(filesList_meatless, function(x) fread(x))
names(alldata_meatless) <- filesList_meatless


racedata_meatless <- alldata_meatless[grepl("Race",names(alldata_meatless))]
races_meatless <- sub(".csv", "",sub(".*Race_", "", names(racedata_meatless)))
racezips_meatless <- substring(sub(".*MeatlessMonday", "", names(racedata_meatless)),1,5)
Racemat_meatless <- do.call(rbind, lapply(1:length(racedata_meatless), function(x){
	df <- racedata_meatless[[x]]
	df <- cbind(df, race = races_meatless[x], zipcode = racezips_meatless[x])
	colnames(df)[1] <- 'Meal'
	return(df)
}))

incomedata_meatless <- alldata_meatless[grepl("Income",names(alldata_meatless))]
incomes_meatless <- sub(".csv", "",sub(".*Income_", "", names(incomedata_meatless)))
incomezips_meatless <- substring(sub(".*MeatlessMonday", "", names(incomedata_meatless)),1,5)
Incomemat_meatless <- do.call(rbind, lapply(1:length(incomedata_meatless), function(x){
	df <- incomedata_meatless[[x]]
	df <- cbind(df, income = incomes_meatless[x], zipcode = incomezips_meatless[x]) 
	colnames(df)[1] <- 'Meal'
	return(df)
}))

povertydata_meatless <- alldata_meatless[grepl("Poverty",names(alldata_meatless))]
povertys_meatless <- sub(".csv", "",sub(".*Poverty_", "", names(povertydata_meatless)))
povertyzips_meatless <- substring(sub(".*MeatlessMonday", "", names(povertydata_meatless)),1,5)
Povertymat_meatless <- do.call(rbind, lapply(1:length(povertydata_meatless), function(x){
	df <- povertydata_meatless[[x]]
	df <- cbind(df, poverty = povertys_meatless[x], zipcode = povertyzips_meatless[x])
	colnames(df)[1] <- 'Meal'
	return(df)
}))


Racemat <- cbind(Racemat_meatless, scenario = 'Meatless') %>%
	rbind(cbind(Racemat_base, scenario = 'Baseline')) %>%
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



Incomemat <- cbind(Incomemat_meatless, scenario = 'Meatless') %>%
	rbind(cbind(Incomemat_base, scenario = 'Baseline')) %>% 
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
	) %>% 
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


Povertymat <- cbind(Povertymat_meatless, scenario = 'Meatless') %>%
	rbind(cbind(Povertymat_base, scenario = 'Baseline')) %>% 
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
	) %>% 
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

mRacemat <- melt(Racemat_avg, id.vars = c('Meal','race','scenario')) %>% mutate(value=as.numeric(value))
mIncomemat <- melt(Incomemat, id.vars = c('Meal','income','zipcode','scenario'))
mPovertymat <- melt(Povertymat, id.vars = c('Meal','poverty','zipcode','scenario'))

tplot_Race <- ggplot(mRacemat %>% filter(variable == 'MeatPerc'), 
	aes(x=Meal, y=value, group = scenario, color = scenario)) + 
	geom_line() + 
	facet_wrap(race~., scales='free') +
	theme_bw() + 
	scale_y_continuous(labels = scales::percent)

windows()
print(tplot_Race)

tplot_Income <- ggplot(mIncomemat %>% filter(variable == 'MeatPerc'), 
	aes(x=Meal, y=value, group = variable, color = variable)) + 
	geom_line() + 
	facet_grid(income~zipcode, scales='free') +
	theme_bw() + 
	scale_y_continuous(labels = scales::percent)

windows()
print(tplot_Income)

tplot_Poverty <- ggplot(mPovertymat %>% filter(variable == 'MeatPerc'),
	aes(x=Meal, y=value, group = variable, color = variable)) +
	geom_line() + 
	facet_grid(poverty~zipcode, scales='free') +
	theme_bw() + 
	scale_y_continuous(labels = scales::percent)

windows()
print(tplot_Poverty)

# pie chart
mAvgRacemat <- mRacemat %>% group_by(race, variable, scenario ) %>% summarize(value = mean(value))
mAvgIncomemat <- mIncomemat %>% group_by(income, variable, scenario ) %>% summarize(value = mean(value))
mAvgPovertymat <- mPovertymat %>% group_by(poverty, variable, scenario ) %>% summarize(value = mean(value))

mAvg <- mRacemat %>% group_by(variable ) %>% summarize(value = mean(value))

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
mRacemat <- melt(Racemat_avg, id.vars = c('Meal','race','scenario', 'zipcode')) %>% mutate(value=as.numeric(value))
mAvgRacemat <- mRacemat %>% group_by(variable, race, scenario,zipcode ) %>% summarize(value = mean(value))
mAvgIncomemat <- mIncomemat %>% group_by(income, zipcode, variable, scenario ) %>% summarize(value = sum(value))
mAvgPovertymat <- mPovertymat %>% group_by(poverty, zipcode, variable, scenario ) %>% summarize(value = sum(value))

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

