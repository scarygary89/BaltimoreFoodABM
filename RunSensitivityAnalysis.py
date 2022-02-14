"""
Food System Dynamics Model
Author: Gary Lin

Packages needed:
- numpy
- scipy
"""

import sys
import numpy as np
import os
import pandas as pd
from datetime import timedelta
from datetime import datetime
import multiprocessing as mp

import core.ProcessData
import core.Model
import core.AgentProcs
import core.PlotFunctions
import core.OutputFunctions

def main(argv):
	print('Initializing Baltimore Food System Agent Based Model...')
	try:
		os.mkdir('Output')
	except:
		pass
	globalInputDict = core.ProcessData.ImportGlobalParmsData('data/GlobalParms.csv')

	# Import Agent Data
	popInputDict = core.ProcessData.ImportZipPopData('data/ZipPopulation.csv')
	cohortAttributeInputDict = core.ProcessData.ImportCohortParmsData('data/CohortParms.csv')
	visitInputDict = core.ProcessData.ImportVisitData('data/Visits.csv')
	foodPrefInputDict = core.ProcessData.ImportFoodPreferences('data/CohortFoodPreferences.csv')

	# Import Environmental, Pricing, and Marketing Data
	inventoryDict = core.ProcessData.ImportStoreData('data/StoreInventory.csv')
	inventoryMoreMeatlessDict = core.ProcessData.ImportStoreData('data/StoreInventoryMoreMeatless.csv')
	foodPricesDict = core.ProcessData.ImportTimeSeriesData('data/FoodPrices.csv')
	meatPricesSpikeDict = core.ProcessData.ImportTimeSeriesData('data/MeatPricesSpike.csv')
	supplyTSDict = core.ProcessData.ImportStoreTimeSeriesData('data/Supply.csv')
	supplyShockTSDict = core.ProcessData.ImportStoreTimeSeriesData('data/SupplyShock.csv')
	noMarketingTSDict = core.ProcessData.ImportTimeSeriesData('data/MarketingEfforts.csv')
	meatlessMarketingTSDict = core.ProcessData.ImportTimeSeriesData('data/MeatlessMondayMarketing.csv')

	SensitivityMultiplier = [.25, .5, .75, 1, 1.25, 1.5, 1.75]
	
	# marketing
	globalInputDict_market = globalInputDict.copy()
	for mult in SensitivityMultiplier:
		runName = "MarketingSensitivity" + str(mult)
		globalInputDict_market['MarketingMultiplier'] = globalInputDict['MarketingMultiplier'] * mult
		print('Running Sensitivity Analysis for Marketing with Multiplier = ' + str(globalInputDict_market['MarketingMultiplier']))
		RunScenarioZipcode(runName, globalInputDict_market, popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict, foodPricesDict, inventoryDict, supplyTSDict, meatlessMarketingTSDict)

	# increase in meatless
	globalInputDict_availability = globalInputDict.copy()
	for mult in SensitivityMultiplier:
		runName = "AvailabilitySensitivity" + str(mult)
		globalInputDict_availability['AvailabilityMultiplier'] = globalInputDict['AvailabilityMultiplier'] * mult
		print('Running Sensitivity Analysis for  Availability with Multiplier = ' + str(globalInputDict_availability['AvailabilityMultiplier']))
		RunScenarioZipcode(runName, globalInputDict_availability, popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict, foodPricesDict, inventoryMoreMeatlessDict, supplyTSDict, noMarketingTSDict)

	# price
	globalInputDict_price = globalInputDict.copy()
	for mult in SensitivityMultiplier:
		runName = "PricingSensitivity" + str(mult)
		globalInputDict_price['PriceMultiplier'] = globalInputDict['PriceMultiplier'] * mult
		print('Running Sensitivity Analysis for Pricing with Multiplier = ' + str(globalInputDict_price['PriceMultiplier']))
		RunScenarioZipcode(runName, globalInputDict_price, popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict, meatPricesSpikeDict, inventoryDict, supplyTSDict, noMarketingTSDict)

	# supply shortage
	globalInputDict_price = globalInputDict.copy()
	for mult in SensitivityMultiplier:
		runName = "SupplySensitivity" + str(mult)
		globalInputDict_price['AvailabilityMultiplier'] = globalInputDict['AvailabilityMultiplier'] * mult
		print('Running Sensitivity Analysis for Pricing with Multiplier = ' + str(globalInputDict_price['AvailabilityMultiplier']))
		RunScenarioZipcode(runName, globalInputDict_price, popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict, foodPricesDict, inventoryDict, supplyTSDict, noMarketingTSDict)

	# marketing + increase in meatless availability (comprehensive marketing)
	globalInputDict_marketavail = globalInputDict.copy()
	for mult1 in SensitivityMultiplier:
		for mult2 in SensitivityMultiplier:
			runName = "AvailabilitySensitivity" + str(mult1) + '_' + "MarketingSensitivity" + str(mult2)
			globalInputDict_marketavail['AvailabilityMultiplier'] = globalInputDict['AvailabilityMultiplier'] * mult1
			globalInputDict_marketavail['MarketingMultiplier'] = globalInputDict['MarketingMultiplier'] * mult2
			print('Running Sensitivity Analysis for Availability with Multiplier = ' + str(globalInputDict_marketavail['AvailabilityMultiplier']))
			print('Running Sensitivity Analysis for Marketing with Multiplier = ' + str(globalInputDict_marketavail['MarketingMultiplier']))
			RunScenarioZipcode(runName, globalInputDict_marketavail, popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict, foodPricesDict, inventoryMoreMeatlessDict, supplyTSDict, meatlessMarketingTSDict)

	# price + supply shortage (covid)
	globalInputDict_priceavail = globalInputDict.copy()
	for mult1 in SensitivityMultiplier:
		for mult2 in SensitivityMultiplier:
			runName = "AvailabilitySensitivity" + str(mult1) + '_' + "PricingSensitivity" + str(mult2)
			globalInputDict_priceavail['AvailabilityMultiplier'] = globalInputDict['AvailabilityMultiplier'] * mult1
			globalInputDict_priceavail['PriceMultiplier'] = globalInputDict['PriceMultiplier'] * mult2
			print('Running Sensitivity Analysis for Availability with Multiplier = ' + str(globalInputDict_priceavail['AvailabilityMultiplier']))
			print('Running Sensitivity Analysis for Pricing with Multiplier = ' + str(globalInputDict_priceavail['PriceMultiplier']))
			RunScenarioZipcode(runName, globalInputDict_priceavail, popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict, meatPricesSpikeDict, inventoryDict, supplyShockTSDict, noMarketingTSDict)

	print('Finished!')


def RunScenarioZipcode(RunName, globalInputDict, popDict, cohortAttDict, visitDict, foodPrefDict, foodPricesDict, inventoryDict, supplyTSDict, marketingTSDict):
	cpus = mp.cpu_count()
	poolCount = cpus*2 - 2
	zipcodeList, povertyList, raceList, incomeList = core.ProcessData.ExtractZipcodesPovertyRaceIncome(popDict)
	args = []
	for runID in range(len(zipcodeList)):
		inputDict = {}
		inputDict['RunName'] = RunName
		inputDict['zip'] = zipcodeList[runID]
		inputDict['cohortAttDict'] = cohortAttDict
		inputDict['visitDict'] = visitDict
		inputDict['foodPrefDict'] = foodPrefDict
		inputDict['globalInputDict'] = globalInputDict
		inputDict['foodPricesDict'] = foodPricesDict
		inputDict['inventoryDict'] = inventoryDict
		inputDict['supplyTSDict'] = supplyTSDict
		inputDict['marketingTSDict'] = marketingTSDict
		inputDict['population'] = core.ProcessData.ExtractZipFromPopDict(inputDict['zip'], popDict)
		args.append((inputDict))

	pool = mp.Pool(processes = poolCount)
	pool.map_async(RunProc, args)
	pool.close()
	pool.join()


def RunProc(inputDict):
	print('Initializing Job for Zip Code ' + str(inputDict['zip']))
	storeDict, storeList = core.AgentProcs.BuildStores(inputDict['inventoryDict'])
	agents = core.AgentProcs.BuildAgents(inputDict['population'], inputDict['cohortAttDict'], inputDict['visitDict'], inputDict['foodPrefDict'])
	model = core.Model.FoodModel(inputDict['globalInputDict'], inputDict['foodPricesDict'], inputDict['supplyTSDict'], inputDict['marketingTSDict'], agents, storeList, storeDict)
	model.RunModel(inputDict['zip'])
	outputDict, zipcode, poverty, race, income = core.OutputFunctions.BreakdownFoodConsumption(inputDict['population'], model.agentList, model.iterations)
	core.OutputFunctions.ExportFoodConsumptionOutput(outputDict, inputDict['RunName'], 'Poverty', poverty, zipcode)
	core.OutputFunctions.ExportFoodConsumptionOutput(outputDict, inputDict['RunName'], 'Race', race, zipcode)
	core.OutputFunctions.ExportFoodConsumptionOutput(outputDict, inputDict['RunName'], 'Income', income, zipcode)


if __name__ == "__main__":
	# execute only if run as a script
	main(sys.argv[1:])