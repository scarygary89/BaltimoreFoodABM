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

	nruns = 20

	for i in range(nruns):
		print('Starting Run ' + str(i))
		print('Running Baseline Scenario...')
		RunScenarioZipcode('Baseline' + str(i), globalInputDict, popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict, foodPricesDict, inventoryDict, supplyTSDict, noMarketingTSDict)

		print('Running Meatless Monday Scenario...')
		RunScenarioZipcode('MeatlessMondayMarketing' + str(i), globalInputDict, popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict, foodPricesDict, inventoryDict, supplyTSDict, meatlessMarketingTSDict)

		print('Running Increase in Meatless Options Scenario...')
		RunScenarioZipcode('MoreMeatlessOptions' + str(i), globalInputDict, popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict, foodPricesDict, inventoryMoreMeatlessDict, supplyTSDict, noMarketingTSDict)

		print('Running Meat Prices Spike Scenario...')
		RunScenarioZipcode('PriceSurge' + str(i), globalInputDict, popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict, meatPricesSpikeDict, inventoryDict, supplyTSDict, noMarketingTSDict)
		
		print('Supply Chain Shock Scenario...')
		RunScenarioZipcode('SupplyShock' + str(i), globalInputDict, popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict, foodPricesDict, inventoryDict, supplyShockTSDict, noMarketingTSDict)

		print('Running Comprehensive Marketing Scenario...')
		RunScenarioZipcode('ComprehensiveMarketing' + str(i), globalInputDict, popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict, foodPricesDict, inventoryMoreMeatlessDict, supplyTSDict, meatlessMarketingTSDict)

		print('Running COVID-19 Scenario...')
		RunScenarioZipcode('COVID' + str(i), globalInputDict, popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict, meatPricesSpikeDict, inventoryDict, supplyShockTSDict, noMarketingTSDict)

	print('Simulations Finished!')


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