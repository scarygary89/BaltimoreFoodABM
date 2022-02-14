"""
Food System Dynamics Model
Author: Gary Lin

Packages needed:
- numpy
- scipy
"""

import sys
import math
import numpy as np
import os
import pandas as pd
from datetime import timedelta
from datetime import datetime

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
	foodPricesDict = core.ProcessData.ImportTimeSeriesData('data/FoodPrices.csv')
	storeTSDict = core.ProcessData.ImportStoreTimeSeriesData('data/Stores.csv')
	noMarketingTSDict = core.ProcessData.ImportTimeSeriesData('data/MarketingEfforts.csv')
	meatlessMarketingTSDict = core.ProcessData.ImportTimeSeriesData('data/MeatlessMondayMarketing.csv')

	# Baseline run
	print('Running Baseline Scenario...')
	agentList_base = core.AgentProcs.BuildAgents(popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict)
	FMod_Base = core.Model.FoodModel(globalInputDict, foodPricesDict, inventoryDict, storeTSDict, noMarketingTSDict, agentList_base)
	FMod_Base.RunModel()
	outputDict, zipcode, poverty, race, income = core.OutputFunctions.BreakdownFoodConsumption(popInputDict, FMod_Base.agentList, FMod_Base.iterations)
	core.OutputFunctions.ExportFoodConsumptionOutput(outputDict, 'Baseline', 'Poverty', poverty, zipcode)
	core.OutputFunctions.ExportFoodConsumptionOutput(outputDict, 'Baseline', 'Race', race, zipcode)
	core.OutputFunctions.ExportFoodConsumptionOutput(outputDict, 'Baseline', 'Income', income, zipcode)

	# Meatless Monday run
	print('Running Meatless Monday Scenario...')
	agentList_meatless = core.AgentProcs.BuildAgents(popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict)
	FMod_meatless = core.Model.FoodModel(globalInputDict, foodPricesDict, inventoryDict, storeTSDict, meatlessMarketingTSDict, agentList_meatless)
	FMod_meatless.RunModel()

	# Print Output
	outputDict, zipcode, poverty, race, income = core.OutputFunctions.BreakdownFoodConsumption(popInputDict, FMod_meatless.agentList, FMod_meatless.iterations)
	core.OutputFunctions.ExportFoodConsumptionOutput(outputDict, 'MeatlessMonday', 'Poverty', poverty, zipcode)
	core.OutputFunctions.ExportFoodConsumptionOutput(outputDict, 'MeatlessMonday', 'Race', race, zipcode)
	core.OutputFunctions.ExportFoodConsumptionOutput(outputDict, 'MeatlessMonday', 'Income', income, zipcode)

if __name__ == "__main__":
    # execute only if run as a script
    main(sys.argv[1:])