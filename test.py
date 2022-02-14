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
	supplyTSDict = core.ProcessData.ImportStoreTimeSeriesData('data/Stores.csv')
	marketingTSDict = core.ProcessData.ImportTimeSeriesData('data/MarketingEfforts.csv')

	# Build Agents
	storeDict, storeList = core.AgentProcs.BuildStores(inventoryDict)
	agentList = core.AgentProcs.BuildAgents(popInputDict, cohortAttributeInputDict, visitInputDict, foodPrefInputDict)
	
	# Baseline run
	FMod_Base = core.Model.FoodModel(globalInputDict, foodPricesDict, supplyTSDict, marketingTSDict, agentList, storeList, storeDict)
	FMod_Base.RunModel()

	# Print Output
	outputDict, zipcode, poverty, race, income = core.OutputFunctions.BreakdownFoodConsumption(popInputDict, agentList, FMod_Base.iterations)
	core.OutputFunctions.ExportFoodConsumptionOutput(outputDict, 'Test', 'Poverty', poverty, zipcode)
	core.OutputFunctions.ExportFoodConsumptionOutput(outputDict, 'Test', 'Race', race, zipcode)
	core.OutputFunctions.ExportFoodConsumptionOutput(outputDict, 'Test', 'Income', income, zipcode)

if __name__ == "__main__":
	# execute only if run as a script
	main(sys.argv[1:])