
#load libraries:
source("scriptsAndRmd/masterLibraryActon.R")



#load GLEON dissolved & saturated gas concentration code 
source("scriptsAndRmd/def.calc.sdg.R")

######Loading Scripts. Skip to reading saved files unless
######starting from scratch is the point -------
#load raw data files: GC, LGR, eddyPro, hobo:
source("scriptsAndRmd/compileGcDataNonGrts.R")  
        #loads GC data from master file on the lablan, 
        #loads the dissolved gas and trap sample field data from different tabs the file
        #"ReservoirEbullitionStudy/ebullition2017/data/masterDataSheetEbullition2017.xlsx" 
        #on the L: drive, puts them together into five dataframes called 
        #actonDgJoin, actonTrapJoin, dockAmbientAir, metaDataDCact, metaDataTrapAct
        #also calculates actonTrapAgg with aggregated mean and sd trap GHG #s

source("scriptsAndRmd/GRTS/readLgrActonGRTS.R")
        #reads in raw LGR files. Modified so that the script only 
        #searches in the Acton subfolder of the GGA data directory

source("scriptsAndRmd/loadEddyPro.R")
        #loads, deals with out of order/overlapping files by putting the
        # loaded data frame into order by date and deleting duplicate lines
        # doesn't filter or change
        # UPDATED 20190303 to load the reprocessed 30 min data, with 7500 T correction
        
rm(epList, epFiles, txtFiles2017, txtFiles2018)  

source("scriptsAndRmd/loadVWS_RBR.R") 
        #load the vanni weather station, buoy T, campbell met, and RBR thermistor data, 
              # vws file that is loaded: vws20160929_20181109_concat.csv
              # RBR file that is loaded: RBR20170510_20181214.csv
              # buoy T files that are loaded: 
                      #2017_ThermistorData.csv
                      #2018_ThermistorData.csv
              # campbell met file that is loaded: CR6Series_BioMet20181113.csv
        #turn 15-min VWS readings into 30-min averages,
        #adjust the level offset in the VWS dataset,
        #turn the 15-min buoy T readings into 30-min averages
source("scriptsAndRmd/readHobo.R")
        #reads in all of the hobo files, from Acton and Harsha
        #updated 2018-08-23 to read both 2017 and 2018 files
#######-----


#source("scriptsAndRmd/loadPrelimOutputs.R")
        #loads data into dataframes: 
            #DISSOLVED GAS: actonDgJoin, actonTrapJoin, actonTrapAgg
            #LGR CHAMBER: gga
            #EDDY COVARIANCE: epOutOrder
            #METEOROLOGICAL & WATER T: vanni30min, rbrTsub, rbrDaily, buoyT30min, buoyTdaily, U12sonde, campMet
            #ACTIVE BUBBLE TRAPS: hobo

#turn raw data into data products: dissolved/sat gas, chamber fluxes
source("scriptsAndRmd/dissolvedGasDiffCalc.R")
        #takes actonDgJoin, reformats it and uses the GLEON code to 
        #calculate the dissolved/sat gas concentrations, stored
        #in data frame "actonDGoutput"
source("scriptsAndRmd/plotCleanLgrActon.R")
        #reads in the field data from the file "actonEddyCovariance/survey/chamberBiweekly.xlsx"
        #optimizes start and end times for the chamber calcs
        #up to 2018-11-15 visit have been time adjusted
source("scriptsAndRmd/calculateChamberEmissions.R")
        #produces chamData and chamDataSub
source("scriptsAndRmd/qcEddyPro.R") 
        #makes epOutSub, filters data by QC parameters
        #makes DailyEcFluxes and MonthlyCh4

        #ADDED FEB 2019:
        #makes the qa-filtered, continuous, labeled data frame "epREddy" 
        #to be used in the
        #rEddyProc.R script to gap-fill with MDC
source("scriptsAndRmd/calculateEbEmissions.R")
        #calculates time series of ebullition emissions
        #from the active trap data
#remove non longer needed data frames and lists:
rm(vanniMet, txtFilesSize, OUT, rbrT, ggaGRTS1, 
   gga.model,gga.i,ep.i, data.i.co2, data.i.ch4, data.i, 
   buoyT, adjDataDf)
rm(ch4.ex.pred, chmVol.L.i, co2.ex.pred, gga, 
   ggaList, dupes)
rm(metaDataTrap, metaDataDG)

file.edit('scriptsAndRmd/rEddyProc.R') #mean diurnal course gap-filling for LE, H, ustar 
                                       #preps input into ANN
file.edit('scriptsAndRmd/ANN/evaluateANN2019.R') 
## get the gap-filled data
## make Figure 8 (VIF)

## Figures:
file.edit('scriptsAndRmd/edi.256.1.r') #vanni stream gauge data, figure 2 d
file.edit('scriptsAndRmd/metPlotsFig2.R')#figure 2 a-c, d-e
file.edit('scriptsAndRmd/hydroDynamicsVanniBuoy.R') #Figure 2 e&f
file.edit('scriptsAndRmd/cumulativeTS.R')
## Multi-panel time series figure showing:
##  - 30 min measured, 30 min gap-filled, and daily avg EC results
##  - 2 hr measured AFT, daily avg AFT + interpolated chamber for U-14
##  - 2 hr measured AFT, daily avg AFT + interpolated chamber for U-12
##  - GRTS results
##  - AKA Figure 3
## Two-panel figure showing 2017 and 2018 cumulative FCH4 (Fig 7)
file.edit('scriptsAndRmd/fluxTmprPlots.R')
    ## Multi-panel plots showing Q10 and 
file.edit('scriptsAndRmd/ecFluxAnalysisPlots.R')

file.edit('scriptsAndRmd/diurnalAnalysis.R') #makes diurnal pdfs
file.edit('scriptsAndRmd/GRTS/exploratoryPlotsActonGRTS.R') 
    ## Makes linear regression of ebullition as f(depth) and
    ## ratio of ebullitive FCH4:total FCH4 (Figures 5 and 6)

#run fluxTmprPlots to make plots used in AGU poster
# cumulativeTS.R is the script used to transform time series flux data into 
##### cumulative emission estimates



#gapfilling scripts:

