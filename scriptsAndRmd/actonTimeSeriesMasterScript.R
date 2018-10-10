
#load libraries:
source("scriptsAndRmd/GRTS/masterLibraryActonGRTS.R")

#load GLEON dissolved & saturated gas concentration code 
source("scriptsAndRmd/def.calc.sdg.R")

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
        #doesn't filter or change
  
source("scriptsAndRmd/loadVWS_RBR.R") 
        #load the vanni weather station, buoy T, campbell met, and RBR thermistor data, 
              # vws file that is loaded: vws20160929_20181002_concat.csv
              # RBR file that is loaded: RBR20170510_20180827.csv
              # buoy T file that is loaded: vanniBuoyTmpr.csv (just 2018 data)
              # campbell met file that is loaded: CR6Series_BioMet20180814.csv
        #turn 15-min VWS readings into 30-min averages,
        #adjust the level offset in the VWS dataset,
        #turn the 15-min buoy T readings into 30-min averages
source("scriptsAndRmd/readHobo.R")
        #reads in all of the hobo files, from Acton and Harsha
        #updated 2018-08-23 to read both 2017 and 2018 files


#turn raw data into data products: dissolved/sat gas, chamber fluxes
source("scriptsAndRmd/dissolvedGasDiffCalc.R")
        #takes actonDgJoin, reformats it and uses the GLEON code to 
        #calculate the dissolved/sat gas concentrations, stored
        #in data frame "actonDGoutput"
source("scriptsAndRmd/plotCleanLgrActon.R")
        #reads in the field data from the file "actonEddyCovariance/survey/chamberBiweekly.xlsx"
        #optimizes start and end times for the chamber calcs
source("scriptsAndRmd/calculateChamberEmissions.R")
        #produces chamData and chamDataSub
source("scriptsAndRmd/qcEddyPro.R") #makes epOutSub, filters data by QC parameters
        #makes DailyEcFluxes and MonthlyCh4
source("scriptsAndRmd/calculateEbEmissions.R")
        #calculates time series of ebullition emissions
        #from the active trap data

#remove non longer needed data frames and lists:
rm(vanniMet, vanniMetSub, txtFilesSize, OUT, rbrT, ggaGRTS1, 
   gga.model,gga.i,ep.i, data.i.co2, data.i.ch4, data.i, 
   buoyT, adjDataDf)
rm(ch4.ex.pred, chmVol.L.i, co2.ex.pred, gga, epList, 
   ggaList, dupes)
rm(metaDataTrap, metaDataDG)
