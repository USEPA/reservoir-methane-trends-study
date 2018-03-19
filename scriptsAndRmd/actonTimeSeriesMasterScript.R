
#load libraries:
source("scriptsAndRmd/GRTS/masterLibraryActonGRTS.R")

#load GLEON dissolved & saturated gas concentration code 
source("scriptsAndRmd/def.calc.sdg.R")

#load raw data files: GC, LGR, eddyPro:
source("scriptsAndRmd/compileGcDataNonGrts.R")  
        #loads GC data from master file on the lablan, 
        #loads the dissolved gas sample field data from the file
        # "actonEddyCovariance/gasTransferVelocity/dissolvedGasSampleCodes.xlsx" 
        # on the L: drive, puts them together into a dataframe called 
        # actonDgJoin
source("scriptsAndRmd/GRTS/readLgrActonGRTS.R")
        #reads in raw LGR files. Modified so that the script only 
        #searches in the Acton subfolder of the GGA data directory
source("scriptsAndRmd/loadEddyPro.R")
source("scriptsAndRmd/loadVWS_RBR.R") 
        #load the vanni weather station, buoy T, and RBR thermistor data


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

#remove non longer needed data frames and lists:
rm(vanniMet, vanniMetSub, txtFilesSize, OUT, rbrT, ggaGRTS1, 
   gga.model,gga.i,ep.i, data.i.co2, data.i.ch4, data.i, 
   buoyT, buoyT30min, adjDataDf)
rm(ch4.ex.pred, chmVol.L.i, co2.ex.pred, gga, epList, 
   ggaList)
