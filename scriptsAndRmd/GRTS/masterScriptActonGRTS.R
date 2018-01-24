
# Load libraries and functions
source("scriptsAndRmd/GRTS/masterLibraryActonGRTS.R")

# Read raw data
source("scriptsAndRmd/GRTS/readSitesEqAreaDataActonGRTS.R") # Reads shapefiles, 30s
source("scriptsAndRmd/compileGcDataActon.R") # GC data, merges with eqAreaData, 15s
source("scriptsAndRmd/GRTS/readLgrActonGRTS.R") # Reads in raw LGR data, need to make sure all LGR files are unzipped

#source("ohio2016/scriptsAndRmd/readChem.R") # Merges with eqAreaData
#source("ohio2016/scriptsAndRmd/readChl.R") # Merges with eqAreaData

# Calculate derived quantities
source("scriptsAndRmd/GRTS/plotCleanLgrActonGRTS.R") # Merges chamber time with eqAreaData, 1min
source("scriptsAndRmd/GRTS/calculateEmissionsActonGRTS.R") # Merges with eqAreaData, 3min

# grts calculations
source("scriptsAndRmd/GRTS/grtsWgtAdjActonGRTS.R") # Merges with eqAreaData, 2s
source("scriptsAndRmd/GRTS/grtsMeanVarianceActonGRTS.R") # 20s

# Data analysis
#source("scriptsAndRmd/descRes.R")
source("scriptsAndRmd/exploratoryPlotsActonGrts.R")
