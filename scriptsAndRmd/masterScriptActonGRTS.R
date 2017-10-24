
# Load libraries and functions
source("scriptsAndRmd/masterLibraryActonGRTS.R")

# Read raw data
source("scriptsAndRmd/readSitesEqAreaDataActonGRTS.R") # Reads shapefiles, 30s
source("scriptsAndRmd/compileGcDataActon.R") # GC data, merges with eqAreaData, 15s
source("scriptsAndRmd/readLgrActonGRTS.R") # Reads in raw LGR data, merge w/CowanGC, 16s
source("ohio2016/scriptsAndRmd/readChem.R") # Merges with eqAreaData
source("ohio2016/scriptsAndRmd/readChl.R") # Merges with eqAreaData

# Calculate derived quantities
source("ohio2016/scriptsAndRmd/plotCleanLgr.R") # Merges chamber time with eqAreaData, 10min
source("ohio2016/scriptsAndRmd/calculateEmissions.R") # Merges with eqAreaData, 3min

# grts calculations
source("ohio2016/scriptsAndRmd/grtsWgtAdj.R") # Merges with eqAreaData, 2s
source("ohio2016/scriptsAndRmd/grtsMeanVariance.R") # 20s

# Data analysis
source("ohio2016/scriptsAndRmd/descRes.R")
source("ohio2016/scriptsAndRmd/exploratoryPlots.R")
