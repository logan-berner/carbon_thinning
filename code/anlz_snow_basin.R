# THIS R SCRIPT 
# AUTHOR: LOGAN BERNER
# DATE: 2020-05-10

rm(list=ls())
require(R.utils)
require(ggplot2)
require(ggpubr)
require(data.table)
require(maptools)
require(raster)
require(rgdal)
setwd('C:/Users/Logan/Google Drive/research/side_projects/mildrexler_oregon_forests/eastside_screen/')
nad83 <- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")

# LOAD DATA SETS ----------------------------------------------------------------------
sb.dt <- fread('data/snow_basin_plan.csv')
trees.dt <- fread('data/fia/WAORID_fia_tree_surveys.csv')
plots.dt <- fread('data/fia/WAORID_fia_plots.csv')
sp.codes.dt <- fread('data/fia/REF_SPECIES.CSV')
sb.shp <- readOGR('data/gis_data/SnowBasinHCPC/ProjectBoundary.shp')
nf.shp <- readOGR('data/gis_data/eastside_national_forests.shp')

# IDENTIFY PLOTS TO USE IN ANALYSIS ----------------------------------------------------------------------
names(plots.dt) <- tolower(names(plots.dt))
plots.dt <- plots.dt[, plot.id := paste(statecd, plot, sep='.')] # create unique identifier
plots.dt[, measyear.recent := max(measyear), by = plot.id] # compute most recent year of survey at each plot
plots.dt <- plots.dt[measyear == measyear.recent] # take most recent survey
plots.dt <- plots.dt[measyear >= 2010] # use plots meaasured since 2010

# spatialize plots and double check alignment
plots.pt <- SpatialPointsDataFrame(coords = data.frame(plots.dt$lon, plots.dt$lat), data = plots.dt, proj4string = nad83)

# determine number of FIA plots in Snow Basin project area. 
sb.nad83.shp <- spTransform(sb.shp, CRSobj = nad83) 
plots.dt[, snow.basin := over(plots.pt, sb.nad83.shp)[,4]]
table(plots.dt$snow.basin)
plots.dt[, snow.basin := NULL]

# there are only 11 forested plots in project area, so use plot from whole Wallowa-Whitman Nat'l Forest
plots.dt[, nf.name := over(plots.pt, nf.shp)[,5]]
plots.dt <- plots.dt[nf.name == 'Wallowa-Whitman National Forest']
plots.dt <- plots.dt[plot_status_cd == 1] # "at least one accessible forest land condition present on plot"
keep.cols <- c('plot.id','lat','lon','measyear','invyr','nf.name')
plots.dt <- plots.dt[, ..keep.cols]


# ADD ANCILLARY DATA TO TREE SURVEYS ---------------------------------------------------------------------
names(trees.dt) <- tolower(names(trees.dt))
names(sp.codes.dt) <- tolower(names(sp.codes.dt))
trees.dt <- trees.dt[, plot.id := paste(statecd, plot, sep='.')] # create plot id from state x plot number
trees.dt <- plots.dt[trees.dt, on = c('plot.id','invyr')] # add plot data
trees.dt <- trees.dt[is.na(nf.name) == F] # take trees from select natl forests

# add species name to each tree
sp.codes.dt <- sp.codes.dt[, c('spcd','common_name','genus')]
trees.dt <- sp.codes.dt[trees.dt, on = 'spcd']

# take only live tree from subplots that were surveyed (not microplots or macroplots)
trees.dt <- trees.dt[statuscd == 1] 
trees.dt <- trees.dt[tpa_unadj == 6.018046]

# take select cols from tree data table
keep.cols <- c('plot.id','measyear','lat','lon','nf.name','plot','subp','tree',
               'spcd','common_name','genus','dia','ht','carbon_ag')
trees.dt <- trees.dt[, ..keep.cols]

# rename some cols
setnames(trees.dt, c('common_name','dia','ht','carbon_ag'), c('common.name','dia.in','ht.in','agc.lb'))

# collapse some species into 'other' category
trees.dt[common.name != 'ponderosa pine' & common.name != 'Douglas-fir' & common.name != 'western larch' & common.name != 'grand fir', common.name := 'other']
trees.dt[, common.name := factor(common.name, levels = c('Douglas-fir','grand fir','ponderosa pine','western larch','other'))]


# ASSESS FRACTION OF LARGE TREES BY DBH FOR EACH SPECIES ON FIA PLOTS ----------------------------------------------------------------------
trees.dt <- trees.dt[common.name == 'ponderosa pine' | common.name == 'grand fir']
trees.dt[, dia.in.rnd := round(dia.in)]

# calculate 
lrg.tree.agc.by.sp.dbh.dt <- trees.dt[dia.in >= 21, .(n.stems = .N), by = c('common.name','dia.in.rnd')]
lrg.tree.agc.by.sp.dbh.dt <- lrg.tree.agc.by.sp.dbh.dt[, stem.frac := n.stems / sum(n.stems), by = c('common.name')]
lrg.tree.agc.by.sp.dbh.dt <- lrg.tree.agc.by.sp.dbh.dt[, n.stems := NULL]
lrg.tree.agc.by.sp.dbh.dt <- lrg.tree.agc.by.sp.dbh.dt[order(common.name,dia.in.rnd)]


# ESTIMATE SNOW BASIN REMOVAL / REMAINING -------------------------------------------------------------------------------------
# combine snow basin plan with size class data 
sb.sizecls.dt <- merge(sb.dt, lrg.tree.agc.by.sp.dbh.dt, allow.cartesian=T) %>% data.table()

# calc number of trees in each size class for each species and treatment 
sb.sizecls.dt <- sb.sizecls.dt[, n.trees := total.acres * lrg.tree.per.acre * stem.frac]

# calc AGC of trees in each size class using Jenkins et al. (2004) allometry for True Firs and Pine
sb.sizecls.dt[common.name == 'grand fir', agc.kg.per.tree := exp(-2.5384 + 2.4814 * log(dia.in.rnd/2.54))*0.5]
sb.sizecls.dt[common.name == 'ponderosa pine', agc.kg.per.tree := exp(-2.5356 + 2.4349 * log(dia.in.rnd/2.54))*0.5]

# calc total tree AGC by size class
sb.sizecls.dt <- sb.sizecls.dt[, agc.kg.total.dbh := n.trees * agc.kg.per.tree]

# calc total tree AGC for each species and treatment
sb.tx.dt <- sb.sizecls.dt[, .(agc.kg.total = sum(agc.kg.total.dbh)), by = c('common.name','treatment') ]
sb.tx.dt <- sb.tx.dt[, agc.ston.total := agc.kg.total / 908]
sb.tx.dt <- sb.tx.dt[, agc.frac := agc.ston.total / sum(agc.ston.total), by = common.name]

# write out
fwrite(sb.sizecls.dt, 'output/snow_basin_large_tree_size_class_distribution.csv')
fwrite(sb.tx.dt, 'output/snow_basin_treatment_effects.csv')
