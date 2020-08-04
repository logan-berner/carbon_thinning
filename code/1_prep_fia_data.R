# THIS R SCRIPT 
# AUTHOR: LOGAN BERNER
# DATE: 2020-05-02

rm(list=ls())
require(R.utils)
require(ggplot2)
require(ggpubr)
require(data.table)
require(maptools)
require(raster)
require(rgdal)
nad83 <- CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
setwd('C:/Users/Logan/Google Drive/research/side_projects/mildrexler_oregon_forests/eastside_screen/')
mkdirs('figures/')
mkdirs('output/')

# LOAD DATA SETS ----------------------------------------------------------------------
tree.dt <- fread('data/fia/WAORID_fia_tree_surveys.csv')
plot.dt <- fread('data/fia/WAORID_fia_plots.csv')
sp.codes.dt <- fread('data/fia/REF_SPECIES.CSV')

allom.dt <- fread('data/biomass_equations_eastside.csv') # allometry equations (AGB ~ DBH + Height)
traits.dt <- fread('data/tree_traits.csv') # wood density, leaf and wood carbon content

nf.all.shp <- readOGR('data/gis_data/S_USA.AdministrativeForest.shp')


# SUBSET SHAPEFILE TO NATIONAL FORESTS USED IN ANALYSIS ----------------------------------------------------------------------
nf.oi <- paste(c('Deschutes','Fremont-Winema','Malheur','Ochoco','Umatilla','Wallowa-Whitman'), 'National Forest', sep=' ')
nf.shp <- nf.all.shp[nf.all.shp$FORESTNAME %in% nf.oi, ]
writeOGR(nf.shp, 'data/gis_data/eastside_national_forests.shp', layer = 'nat_forest', driver = 'ESRI Shapefile', overwrite_layer = T)
plot(nf.shp)

  
# IDENTIFY PLOTS TO USE IN ANALYSIS ----------------------------------------------------------------------
names(plot.dt) <- tolower(names(plot.dt))
plot.dt <- plot.dt[, plot.id := paste(statecd, plot, sep='.')] # create unique identifier
plot.dt[, measyear.recent := max(measyear), by = plot.id] # compute most recent year of survey at each plot
plot.dt <- plot.dt[measyear == measyear.recent] # take most recent survey
plot.dt <- plot.dt[measyear >= 2010] # use plots meaasured since 2010

# spatialize plots and double check alignment,
plots.pt <- SpatialPointsDataFrame(coords = data.frame(plot.dt$lon, plot.dt$lat), data = plot.dt, proj4string = nad83)
plot(nf.shp, col = 'gray')
points(plots.pt, col = 'blue', pch = '*', cex = 0.5)

# extract natl forest name to each plot
plot.dt[, nf.name := over(plots.pt, nf.shp)[,5]]

# take 'sampled' plots from eastside natl forests
plot.dt <- plot.dt[is.na(nf.name)==F]
plot.dt <- plot.dt[plot_status_cd == 1] # "at least one accessible forest land condition present on plot"
keep.cols <- c('plot.id','lat','lon','measyear','invyr','nf.name')
plot.dt <- plot.dt[, ..keep.cols]

# re-spatialize points for plots in eastside natl forests
plots.pt <- SpatialPointsDataFrame(coords = data.frame(plot.dt$lon, plot.dt$lat), data = plot.dt, proj4string = nad83)
plot(nf.shp, col = 'gray')
points(plots.pt, col = 'blue', pch = '*', cex = 0.5)
writeOGR(plots.pt, 'data/gis_data/eastside_nf_fia_plots_gte2010surveys.shp', layer = 'plots', driver = 'ESRI Shapefile', overwrite_layer = T)

# ADD ANCILLARY DATA TO TREE SURVEYS ---------------------------------------------------------------------
names(tree.dt) <- tolower(names(tree.dt))
names(sp.codes.dt) <- tolower(names(sp.codes.dt))
tree.dt <- tree.dt[, plot.id := paste(statecd, plot, sep='.')] # create plot id from state x plot number
tree.dt <- plot.dt[tree.dt, on = c('plot.id','invyr')] # add plot data
tree.dt <- tree.dt[is.na(nf.name) == F] # take trees from select natl forests

# add species name to each tree
sp.codes.dt <- sp.codes.dt[, c('spcd','common_name','genus')]
tree.dt <- sp.codes.dt[tree.dt, on = 'spcd']

# take only live tree from plots that were surveyed
tree.dt <- tree.dt[statuscd == 1] 

# add id to each tree
tree.dt[, tree.id := paste(invyr, plot.id, subp, tree, sep='.')]

length(unique(tree.dt$plot.id))

# take select cols from tree data table
keep.cols <- c('tree.id','plot.id','measyear','invyr','lat','lon','statecd','nf.name','plot','subp','tree',
               'spcd','common_name','genus','dia','ht','carbon_ag','drybio_ag','tpa_unadj')
tree.dt <- tree.dt[, ..keep.cols]

# rename some cols
setnames(tree.dt, c('common_name','dia','ht','carbon_ag','drybio_ag','tpa_unadj'), c('common.name','dia.in','ht.ft','fia.agc.lb','fia.agb.lb','tpa.unadj'))

# select species of interest
tree.dt <- tree.dt[common.name == 'Douglas-fir' | common.name == 'grand fir' | common.name == 'ponderosa pine' | common.name == 'western larch' | common.name == "Engelmann spruce"]
tree.dt[, common.name := factor(common.name, levels = c('Douglas-fir','Engelmann spruce','grand fir','ponderosa pine','western larch'))]

# convert to metric
tree.dt <- tree.dt[, ":="(dia.cm = dia.in*2.54, ht.m = ht.ft/3.28, fia.agb.kg = fia.agb.lb/2.205, fia.agc.kg = fia.agc.lb/2.205)]
tree.dt[, c('dia.in','ht.ft','fia.agb.lb','fia.agc.lb') := NULL]

# add wood traits to each tree
tree.dt <- tree.dt[traits.dt, on = 'common.name']

# ascribe size classes
tree.dt[dia.cm < 10, size.class := 'sapling'] # double check this size distinction
tree.dt[dia.cm >= 10, size.class := 'mature']

# ESTIMATE COMPENENT BIOMASS FOR EACH TREE ---------------------------------------------------------------------

# add allometric equations 
tree.sapl.dt <- tree.dt[size.class == 'sapling']
tree.sapl.dt <- tree.sapl.dt[allom.dt[organ=='SB'], on = 'common.name'] # append sapling allom to small trees
tree.matr.dt <- tree.dt[size.class == 'mature']
tree.matr.dt <- tree.matr.dt[allom.dt[organ != 'SB'], on = 'common.name', allow.cartesian=T] # append component allometry to larger trees
tree.organ.dt <- rbind(tree.matr.dt, tree.sapl.dt)

# build expressions to estimate organ biomass for each tree
tree.organ.dt[, eqn.exp := as.character(gsub('Y', ht.m, eqn)), by=1:nrow(tree.organ.dt)] # substitute tree height into eqn for each tree
tree.organ.dt[, eqn.exp := as.character(gsub('X', dia.cm, eqn.exp)), by=1:nrow(tree.organ.dt)] # substitute tree diameter into eqn for each tree

# compute biomass for each component expect bole volume
tree.organ.excldBV.dt <- tree.organ.dt[organ != 'BV']
tree.organ.excldBV.dt[, agb.g := eval(parse(text=eqn.exp)), by=1:nrow(tree.organ.excldBV.dt)] 

# compute bole volume and then estimote bole biomass using info on wood density
tree.organ.BV.dt <- tree.organ.dt[organ == 'BV']
tree.organ.BV.dt[, vol.m3 := eval(parse(text=eqn.exp)), by=1:nrow(tree.organ.BV.dt)]
tree.organ.BV.dt[, agb.g := vol.m3*(10^6)*wd.g.cm3][, vol.m3 := NULL]
tree.organ.BV.dt[organ == 'BV', organ := 'BM']

# compiben all organs back into one data table 
tree.organ.dt <- rbind(tree.organ.BV.dt, tree.organ.excldBV.dt)

# estimate carbon storage in each organ
tree.organ.dt[organ == 'FB', agc.g := agb.g * leaf.frac.c.avg]
tree.organ.dt[organ != 'FB', agc.g := agb.g * wood.frac.c.avg]

# compute total tree biomass and append back to tree.dt
tree.tagb.dt <- tree.organ.dt[, .(agb.kg = sum(agb.g)/10^3, agc.kg = sum(agc.g)/10^3), by = 'tree.id']
tree.dt <- tree.dt[tree.tagb.dt, on = 'tree.id']


# COMPARE TREE AGB and AGC FROM FIA VS REGIONAL ALLOMETRY ---------------------------------------------------------------------

# tree AGB
fig.fia.vs.reg.agb <- ggplot(tree.dt, aes(fia.agb.kg, agb.kg)) + geom_point(alpha = 0.25) + geom_abline()
fig.fia.vs.reg.agb <- fig.fia.vs.reg.agb + facet_wrap(~common.name, scales = 'free')
fig.fia.vs.reg.agb <- fig.fia.vs.reg.agb + xlab('FIA tree AGB (kg)') + ylab('Regional allometry tree AGB (kg)')
fig.fia.vs.reg.agb <- fig.fia.vs.reg.agb + theme_bw() + theme(axis.text=element_text(size=12), axis.title=element_text(size=14))

fig.fia.vs.reg.agb

jpeg('figures/Tree_AGB_FIA_vs_RegAllom_by_Species.jpg', width = 9, height = 6, units = 'in', res = 400)
fig.fia.vs.reg.agb
dev.off()

# tree AGC
fig.fia.vs.reg.agc <- ggplot(tree.dt, aes(fia.agc.kg, agc.kg)) + geom_point(alpha = 0.25) + geom_abline()
fig.fia.vs.reg.agc <- fig.fia.vs.reg.agc + facet_wrap(~common.name, scales = 'free')
fig.fia.vs.reg.agc <- fig.fia.vs.reg.agc + xlab('FIA tree AGC (kg)') + ylab('Regional allometry tree AGC (kg)')
fig.fia.vs.reg.agc <- fig.fia.vs.reg.agc + theme_bw() + theme(axis.text=element_text(size=12), axis.title=element_text(size=14))

fig.fia.vs.reg.agc

jpeg('figures/Tree_AGC_FIA_vs_RegAllom_by_Species.jpg', width = 9, height = 6, units = 'in', res = 400)
fig.fia.vs.reg.agc
dev.off()


# WRITE OUT ---------------------------------------------------------------------
fwrite(tree.dt, 'output/WAORID_fia_tree_survey_biomass.csv')

# END SCRIPT ---------------------------------------------------------------------