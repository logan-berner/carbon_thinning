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
setwd('C:/Users/Logan/Google Drive/research/side_projects/mildrexler_oregon_forests/carbon_thinning/')

# LOAD DATA SETS ----------------------------------------------------------------------
trees.dt <- fread('data/fia/WAORID_fia_tree_surveys.csv')
plots.dt <- fread('data/fia/WAORID_fia_plots.csv')
sp.codes.dt <- fread('data/fia/REF_SPECIES.CSV')
spgrp.codes.dt <- fread('data/fia/REF_SPECIES_GROUP.CSV')
nf.shp <- readOGR('data/gis_data/eastside_national_forests_nad83.shp')


# IDENTIFY PLOTS TO USE IN ANALYSIS ----------------------------------------------------------------------
names(plots.dt) <- tolower(names(plots.dt))
plots.dt <- plots.dt[, plot.id := paste(statecd, plot, sep='.')]
plots.dt[, measyear.recent := max(measyear), by = plot.id] # compute most recent year of survey at each plot
plots.dt <- plots.dt[measyear == measyear.recent] # take most recent survey
plots.dt <- plots.dt[measyear >= 2010] # use plots meaasured since 2010

# spatialize plots and double check alignment,
plots.pt <- SpatialPointsDataFrame(coords = data.frame(plots.dt$lon, plots.dt$lat), data = plots.dt, proj4string = nad83)
plot(nf.shp, col = 'gray')
points(plots.pt, col = 'blue', pch = '*', cex = 0.5)

# extract natl forest name
plots.dt[, nf.name := over(plots.pt, nf.shp)[,5]]

# take 'sampled' plots from eastside natl forests
plots.dt <- plots.dt[is.na(nf.name)==F]
plots.dt <- plots.dt[plot_status_cd == 1] # "at least one accessible forest land condition present on plot"
keep.cols <- c('plot.id','lat','lon','measyear','invyr','nf.name')
plots.dt <- plots.dt[, ..keep.cols]

# re-spatialize points for plots in eastside natl forests
plots.pt <- SpatialPointsDataFrame(coords = data.frame(plots.dt$lon, plots.dt$lat), data = plots.dt, proj4string = nad83)
plot(nf.shp, col = 'gray')
points(plots.pt, col = 'blue', pch = '*', cex = 0.5)
writeOGR(plots.pt, 'data/gis_data/eastside_fia_plots_post2010survey_wgs84.shp', layer = 'plots', driver = 'ESRI Shapefile', overwrite_layer = T)


# ADD ANCILLARY DATA TO TREE SURVEYS ---------------------------------------------------------------------
names(trees.dt) <- tolower(names(trees.dt))
names(sp.codes.dt) <- tolower(names(sp.codes.dt))
trees.dt <- trees.dt[, plot.id := paste(statecd, plot, sep='.')] # create plot id from state x plot number
trees.dt <- plots.dt[trees.dt, on = c('plot.id','invyr')] # add plot data
trees.dt <- trees.dt[is.na(nf.name) == F] # take trees from select natl forests

# add species name to each tree
sp.codes.dt <- sp.codes.dt[, c('spcd','common_name','genus')]
trees.dt <- sp.codes.dt[trees.dt, on = 'spcd']

# take only live tree from subplots (not microplots or macroplots)
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


# ASSESS TREE COUNT AND AGC BY DBH ----------------------------------------------------------------------
trees.dt[, dia.in.rnd := round(dia.in)]

# calculate
tree.agc.by.sp.dbh.dt <- trees.dt[, .(tot.agc.lb = sum(agc.lb), n.stems = .N), by = c('common.name','dia.in.rnd')]
tree.agc.by.sp.dbh.dt <- tree.agc.by.sp.dbh.dt[, ':='(agc.pcnt = tot.agc.lb / sum(tot.agc.lb) * 100, stems.pcnt = n.stems / sum(n.stems) * 100), by = c('common.name')]
tree.agc.by.sp.dbh.dt <- tree.agc.by.sp.dbh.dt[order(common.name,dia.in.rnd)]
tree.agc.by.sp.dbh.dt <- tree.agc.by.sp.dbh.dt[, ':='(agc.cum.pcnt = cumsum(agc.pcnt), stems.cum.pcnt = cumsum(stems.pcnt)), by = c('common.name')]
tree.agc.by.sp.dbh.dt <- tree.agc.by.sp.dbh.dt[, ':='(agc.cum.pcnt.rev = 100-agc.cum.pcnt, stems.cum.pcnt.rev = 100 - stems.cum.pcnt)]

# plot cumulative % of stems by DBH for each species  
tree.stems.by.sp.dbh.fig <- ggplot(tree.agc.by.sp.dbh.dt, aes(dia.in.rnd, stems.cum.pcnt, group = common.name)) + 
  geom_line(aes(color = common.name)) + scale_color_discrete(name = 'Species') + 
  ylab("Cumulative % of tree species stems") + xlab('Tree DBH (in)') + 
  geom_vline(xintercept = 21, lty = 2) + 
  theme_bw() + theme(legend.position = c(0.8,0.4), legend.text=element_text(size=10), legend.title=element_text(size=12),
                     axis.text=element_text(size=12), axis.title=element_text(size=14))

# plot cumulative % of AGC by DBH for each species  
tree.agc.by.sp.dbh.fig <- ggplot(tree.agc.by.sp.dbh.dt, aes(dia.in.rnd, agc.cum.pcnt, group = common.name)) + 
  geom_line(aes(color = common.name)) + scale_color_discrete(name = 'Species') + 
  ylab("Cumulative % of tree species AGC") + xlab('Tree DBH (in)') + 
  geom_vline(xintercept = 21, lty = 2) + 
  theme_bw() + theme(legend.position = 'none', axis.text=element_text(size=12), axis.title=element_text(size=14))

# combine figures
jpeg('figures/SpStemCntAGC_by_DBH.jpg', width = 10, height = 4, res = 400, units = 'in')
ggarrange(tree.stems.by.sp.dbh.fig, tree.agc.by.sp.dbh.fig, labels=c('(a)','(b)'), label.x = 0.15, label.y = 0.98)
dev.off()

tree.agc.by.sp.dbh.dt[dia.in.rnd == 20]

tree.agc.by.sp.dbh.dt[dia.in.rnd == 20,c(1,7,8)]
