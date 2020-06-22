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

# take select cols from tree data table
keep.cols <- c('plot.id','measyear','lat','lon','nf.name','plot','subp','tree',
               'spcd','common_name','genus','dia','ht','carbon_ag','tpa_unadj')
tree.dt <- tree.dt[, ..keep.cols]

# rename some cols
setnames(tree.dt, c('common_name','dia','ht','carbon_ag','tpa_unadj'), c('common.name','dia.in','ht.in','agc.lb','tpa.unadj'))

# select species of interest
tree.dt <- tree.dt[common.name == 'Douglas-fir' | common.name == 'grand fir' | common.name == 'ponderosa pine' | common.name == 'western larch' | common.name == "Engelmann spruce"]
tree.dt[, common.name := factor(common.name, levels = c('Douglas-fir','Engelmann spruce','grand fir','ponderosa pine','western larch'))]


# RESAMPLE PLOTS USED IN ANALYSIS ----------------------------------------------------------------------
n.mc <- 10000
frac.mc <- 1/3

trees.mc.lst <- list()

for (i in 1:n.mc){
  plots.mc.dt <- plot.dt[sample(1:nrow(plot.dt), nrow(plot.dt)*frac.mc, replace = T)]
  trees.mc.dt <- tree.dt[plot.id %in% plots.mc.dt$plot.id]
  trees.mc.dt$rep <- i
  trees.mc.lst[[i]] <- trees.mc.dt
}

trees.mc.dt <- rbindlist(trees.mc.lst)


# ASSESS TREE COUNT AND AGC BY DBH ----------------------------------------------------------------------
trees.mc.dt[, dia.in.rnd := round(dia.in,1)]

# apply adjustment factors for microplots, subplot, and macroplots
tree.agc.by.sp.dbh.mc.dt <- trees.mc.dt[, .(tot.agc.lb = sum(agc.lb*tpa.unadj), n.stems = sum(tpa.unadj)), by = c('common.name','dia.in.rnd','rep')]
tree.agc.by.sp.dbh.mc.dt <- tree.agc.by.sp.dbh.mc.dt[, ':='(agc.pcnt = tot.agc.lb / sum(tot.agc.lb) * 100, total.stems = sum(n.stems), stems.pcnt = n.stems / sum(n.stems) * 100), by = c('common.name','rep')]
tree.agc.by.sp.dbh.mc.dt <- tree.agc.by.sp.dbh.mc.dt[order(rep,common.name,dia.in.rnd)]
tree.agc.by.sp.dbh.mc.dt <- tree.agc.by.sp.dbh.mc.dt[, ':='(agc.cum.pcnt = cumsum(agc.pcnt), stems.cum.pcnt = cumsum(stems.pcnt)), by = c('common.name','rep')]
tree.agc.by.sp.dbh.mc.dt <- tree.agc.by.sp.dbh.mc.dt[, ':='(agc.cum.pcnt.abv = 100-agc.cum.pcnt, stems.cum.pcnt.abv = 100 - stems.cum.pcnt)]

tree.agc.by.sp.dbh.smry.dt <- tree.agc.by.sp.dbh.mc.dt[, .(total.stems.med = round(median(total.stems)),
                                                           total.stems.q025 = round(quantile(total.stems, 0.025), 1),
                                                           total.stems.q975 = round(quantile(total.stems, 0.975), 1),
                                                           agc.cum.pcnt.med = round(median(agc.cum.pcnt), 1),
                                                           agc.cum.pcnt.q025 = round(quantile(agc.cum.pcnt, 0.025), 1),
                                                           agc.cum.pcnt.q975 = round(quantile(agc.cum.pcnt, 0.975), 1),
                                                           agc.cum.pcnt.abv.med = round(median(agc.cum.pcnt.abv), 1),
                                                           agc.cum.pcnt.abv.q025 = round(quantile(agc.cum.pcnt.abv, 0.025), 1),
                                                           agc.cum.pcnt.abv.q975 = round(quantile(agc.cum.pcnt.abv, 0.975), 1),
                                                           stems.cum.pcnt.med = round(median(stems.cum.pcnt), 1),
                                                           stems.cum.pcnt.q025 = round(quantile(stems.cum.pcnt, 0.025), 1),
                                                           stems.cum.pcnt.q975 = round(quantile(stems.cum.pcnt, 0.975), 1),
                                                           stems.cum.pcnt.abv.med = round(median(stems.cum.pcnt.abv), 1),
                                                           stems.cum.pcnt.abv.q025 = round(quantile(stems.cum.pcnt.abv, 0.025), 1), 
                                                           stems.cum.pcnt.abv.q975 = round(quantile(stems.cum.pcnt.abv, 0.975), 1)), 
                                                       by = c('common.name', 'dia.in.rnd')]

# plot cumulative % of stems by DBH for each species
tree.stems.by.sp.dbh.fig <- ggplot(tree.agc.by.sp.dbh.smry.dt, aes(dia.in.rnd, stems.cum.pcnt.abv.med, group = common.name)) + 
  geom_ribbon(aes(x = dia.in.rnd, ymin=stems.cum.pcnt.abv.q025, ymax=stems.cum.pcnt.abv.q975, fill=common.name), alpha = 0.25)+ 
  geom_line(aes(color = common.name)) +
  ylab("Percentage of all tree stems \n above diameter threshold") + xlab('Tree diameter (in)') + 
  geom_vline(xintercept = 21, lty = 2) + labs(col='Common name', fill='Common name') + 
  theme_bw() + theme(legend.position = c(0.7,0.4), legend.text=element_text(size=10), legend.title=element_text(size=12),
                     axis.text=element_text(size=12), axis.title=element_text(size=14))

# plot cumulative % of AGC by DBH for each species  
tree.agc.by.sp.dbh.fig <- ggplot(tree.agc.by.sp.dbh.smry.dt, aes(dia.in.rnd, agc.cum.pcnt.abv.med, group = common.name)) + 
  geom_ribbon(aes(x = dia.in.rnd, ymin=agc.cum.pcnt.abv.q025, ymax=agc.cum.pcnt.abv.q975, fill=common.name), alpha = 0.25)+ 
  geom_line(aes(color = common.name)) + 
  ylab("Percentage of total AGC in trees \n above diameter threshold") + xlab('Tree diameter (in)') + 
  geom_vline(xintercept = 21, lty = 2) + 
  theme_bw() + theme(legend.position = 'none', axis.text=element_text(size=12), axis.title=element_text(size=14))

# combine figures
jpeg('figures/Pcnt_StemCnt_AGC_Above_DBH_by_Species.jpg', width = 10, height = 4, res = 400, units = 'in')
ggarrange(tree.stems.by.sp.dbh.fig, tree.agc.by.sp.dbh.fig, labels=c('(a)','(b)'), label.x = 0.85, label.y = 0.98)
dev.off()

# info for figure legend
tree.agc.by.sp.dbh.smry.dt[dia.in.rnd == 21 & common.name == "ponderosa pine"]
nrow(tree.dt) # number of trees
length(unique(tree.dt$plot.id)) # number of plots
fivenum(tree.dt$measyear)

# table showing % of tree stems and AGB in trees <21"
fancy.table <- tree.agc.by.sp.dbh.smry.dt[dia.in.rnd == 20.9]
fancy.table <- fancy.table[, .(total.stems = paste0(sprintf('%.0f', total.stems.med),' [', sprintf('%.0f', total.stems.q025),', ', sprintf('%.0f', total.stems.q975),']'),
                               stems.cum.pcnt = paste0(sprintf('%.1f', stems.cum.pcnt.med),' [', sprintf('%.1f', stems.cum.pcnt.q025),', ', sprintf('%.1f', stems.cum.pcnt.q975),']'),
                               agc.cum.pcnt = paste0(sprintf('%.1f', agc.cum.pcnt.med),' [', sprintf('%.1f', agc.cum.pcnt.q025),', ', sprintf('%.1f', agc.cum.pcnt.q975),']'),
                               stems.cum.pcnt.abv = paste0(sprintf('%.1f', stems.cum.pcnt.abv.med),' [', sprintf('%.1f', stems.cum.pcnt.abv.q025),', ', sprintf('%.1f', stems.cum.pcnt.abv.q975),']'),
                               agc.cum.pcnt.abv = paste0(sprintf('%.1f', agc.cum.pcnt.abv.med),' [', sprintf('%.1f', agc.cum.pcnt.abv.q025),', ', sprintf('%.1f', agc.cum.pcnt.abv.q975),']')), 
                           by = common.name]
fwrite(fancy.table, 'output/tree_stems_agc_blw_21in_on_natl_forest.csv')


# WORK IN PROGRESS ... COMPUTE AVERAGE DENSITY OF LARGE TREES ON LANDSCAPE ------------------------------------------------
# question: How to handle plots that do not include four forested subplots?
sqft.per.subplot <- (pi*24^2) # each subplot has a 24 ft radius 
sqft.per.acre <- 43560 # there are 43560 ft2 / acre

# compute tree density on each subplot  
tree.dens.subp.dt <- tree.dt[, .(n.trees = .N), by = c('nf.name','plot.id','subp')]
tree.lrg.dens.dt <- tree.dt[dia.in >= 21, .(n.lrg.trees = .N), by = c('nf.name','plot.id','subp')]
tree.dens.subp.dt <- tree.lrg.dens.dt[tree.dens.subp.dt, on = c('nf.name','plot.id','subp')]
tree.dens.subp.dt[is.na(n.lrg.trees), n.lrg.trees := 0]
tree.dens.subp.dt <- tree.dens.subp.dt[, ':='(lrg.trees.per.acre = n.lrg.trees / (sqft.per.subplot) * sqft.per.acre,
                    trees.per.acre = n.trees / (sqft.per.subplot) * sqft.per.acre)]
tree.dens.subp.dt <- tree.dens.subp.dt[order(plot.id,subp)]

# compute average tree density on each plot
tree.dens.dt <- tree.dens.subp.dt[, .(trees.per.acre.avg = mean(trees.per.acre), lrg.trees.per.acre.avg = mean(lrg.trees.per.acre)), by = c('nf.name','plot.id')]

# summarize tree density across each national forest
tree.dens.smry.by.nf.dt <- tree.dens.dt[, .(n.plots = .N,
                                            trees.per.acre.med = median(trees.per.acre.avg),
                                            trees.per.acre.q025 = quantile(trees.per.acre.avg, 0.025),
                                            trees.per.acre.q25 = quantile(trees.per.acre.avg, 0.25),
                                            trees.per.acre.q75 = quantile(trees.per.acre.avg, 0.75),
                                            trees.per.acre.q975 = quantile(trees.per.acre.avg, 0.975),
                                            lrg.trees.per.acre.med = median(lrg.trees.per.acre.avg),
                                            lrg.trees.per.acre.q025 = quantile(lrg.trees.per.acre.avg, 0.025),
                                            lrg.trees.per.acre.q25 = quantile(lrg.trees.per.acre.avg, 0.25),
                                            lrg.trees.per.acre.q75 = quantile(lrg.trees.per.acre.avg, 0.75),
                                            lrg.trees.per.acre.q975 = quantile(lrg.trees.per.acre.avg, 0.975)),
                                            by = nf.name]

tree.dens.smry.by.nf.dt <- tree.dens.smry.by.nf.dt[order(nf.name)]
rnd.cols <- names(tree.dens.smry.by.nf.dt)[-1]
tree.dens.smry.by.nf.dt[, (rnd.cols) := round(.SD, 1), .SDcols = rnd.cols] # round tree density to one decimal
tree.dens.smry.by.nf.dt

# make fancy output table
tree.dens.smry.by.nf.fancy.dt <- tree.dens.smry.by.nf.dt[, 1:2]
tree.dens.smry.by.nf.fancy.dt$trees.per.acre <- paste0(sprintf('%.1f', tree.dens.smry.by.nf.dt$trees.per.acre.med), ' [',
                                                       sprintf('%.1f', tree.dens.smry.by.nf.dt$trees.per.acre.q025), ', ',
                                                       sprintf('%.1f', tree.dens.smry.by.nf.dt$trees.per.acre.q975), ']')
tree.dens.smry.by.nf.fancy.dt$lrg.trees.per.acre <- paste0(sprintf('%.1f', tree.dens.smry.by.nf.dt$lrg.trees.per.acre.med), ' [',
                                                       sprintf('%.1f', tree.dens.smry.by.nf.dt$lrg.trees.per.acre.q025), ', ',
                                                       sprintf('%.1f', tree.dens.smry.by.nf.dt$lrg.trees.per.acre.q975), ']')
fwrite(tree.dens.smry.by.nf.fancy.dt, 'output/tree_density_by_natl_forest.csv')


# MORE TO COME... ----------------------------------------------------------------------


# HOLDING-- PROABLY DELETE ----------------------------------------------------------------------
# # plot cumulative % of stems by DBH for each species  
# tree.stems.by.sp.dbh.fig <- ggplot(tree.agc.by.sp.dbh.smry.dt, aes(dia.in.rnd, stems.cum.pcnt.med, group = common.name)) + 
#   geom_ribbon(aes(x = dia.in.rnd, ymin=stems.cum.pcnt.q025, ymax=stems.cum.pcnt.q975, fill=common.name), alpha = 0.25)+ 
#   geom_line(aes(color = common.name)) +
#   ylab("Cumulative % of tree species stems") + xlab('Tree diameter (in)') + 
#   geom_vline(xintercept = 21, lty = 2) + labs(col='Common name', fill='Common name') + 
#   theme_bw() + theme(legend.position = c(0.8,0.4), legend.text=element_text(size=10), legend.title=element_text(size=12),
#                      axis.text=element_text(size=12), axis.title=element_text(size=14))
# 
# # plot cumulative % of AGC by DBH for each species  
# tree.agc.by.sp.dbh.fig <- ggplot(tree.agc.by.sp.dbh.smry.dt, aes(dia.in.rnd, agc.cum.pcnt.med, group = common.name)) + 
#   geom_ribbon(aes(x = dia.in.rnd, ymin=agc.cum.pcnt.q025, ymax=agc.cum.pcnt.q975, fill=common.name), alpha = 0.25)+ 
#   geom_line(aes(color = common.name)) + 
#   ylab("Cumulative % of tree species AGC") + xlab('Tree diameter (in)') + 
#   geom_vline(xintercept = 21, lty = 2) + 
#   theme_bw() + theme(legend.position = 'none', axis.text=element_text(size=12), axis.title=element_text(size=14))
# 
# # combine figures
# jpeg('figures/SpStemCntAGC_by_DBH.jpg', width = 10, height = 4, res = 400, units = 'in')
# ggarrange(tree.stems.by.sp.dbh.fig, tree.agc.by.sp.dbh.fig, labels=c('(a)','(b)'), label.x = 0.15, label.y = 0.98)
# dev.off()
