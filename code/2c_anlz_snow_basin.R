# THIS R SCRIPT ASSESSES IMPACTS OF PROPOSED FOREST THINNING ON FOREST CARBON STORAGE IN LARGE TREES
# AUTHOR: LOGAN BERNER, ECOSPATIAL SERVICES L.L.C.
# DATE: 2020-07-19

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
tree.dt <- fread('output/WAORID_fia_tree_survey_biomass.csv')
plot.dt <- fread('data/fia/WAORID_fia_plots.csv')
cond.dt <- fread('data/fia/state/WA_COND.csv')
sp.codes.dt <- fread('data/fia/REF_SPECIES.CSV')
sb.shp <- readOGR('data/gis_data/SnowBasinHCPC/ProjectBoundary.shp')
nf.shp <- readOGR('data/gis_data/eastside_national_forests.shp')

# IDENTIFY PLOTS TO USE IN ANALYSIS ----------------------------------------------------------------------
names(plot.dt) <- tolower(names(plot.dt))
plot.dt <- plot.dt[, plot.id := paste(statecd, plot, sep='.')] # create unique identifier
plot.dt[, measyear.recent := max(measyear), by = plot.id] # compute most recent year of survey at each plot
plot.dt <- plot.dt[measyear == measyear.recent] # take most recent survey
plot.dt <- plot.dt[measyear >= 2010] # use plots meaasured since 2010

# SPATIALIZE PLOTS AND DETERMINE WHICH TO USE IN THE ANALYSIS ----------------------------------------------
# spatialize plots and double check alignment
plots.pt <- SpatialPointsDataFrame(coords = data.frame(plot.dt$lon, plot.dt$lat), data = plot.dt, proj4string = nad83)

# determine number of FIA plots in Snow Basin project area. 
sb.nad83.shp <- spTransform(sb.shp, CRSobj = nad83) 
plot.dt[, snow.basin := over(plots.pt, sb.nad83.shp)[,4]]
plot.sb.dt <- plot.dt[snow.basin == 1]
nrow(plot.sb.dt) # total number of FIA plots in Snow Basin 

# there are only 13 forested plots in project area, so use plot from whole Wallowa-Whitman Nat'l Forest
plot.dt[, nf.name := over(plots.pt, nf.shp)[,5]]
plot.dt <- plot.dt[plot_status_cd == 1] # "at least one accessible forest land condition present on plot"
plot.dt <- plot.dt[nf.name == 'Wallowa-Whitman National Forest']

# PREP AND SUBSET TREE SURVEYS ---------------------------------------------------------------------

# take trees from WWNF
tree.dt <- tree.dt[nf.name == 'Wallowa-Whitman National Forest']

# apply expansion factors for microplots, subplot, and macroplots
tree.dt <- tree.dt[, ':='(agc.kg = agc.kg*tpa.unadj, stems.n = tpa.unadj)]

# round off tree diameter
tree.dt[, dia.cm.rnd := round(dia.cm,1)]

# collapse PIPO, PSME, and LAOX into one 'mixed' species class
tree.dt <- tree.dt[common.name == 'ponderosa pine' | common.name == 'Douglas-fir' | common.name == 'western larch', common.name := 'mixed']

# number of plots used in analysis
length(unique(tree.dt$plot.id))

length(unique(tree.dt[dia.cm >= 53.3]$plot.id))

# ASSESS FRACTION OF LARGE TREES BY DBH FOR EACH SPECIES ON FIA PLOTS USING BOOTSTRAP SAMPLING TO GET UNCERTAINTY ----------------
n.mc <- 1000
frac.mc <- 0.33

tree.mc.lst <- list()

for (i in 1:n.mc){
  plots.mc.dt <- plot.dt[sample(1:nrow(plot.dt), nrow(plot.dt)*frac.mc, replace = T)]
  tree.mc.dt <- tree.dt[plot.id %in% plots.mc.dt$plot.id]
  tree.mc.dt$rep <- i
  tree.mc.lst[[i]] <- tree.mc.dt
}
tree.mc.dt <- rbindlist(tree.mc.lst)

lrg.tree.agc.by.sp.dbh.mc.dt <- tree.mc.dt[dia.cm >= 53.3, .(agc.kg.per.tree = mean(agc.kg), stems.n = sum(tpa.unadj)), by = c('common.name','dia.cm.rnd','rep')] # apply expansion factor
lrg.tree.agc.by.sp.dbh.mc.dt <- lrg.tree.agc.by.sp.dbh.mc.dt[, stem.frac := stems.n / sum(stems.n), by = c('common.name','rep')]
lrg.tree.agc.by.sp.dbh.mc.dt <- lrg.tree.agc.by.sp.dbh.mc.dt[, stems.n := NULL]
lrg.tree.agc.by.sp.dbh.mc.dt <- lrg.tree.agc.by.sp.dbh.mc.dt[order(common.name,dia.cm.rnd,rep)]

# ESTIMATE SNOW BASIN REMOVAL / REMAINING -------------------------------------------------------------------------------------
# combine snow basin plan with size class data 
sb.sizecls.mc.dt <- merge(sb.dt, lrg.tree.agc.by.sp.dbh.mc.dt, allow.cartesian=T) %>% data.table()

# calc number of trees in each size class for each species and treatment 
sb.sizecls.mc.dt <- sb.sizecls.mc.dt[, n.trees := total.acres * lrg.tree.per.acre * stem.frac]

# calc total tree AGC by size class
sb.sizecls.mc.dt <- sb.sizecls.mc.dt[, agc.kg.total.dbh := n.trees * agc.kg.per.tree]

# calc total tree AGC for each species and treatment
sb.tx.mc.dt <- sb.sizecls.mc.dt[, .(agc.gg.tx.total = sum(agc.kg.total.dbh) / 10^6), by = c('common.name','biophys','treatment','rep') ]
sb.tx.mc.dt <- sb.tx.mc.dt[, agc.pcnt.tx := agc.gg.tx.total / sum(agc.gg.tx.total) * 100, by = c('common.name','biophys','rep')]

# compute overall remove / retention
sb.tx.mc.overall.dt <- sb.tx.mc.dt[, .(common.name = 'overall', biophys = 'overall', 
                                       agc.gg.tx.total = sum(agc.gg.tx.total)), by = c('treatment','rep')]
sb.tx.mc.overall.dt[, agc.pcnt.tx := agc.gg.tx.total / sum(agc.gg.tx.total) * 100, by = rep]

# combine treatment totals with overall total
sb.tx.mc.dt <- rbind(sb.tx.mc.dt, sb.tx.mc.overall.dt)

# summarize across monte carlo reps
sb.tx.mc.smry.dt <- sb.tx.mc.dt[, .(agc.gg.tx.total.med = round(median(agc.gg.tx.total),1),
                                    agc.gg.tx.total.q025 = round(quantile(agc.gg.tx.total, 0.025), 1),
                                    agc.gg.tx.total.q975 = round(quantile(agc.gg.tx.total, 0.975), 1),
                                    agc.pcnt.tx.med = round(median(agc.pcnt.tx),1),
                                    agc.pcnt.tx.q025 = round(quantile(agc.pcnt.tx, 0.025), 1),
                                    agc.pcnt.tx.q975 = round(quantile(agc.pcnt.tx, 0.975), 1)),
                                by = c('common.name', 'biophys','treatment')]

# fancy table 
sb.tx.fancy.table <- sb.tx.mc.smry.dt[, .(agc.gg.tx.total = paste0(sprintf('%.1f', agc.gg.tx.total.med),' [', sprintf('%.1f', agc.gg.tx.total.q025),', ', sprintf('%.1f', agc.gg.tx.total.q975),']'),
                                          agc.pcnt.tx = paste0(sprintf('%.1f', agc.pcnt.tx.med),' [', sprintf('%.1f', agc.pcnt.tx.q025),', ', sprintf('%.1f', agc.pcnt.tx.q975),']')),
                                      by = c('common.name','biophys','treatment')]
sb.tx.fancy.table <- dcast(sb.tx.fancy.table, value.var = c('agc.gg.tx.total','agc.pcnt.tx'), formula = common.name + biophys ~ treatment) 
sb.tx.fancy.table[, common.name := factor(common.name, levels = c('grand fir','mixed','overall'))]
sb.tx.fancy.table <- sb.tx.fancy.table[order(common.name)]

# write out
# fwrite(sb.sizecls.dt, 'output/snow_basin_large_tree_size_class_distribution.csv')
fwrite(sb.tx.mc.smry.dt, 'output/snow_basin_treatment_effects.csv')
fwrite(sb.tx.fancy.table, 'output/snow_basin_treatment_effects_fancy.csv')