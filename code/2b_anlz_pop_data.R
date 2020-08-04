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
tree.dt <- fread('output/WAORID_fia_tree_survey_biomass.csv')
plot.dt <- fread('data/fia/WAORID_fia_plots.csv')

# set factor
tree.dt[, common.name := factor(common.name, levels = c('Douglas-fir','Engelmann spruce','grand fir','ponderosa pine','western larch'))]

# apply expansion factors for microplots, subplot, and macroplots
tree.dt <- tree.dt[, ':='(agc.kg = agc.kg*tpa.unadj, stems.n = tpa.unadj)]

# compute number of stems per species and across all species
tree.dt <- tree.dt[, stems.n.sp := sum(stems.n), by = 'common.name']
tree.dt <- tree.dt[, stems.n.overall := sum(stems.n)]

# round off tree diameter 
tree.dt[, dia.cm.rnd := round(dia.cm,1)]

# select columns to keep (reduce file size!!)
cols <- c('plot.id','common.name','dia.cm.rnd','stems.n','stems.n.sp','stems.n.overall','agb.kg','agc.kg')
tree.dt <- tree.dt[, ..cols]

# plot.d 
plot.ids <- unique(tree.dt$plot.id)

# RESAMPLE PLOTS USED IN ANALYSIS ----------------------------------------------------------------------
n.mc <- 10000
frac.mc <- 0.25

trees.mc.lst <- list()

for (i in 1:n.mc){
  mc.plots.ids <- sample(plot.ids, length(plot.ids)*frac.mc, replace = T)
  trees.mc.dt <- tree.dt[plot.id %in% mc.plots.ids]
  trees.mc.dt$rep <- i
  trees.mc.lst[[i]] <- trees.mc.dt
}

trees.mc.dt <- rbindlist(trees.mc.lst)
rm(trees.mc.lst)

# ASSESS TREE COUNT AND AGC BY DBH ----------------------------------------------------------------------

# summarize by species
tree.agc.by.sp.dbh.mc.dt <- trees.mc.dt[, .(agc.kg.sp.dbh.mc = sum(agc.kg), 
                                            stems.n.sp.dbh.mc = sum(stems.n), 
                                            stems.n.sp = mean(stems.n.sp)), 
                                        by = c('common.name','dia.cm.rnd','rep')]

tree.agc.by.sp.dbh.mc.dt <- tree.agc.by.sp.dbh.mc.dt[, ':='(agc.pcnt.sp.dbh.mc = agc.kg.sp.dbh.mc / sum(agc.kg.sp.dbh.mc) * 100, 
                                                            stems.n.sp.mc = sum(stems.n.sp.dbh.mc), 
                                                            stems.pcnt.sp.dbh.mc = stems.n.sp.dbh.mc / sum(stems.n.sp.dbh.mc) * 100), 
                                                     by = c('common.name','rep')]

tree.agc.by.sp.dbh.mc.dt <- tree.agc.by.sp.dbh.mc.dt[order(rep,common.name,dia.cm.rnd)]

tree.agc.by.sp.dbh.mc.dt <- tree.agc.by.sp.dbh.mc.dt[, ':='(agc.cum.pcnt.sp.dbh.mc = cumsum(agc.pcnt.sp.dbh.mc), 
                                                            stems.cum.pcnt.sp.dbh.mc = cumsum(stems.pcnt.sp.dbh.mc)), 
                                                     by = c('common.name','rep')]

tree.agc.by.sp.dbh.mc.dt <- tree.agc.by.sp.dbh.mc.dt[, ':='(agc.cum.pcnt.abv.sp.dbh.mc = 100-agc.cum.pcnt.sp.dbh.mc, 
                                                            stems.cum.pcnt.abv.sp.dbh.mc = 100 - stems.cum.pcnt.sp.dbh.mc)]

# summarize across all species 
tree.agc.by.dbh.mc.dt <- trees.mc.dt[, .(common.name = 'overall', 
                                         agc.kg.sp.dbh.mc = sum(agc.kg), 
                                         stems.n.sp.dbh.mc = sum(stems.n), 
                                         stems.n.sp = mean(stems.n.overall)), 
                                     by = c('dia.cm.rnd','rep')]

tree.agc.by.dbh.mc.dt <- tree.agc.by.dbh.mc.dt[, ':='(agc.pcnt.sp.dbh.mc = agc.kg.sp.dbh.mc / sum(agc.kg.sp.dbh.mc) * 100, 
                                                            stems.n.sp.mc = sum(stems.n.sp.dbh.mc), 
                                                            stems.pcnt.sp.dbh.mc = stems.n.sp.dbh.mc / sum(stems.n.sp.dbh.mc) * 100), 
                                                     by = c('rep')]

tree.agc.by.dbh.mc.dt <- tree.agc.by.dbh.mc.dt[order(rep,dia.cm.rnd)]

tree.agc.by.dbh.mc.dt <- tree.agc.by.dbh.mc.dt[, ':='(agc.cum.pcnt.sp.dbh.mc = cumsum(agc.pcnt.sp.dbh.mc), 
                                                            stems.cum.pcnt.sp.dbh.mc = cumsum(stems.pcnt.sp.dbh.mc)), 
                                                     by = c('rep')]

tree.agc.by.dbh.mc.dt <- tree.agc.by.dbh.mc.dt[, ':='(agc.cum.pcnt.abv.sp.dbh.mc = 100-agc.cum.pcnt.sp.dbh.mc, 
                                                            stems.cum.pcnt.abv.sp.dbh.mc = 100 - stems.cum.pcnt.sp.dbh.mc)]

# combine species-level and cross-species summaries
tree.agc.by.sp.dbh.mc.dt <- rbind(tree.agc.by.sp.dbh.mc.dt, tree.agc.by.dbh.mc.dt)

# summarize across MC reps   
tree.agc.by.sp.dbh.smry.dt <- tree.agc.by.sp.dbh.mc.dt[, .(stems.n.sp = round(median(stems.n.sp)),
                                                           stems.n.mc.med = round(median(stems.n.sp.mc)),
                                                           stems.n.mc.q025 = round(quantile(stems.n.sp.mc, 0.025), 1),
                                                           stems.n.mc.q975 = round(quantile(stems.n.sp.mc, 0.975), 1),
                                                           agc.cum.pcnt.med = round(median(agc.cum.pcnt.sp.dbh.mc), 1),
                                                           agc.cum.pcnt.q025 = round(quantile(agc.cum.pcnt.sp.dbh.mc, 0.025), 1),
                                                           agc.cum.pcnt.q975 = round(quantile(agc.cum.pcnt.sp.dbh.mc, 0.975), 1),
                                                           agc.cum.pcnt.abv.med = round(median(agc.cum.pcnt.abv.sp.dbh.mc), 1),
                                                           agc.cum.pcnt.abv.q025 = round(quantile(agc.cum.pcnt.abv.sp.dbh.mc, 0.025), 1),
                                                           agc.cum.pcnt.abv.q975 = round(quantile(agc.cum.pcnt.abv.sp.dbh.mc, 0.975), 1),
                                                           stems.cum.pcnt.med = round(median(stems.cum.pcnt.sp.dbh.mc), 1),
                                                           stems.cum.pcnt.q025 = round(quantile(stems.cum.pcnt.sp.dbh.mc, 0.025), 1),
                                                           stems.cum.pcnt.q975 = round(quantile(stems.cum.pcnt.sp.dbh.mc, 0.975), 1),
                                                           stems.cum.pcnt.abv.med = round(median(stems.cum.pcnt.abv.sp.dbh.mc), 1),
                                                           stems.cum.pcnt.abv.q025 = round(quantile(stems.cum.pcnt.abv.sp.dbh.mc, 0.025), 1), 
                                                           stems.cum.pcnt.abv.q975 = round(quantile(stems.cum.pcnt.abv.sp.dbh.mc, 0.975), 1)), 
                                                       by = c('common.name', 'dia.cm.rnd')]


# PLOT CUMULATIVE % OF STEMS AND AGB BY DBH FOR EACH SPECIES ----------------------------------------------------------
tree.agc.by.sp.dbh.smry.dt[dia.cm.rnd == max(dia.cm.rnd)]
tree.agc.by.sp.dbh.smry.dt[dia.cm.rnd >= 150 & common.name == 'overall']


# plot cumulative % of stems by DBH for each species
tree.stems.by.sp.dbh.fig <- ggplot(tree.agc.by.sp.dbh.smry.dt, aes(dia.cm.rnd, stems.cum.pcnt.abv.med, group = common.name)) + 
  geom_ribbon(aes(x = dia.cm.rnd, ymin=stems.cum.pcnt.abv.q025, ymax=stems.cum.pcnt.abv.q975, fill=common.name), alpha = 0.25)+ 
  geom_line(aes(color = common.name)) + coord_cartesian(xlim=c(0,150)) + 
  ylab("Percentage of all tree stems \n above diameter threshold") + xlab('Tree diameter (cm)') + 
  geom_vline(xintercept = 21*2.54, lty = 2) + labs(col='Common name', fill='Common name') + 
  theme_bw() + theme(legend.position = c(0.7,0.6), legend.text=element_text(size=10), legend.title=element_text(size=12),
                     axis.text=element_text(size=12), axis.title=element_text(size=14))

# plot cumulative % of AGC by DBH for each species  
tree.agc.by.sp.dbh.fig <- ggplot(tree.agc.by.sp.dbh.smry.dt, aes(dia.cm.rnd, agc.cum.pcnt.abv.med, group = common.name)) + 
  geom_ribbon(aes(x = dia.cm.rnd, ymin=agc.cum.pcnt.abv.q025, ymax=agc.cum.pcnt.abv.q975, fill=common.name), alpha = 0.25)+ 
  geom_line(aes(color = common.name)) + coord_cartesian(xlim=c(0,150)) + 
  ylab("Percentage of total AGC in trees \n above diameter threshold") + xlab('Tree diameter (cm)') + 
  geom_vline(xintercept = 21*2.54, lty = 2) + 
  theme_bw() + theme(legend.position = 'none', axis.text=element_text(size=12), axis.title=element_text(size=14))

# combine figures
jpeg('figures/Pcnt_StemCnt_AGC_Above_DBH_by_Species.jpg', width = 10, height = 4, res = 400, units = 'in')
ggarrange(tree.stems.by.sp.dbh.fig, tree.agc.by.sp.dbh.fig, labels=c('(a)','(b)'), label.x = 0.85, label.y = 0.98)
dev.off()

# info for figure legend
tree.agc.by.sp.dbh.smry.dt[dia.cm.rnd >= round(21*2.54) & common.name == "ponderosa pine"]
nrow(tree.dt) # number of trees
length(unique(tree.dt$plot.id)) # number of plots
fivenum(tree.dt$measyear)


# TABLE SHOWING % OF TREE STEMS AND AGB IN TREES <= 21 AND >= 21'----------------------------------------------------------------------

fancy.table <- tree.agc.by.sp.dbh.smry.dt[dia.cm.rnd == round(21*2.54,1)]
fancy.table <- fancy.table[order(common.name)]
fancy.table <- fancy.table[, .(stems.n.sp = sprintf('%.0f', stems.n.sp),
                               stems.n.mc.med = paste0(sprintf('%.0f', stems.n.mc.med),' [', sprintf('%.0f', stems.n.mc.q025),', ', sprintf('%.0f', stems.n.mc.q975),']'),
                               stems.cum.pcnt = paste0(sprintf('%.1f', stems.cum.pcnt.med),' [', sprintf('%.1f', stems.cum.pcnt.q025),', ', sprintf('%.1f', stems.cum.pcnt.q975),']'),
                               agc.cum.pcnt = paste0(sprintf('%.1f', agc.cum.pcnt.med),' [', sprintf('%.1f', agc.cum.pcnt.q025),', ', sprintf('%.1f', agc.cum.pcnt.q975),']'),
                               stems.cum.pcnt.abv = paste0(sprintf('%.1f', stems.cum.pcnt.abv.med),' [', sprintf('%.1f', stems.cum.pcnt.abv.q025),', ', sprintf('%.1f', stems.cum.pcnt.abv.q975),']'),
                               agc.cum.pcnt.abv = paste0(sprintf('%.1f', agc.cum.pcnt.abv.med),' [', sprintf('%.1f', agc.cum.pcnt.abv.q025),', ', sprintf('%.1f', agc.cum.pcnt.abv.q975),']')), 
                           by = common.name]
fwrite(fancy.table, 'output/tree_stems_agc_blw_abv_21in_threshold_on_natl_forest.csv')

# END SCRIPT ----------------------------------------------------------------------