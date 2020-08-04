# THIS R SCRIPT EXAMINES THE RELATIONSHIP BETWEEN TREE AGC AND DBH AMONG EASTSIDE TREES  
# AUTHOR: LOGAN BERNER
# DATE: 2020-07-19

rm(list=ls())
require(ggplot2)
require(ggpubr)
require(data.table)
require(zoo)
setwd('C:/Users/Logan/Google Drive/research/side_projects/mildrexler_oregon_forests/eastside_screen/')

# LOAD DATA SETS ----------------------------------------------------------------------
tree.dt <- fread('output/WAORID_fia_tree_survey_biomass.csv')

# set factors
tree.dt[, species := factor(common.name, levels = c('Douglas-fir','Engelmann spruce','grand fir','ponderosa pine','western larch'))]

# compute number of stems per species and across all species
tree.dt <- tree.dt[, stems.n := tpa.unadj]
tree.dt <- tree.dt[, stems.n.sp := sum(stems.n), by = 'common.name']
tree.dt <- tree.dt[, stems.n.overall := sum(stems.n)]

# round off tree diameter
tree.dt[, dia.cm.rnd := round(dia.cm, 0)]

# columns to keep
cols <- c('plot.id','common.name','dia.cm.rnd','stems.n','stems.n.sp','stems.n.overall','agb.kg','agc.kg')
tree.dt <- tree.dt[, ..cols]

# plot.d 
plot.ids <- unique(tree.dt$plot.id)
length(plot.ids) # number of plots

# RESAMPLE PLOTS USED IN ANALYSIS --------------------------------------------------------
n.mc <- 10000
frac.mc <- 0.25

tree.mc.lst <- list()

for (i in 1:n.mc){
  mc.plots.ids <- sample(plot.ids, length(plot.ids)*frac.mc, replace = T)
  tree.mc.dt <- tree.dt[plot.id %in% mc.plots.ids]
  tree.mc.dt$rep <- i
  tree.mc.lst[[i]] <- tree.mc.dt
}
tree.mc.dt <- rbindlist(tree.mc.lst)

# COMPUTE AVERAGE TREE AGB AT EACH DBH INCREMENT FOR EACH MONTE CARLO SIMULATION --------------------------------------------------------

# compute tree AGB for each species for each 1 cm increment
tree.mc.avg.dt <- tree.mc.dt[, .(agc.kg.avg = weighted.mean(agc.kg, stems.n),
                                 stems.n.sp.dbh.mc = sum(stems.n), 
                                 stems.n.sp = mean(stems.n.sp)), by = c('common.name','dia.cm.rnd','rep')]

tree.mc.avg.dt <- tree.mc.avg.dt[order(common.name, rep, dia.cm.rnd)]

# compute cumulative % of stems by size class for eac species
tree.mc.avg.dt <- tree.mc.avg.dt[, ':='(stems.n.sp.mc = sum(stems.n.sp.dbh.mc), 
                                        stems.pcnt.sp.dbh.mc = stems.n.sp.dbh.mc / sum(stems.n.sp.dbh.mc) * 100), 
                                 by = c('common.name','rep')]

tree.mc.avg.dt <- tree.mc.avg.dt[, ':='(stems.cum.pcnt.sp.dbh.mc = cumsum(stems.pcnt.sp.dbh.mc)), 
                                 by = c('common.name','rep')]

# focus on DBH range that encompasses 99% of trees for each species
tree.mc.avg.dt <- tree.mc.avg.dt[stems.cum.pcnt.sp.dbh.mc <= 99.5]

# apply moving average to agb by dbh
# tree.mc.avg.dt[, agc.kg.avg := movingFun(agc.kg.avg, 5, mean, type = 'around', na.rm = F), by = c('common.name','rep')]

# compute AGC increment for each cm 
tree.mc.avg.dt <- tree.mc.avg.dt[, agc.inc.kg.avg := agc.kg.avg - data.table::shift(agc.kg.avg), by = c('rep','common.name')]

# summarize across Monte Carlo iterations
tree.mc.avg.smry.dt <- tree.mc.avg.dt[, .(agc.kg.avg.med = round(median(agc.kg.avg, na.rm = T)),
                                          agc.kg.avg.q025 = round(quantile(agc.kg.avg, 0.025, na.rm = T), 1),
                                          agc.kg.avg.q975 = round(quantile(agc.kg.avg, 0.975, na.rm = T), 1),
                                          agc.inc.kg.avg.med = round(median(agc.inc.kg.avg, na.rm = T)),
                                          agc.inc.kg.avg.q025 = round(quantile(agc.inc.kg.avg, 0.025, na.rm = T), 1),
                                          agc.inc.kg.avg.q975 = round(quantile(agc.inc.kg.avg, 0.975, na.rm = T), 1)),
                                      by = c('common.name','dia.cm.rnd')]


# example size classes
tree.exmpl.sizes.dt <- tree.mc.avg.smry.dt[dia.cm.rnd == 25 | dia.cm.rnd == 75]
tree.exmpl.sizes.dt[, agc.ratio := agc.kg.avg.med / lag(agc.kg.avg.med,1)]
tree.exmpl.sizes.dt
tree.mc.avg.smry.dt[dia.cm.rnd == 25 | dia.cm.rnd == 26]


# PLOT TREE AGC AND AGC INCREMENT BY DBH FOR EACH SPECIES  ---------------------------------------------
# plot tree AGC by DBH
tree.agc.fig <- ggplot(tree.mc.avg.smry.dt, aes(dia.cm.rnd, agc.kg.avg.med, group = common.name)) + 
  geom_ribbon(aes(x = dia.cm.rnd, ymin=agc.kg.avg.q025, ymax=agc.kg.avg.q975, fill=common.name), alpha = 0.25)+ 
  geom_line(aes(color = common.name)) +
  ylab("Tree aboveground carbon (kg)") + xlab('Tree diameter (cm)') + 
  geom_vline(xintercept = 21*2.54, lty = 2) + labs(col='Common name', fill='Common name') + 
  theme_bw() + theme(legend.position = c(0.2,0.7), legend.text=element_text(size=10), legend.title=element_text(size=12),
                     axis.text=element_text(size=11), axis.title=element_text(size=12))

tree.agc.fig

jpeg('figures/Tree_AGC_by_DBH.jpg', width = 6.5, height = 4, res = 400, units = 'in')
tree.agc.fig
dev.off()


# plot tree AGC increment by DBH
tree.agc.inc.fig <- ggplot(tree.mc.avg.smry.dt, aes(dia.cm.rnd, agc.inc.kg.avg.med, group = common.name)) + 
  geom_ribbon(aes(x = dia.cm.rnd, ymin=agc.inc.kg.avg.q025, ymax=agc.inc.kg.avg.q975, fill=common.name), alpha = 0.25)+ 
  geom_line(aes(color = common.name)) +
  ylab(expression("Tree aboveground carbon increment (kg cm"^-1~")")) + xlab('Tree diameter (cm)') + 
  geom_vline(xintercept = 21*2.54, lty = 2) + labs(col='Common name', fill='Common name') + 
  theme_bw() + theme(legend.position = 'none', legend.text=element_text(size=10), legend.title=element_text(size=12),
                     axis.text=element_text(size=11), axis.title=element_text(size=12))

tree.agc.inc.fig

# combine figures
jpeg('figures/Tree_Allometry_and_Increment.jpg', width = 10, height = 4, res = 400, units = 'in')
ggarrange(tree.agc.fig, tree.agc.inc.fig, labels=c('(a)','(b)'), label.x = 0.85, label.y = 0.98)
dev.off()






# plot tree AGC by DBH
tree.agc.fig <- ggplot(tree.smry.dt, aes(dia.cm.rnd, agc.kg.avg, group = common.name)) + 
  geom_line(aes(color = common.name)) +
  ylab("Tree aboveground carbon (kg)") + xlab('Tree diameter (cm)') + 
  geom_vline(xintercept = 21*2.54, lty = 2) + labs(col='Common name', fill='Common name') + 
  theme_bw() + theme(legend.position = c(0.2,0.7), legend.text=element_text(size=10), legend.title=element_text(size=12),
                     axis.text=element_text(size=11), axis.title=element_text(size=12))

tree.agc.fig

# plot tree AGC increment by DBH
tree.agc.inc.fig <- ggplot(tree.smry.dt, aes(dia.cm.rnd, agc.inc.kg, group = common.name)) + 
  geom_line(aes(color = common.name)) +
  ylab(expression("Tree aboveground carbon increment (kg cm"^-1~")")) + xlab('Tree diameter (cm)') + 
  geom_vline(xintercept = 21*2.54, lty = 2) + labs(col='Common name', fill='Common name') + 
  theme_bw() + theme(legend.position = 'none', legend.text=element_text(size=10), legend.title=element_text(size=12),
                     axis.text=element_text(size=11), axis.title=element_text(size=12))

tree.agc.inc.fig

# combine figures
jpeg('figures/Tree_Allometry_and_Increment.jpg', width = 10, height = 4, res = 400, units = 'in')
ggarrange(tree.agc.fig, tree.agc.inc.fig, labels=c('(a)','(b)'), label.x = 0.85, label.y = 0.98)
dev.off()




allom.dt[, agc.kg.cumsum := cumsum(agc.kg), by = species]
xx <- allom.dt[dbh.cm ==  25 | dbh.cm == 53]
xx[order(dbh.cm)]
xx.wide <- dcast(xx, species ~ dbh.cm, value.var = 'agc.kg')
xx.wide$frac <- xx.wide$`53`/ xx.wide$`25`
xx.wide
