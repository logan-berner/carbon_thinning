# THIS R SCRIPT GRABS USFS FOREST INVENTORY AND ANALYSIS DATA FROM ONLINE
# AUTHOR: LOGAN BERNER
# DATE: 2020-05-02

rm(list=ls())
require(R.utils)
require(data.table)

setwd('C:/Users/Logan/Google Drive/research/side_projects/mildrexler_oregon_forests/carbon_thinning/')

# IDENTIFY FILES TO DOWNLOAD
files.dt <- data.table(url = c('https://apps.fs.usda.gov/fia/datamart/CSV/ID_TREE.csv',
                               'https://apps.fs.usda.gov/fia/datamart/CSV/OR_TREE.csv',
                               'https://apps.fs.usda.gov/fia/datamart/CSV/WA_TREE.csv',
                               'https://apps.fs.usda.gov/fia/datamart/CSV/ID_PLOT.csv',
                               'https://apps.fs.usda.gov/fia/datamart/CSV/OR_PLOT.csv',
                               'https://apps.fs.usda.gov/fia/datamart/CSV/WA_PLOT.csv'),
                       file = c('ID_TREE.csv',
                                'OR_TREE.csv',
                                'WA_TREE.csv',
                                'ID_PLOT.csv',
                                'OR_PLOT.csv',
                                'WA_PLOT.csv'))

# CREATE DOWNLOAD DIRECTORIES
mkdirs('data/fia')
mkdirs('data/fia/state')


# DOWNLOAD FILES
for (i in 2:nrow(files.dt)){
  download.file(files.dt$url[i], destfile = paste0('data/fia/state/', files.dt$file[i]))
  print(i)
}

# COMBINE FILES FROM MULTIPLE STATES
trees.dt <- do.call("rbind", lapply(list.files('data/fia/state/', pattern = 'TREE.csv', full.names = T), fread))
plots.dt <- do.call("rbind", lapply(list.files('data/fia/state/', pattern = 'PLOT.csv', full.names = T), fread))

# WRITE OUT
fwrite(trees.dt, 'data/fia/WAORID_fia_tree_surveys.csv')
fwrite(plots.dt, 'data/fia/WAORID_fia_plots.csv')

