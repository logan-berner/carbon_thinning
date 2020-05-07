# THIS R SCRIPT GRABS USFS FOREST INVENTORY AND ANALYSIS DATA FROM ONLINE
# AUTHOR: LOGAN BERNER
# DATE: 2020-05-02

rm(list=ls())
require(R.utils)
require(data.table)

setwd('C:/Users/Logan/Google Drive/research/side_projects/mildrexler_oregon_forests/carbon_thinning/')

# CREATE DOWNLOAD DIRECTORIES
mkdirs('data/')
mkdirs('data/fia')
mkdirs('data/fia/state')
mkdirs('data/gis_data/')

# DOWNLOAD TREE AND PLOT FILES ---------------------------------------------------------------

# identify files to download from USFS
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
# download each file
for (i in 1:nrow(files.dt)){
  download.file(files.dt$url[i], destfile = paste0('data/fia/state/', files.dt$file[i]))
  print(i)
}

# combine files from multiple states
trees.dt <- do.call("rbind", lapply(list.files('data/fia/state/', pattern = 'TREE.csv', full.names = T), fread))
plots.dt <- do.call("rbind", lapply(list.files('data/fia/state/', pattern = 'PLOT.csv', full.names = T), fread))

# write out
fwrite(trees.dt, 'data/fia/WAORID_fia_tree_surveys.csv')
fwrite(plots.dt, 'data/fia/WAORID_fia_plots.csv')


# DOWNLOAD ANCILLARY FIA FILES ---------------------------------------------------------------
sp.codes.url <- 'https://apps.fs.usda.gov/fia/datamart/CSV/REF_SPECIES.csv'
download.file(sp.codes.url, 'data/fia/REF_SPECIES.CSV')


# DOWNLOAD SHAPEFILE WITH NATIONAL FOREST BOUNDARIES ---------------------------------------------------------------
nf.shp.url <- 'https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeForest.zip'
nf.shp.zip <- 'data/gis_data/nf_boundaries.zip'
nf.shp.file <- 'data/gis_data/national_forest_boundaries.shp'
download.file(nf.shp.url, nf.shp.zip)
unzip(nf.shp.zip, exdir = 'data/gis_data')
