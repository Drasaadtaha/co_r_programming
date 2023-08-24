
# Reference for the Dataset: https://github.com/emilylaiken/togo-targeting-replication/blob/master/data/survey_indiv2018.csv

library(data.table)
library(ClusterR)
library(FD)
library(fpc)
library(glue)
library(dendextend)
library(ggdendro)
library(magrittr)

#................................................................................... Installation of packages either from CRAN or remotely
# CRAN                          : install.packages("data.table")
# Github, Gitlab, Bitbucket     : remotes::install_github("Rdatatable/data.table")
#................................................................................... usage of Python from within Rstudio using the 'reticulate' R package
# # reticulate R package
# reticulate::py_config()
#................................................................................... alternative way to load multiple R packages
# if(!require(pacman)) install.packages("pacman");
# pacman::p_load(data.table,
#                ClusterR,
#                FD,
#                fpc,
#                glue,
#                install = TRUE)

# file path
pth_survey = 'data/survey_indiv2018.csv'

# check that the file exists
file.exists(pth_survey)
# if (!file.exists(pth_survey)) stop("The file does not exist!")    # check if a file exists and raise an error

# read the data
dat_survey = data.table::fread(file = pth_survey,
                               stringsAsFactors = TRUE,      # we'll specify 'stringsAsFactors' TRUE because we have compute the distance matrix for different types of columns
                               header = TRUE)
# observe the structure of the data
str(dat_survey)
# observe the class of the data
class(dat_survey)

#.......................................... observe the data
# colnames(dat_survey)
# dat_survey[['prefecture']]
# vec_pref = dat_survey$prefecture
# length(vec_pref)
#..........................................  subset vector
# idx = 1
# vec_pref[idx]
#.......................................... create vector & named vector
# vec = c(1,2,3)
# vec = c(one = 1, two = 2, three = 3)
# as.numeric(vec['one'])
#.......................................... create a list & named list
# lst = list(one = 1, two = 2, three = 3)
# lst$one
# lst[['one']]
#..........................................

# omit the 'prefecture' column of the data.table
dat_survey = dat_survey[ , !'prefecture']

# dat_survey[1:5, c('prefecture', 'age')]

dat_survey
str(dat_survey)

# compute the summary of the data
summary(dat_survey$age)

# The gower distance applies a different function to each predictor depending on its type (numeric, ordered, factor). This dissimilarity measure is implemented 
# in many R packages, among others in the cluster package (daisy function) and in the FD package (gowdis function). I’ll take advantage of the gowdis function
# of the FD package as it also allows user-defined weights for each separate predictor,

# we convert the binary variable 'own_phone' to numeric otherwise we will 
# receive the error: Binary variables should be of class 'numeric'
dat_survey$own_phone = as.numeric(dat_survey$own_phone)

# compute the distance matrix
gwd = FD::gowdis(x = dat_survey)
str(gwd)

# convert to matrix
gwd_mat = as.matrix(x = gwd)
dim(gwd_mat)
str(gwd_mat)
gwd_mat[1:10, 1:10]
class(gwd_mat)

# ----------------------------------------------------------------------------------- Optimal number of clusters (see screenshot)
# opt_clusts = ClusterR::Optimal_Clusters_Medoids(data = gwd_mat,
#                                                 max_clusters = 10,
#                                                 criterion = 'dissimilarity',
#                                                 threads = 6,
#                                                 distance_metric = 'euclidean',
#                                                 verbose = TRUE)

# DBSCAN  [ density based clustering ]
dbsc = fpc::dbscan(data = gwd, 
                   eps = 0.2, 
                   MinPts = 5, 
                   method = 'dist', 
                   showplot = 1)

str(dbsc)

# compute the frequency table of the data
table(dbsc$cluster)
# 1    2    3    4 
# 2478 4017 2495 1010 

# compute the length of the unique clusters
N_clusts = length(unique(dbsc$cluster))

# "Cluster Medoids" by specifying the number of clusters [ use the distance matrix as input to the cluster medoids function ]
clusts = ClusterR::Cluster_Medoids(data = gwd_mat, 
                                   clusters = N_clusts, 
                                   swap_phase = TRUE, 
                                   threads = 6,
                                   verbose = TRUE)


str(clusts)

# silhouette width: The silhouette value is a measure of how similar an object is to its own cluster (cohesion) compared to other clusters (separation).
# The silhouette ranges from −1 to +1, where a high value indicates that the object is well matched to its own cluster and poorly matched to neighboring
# clusters. If most objects have a high value, then the clustering configuration is appropriate. If many points have a low or negative value, then the 
# clustering configuration may have too many or too few clusters. [ https://en.wikipedia.org/wiki/Silhouette_(clustering) ]

# function to visualize the Silhouette width
ClusterR::Silhouette_Dissimilarity_Plot(clusts, silhouette = TRUE)

# add the clusters to the data
dat_survey$cluster = dbsc$cluster

# split the data by the 'cluster' column
dat_survey_spl = split(dat_survey, by = 'cluster')
dat_survey_spl

# contigency tables of the clusters using a for-loop
for (i in 1:length(dat_survey_spl)) {
  
  cat(glue::glue("--------------------  Cluster: {i}  ---------------------"), '\n')
  subset_cluster = dat_survey_spl[[i]]
  print(table(subset_cluster$gender, subset_cluster$own_phone))
  cat('\n')
  print(summary(subset_cluster$age))
}

