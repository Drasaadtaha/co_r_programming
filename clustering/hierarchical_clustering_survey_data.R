
# Hierarchical Clustering

# this task can be thought as a market segmentation task, where we can divide a 
# broad target market of customers to smaller more similar groups and design a 
# marketing strategy specifically for each group. For instance, shoppers within each
# subgroup can be shown items and advertisements that are particularly likely to interest them

# Changes:
# - I replaced the 'prefecture' column with regions and now the regions column has only 5 levels. That way we can include the location in the clustering.
# - I created a function for visualization (and adjustment) and it now includes the breaks (as you also mention in the code)


# Reference for the Dataset: https://github.com/emilylaiken/togo-targeting-replication/blob/master/data/survey_indiv2018.csv

# required R packages
library(data.table)
library(FD)
library(dendextend)
library(AMR)
library(cluster)
library(ggplot2)

# file path
pth_survey = 'data/survey_indiv2018.csv'
pth_region = 'data/inferred_home_locations2018.csv'

# check that the 'survey_indiv2018.csv' file exists
file.exists(pth_survey)

# read the data
dat_survey = data.table::fread(file = pth_survey,
                               stringsAsFactors = TRUE,      # we'll specify 'stringsAsFactors' TRUE because we have compute the distance matrix for different types of columns
                               header = TRUE)
# observe the structure of the data
str(dat_survey)
# observe the class of the data
class(dat_survey)

dat_survey
str(dat_survey)
# compute the summary of the data
summary(dat_survey$age)

# check that the 'inferred_home_locations2018.csv' file exists
file.exists(pth_region)

# read the locations
dat_regions = data.table::fread(file = pth_region,
                                stringsAsFactors = TRUE,
                                header = TRUE) 

# Keep only the 'region' and 'prefecture' columns which are required for merging
dat_regions = dat_regions[, c('region', 'prefecture')]

# create an index with the duplicated prefectures so that we can remove these before merging
dups = duplicated(x = dat_regions$prefecture) |>
  which()

# compute the length of the duplicates
LEN_dups = length(x = dups)

# remove the duplicated observations of the 'dat_regions' object
if (LEN_dups > 0) {
  message(glue::glue("{LEN_dups} duplicated observations of the 'dat_regions' object will be removed!"))
  dat_regions = dat_regions[-dups, , drop = FALSE]
}

# merging of the two datasets by the 'prefecture' column, results in fewer than 10000 rows because specific 
# 'prefectures' of the 'dat_survey' object are not included in the 'regions' of the 'dat_regions' object
dat_survey = merge(x = dat_survey, y = dat_regions, by = 'prefecture')

# the unique region levels
table(dat_survey$region)

# we remove the prefecture and keep the regions which has 
dat_survey$prefecture = NULL

# check if there are missing values in all columns 
# the "is.na(dat_survey)" will create a TRUE or FALSE matrix where each value of the output matrix will be TRUE or FALSE
# Then we use "colSums()" to sum the TRUE or FALSE and see how many TRUE we have which will correspond to the missing values
colSums(x = is.na(dat_survey))
# is.na(dat_survey)

# dat_survey[, 'age'] in the next line selects the 'age' column
# colSums(x = dat_survey[, 'age'])

# The gower distance applies a different function to each predictor depending on its type (numeric, ordered, factor). This dissimilarity measure is implemented 
# in many R packages, among others in the cluster package (daisy function) and in the FD package (gowdis function). I’ll take advantage of the gowdis function
# of the FD package as it also allows user-defined weights for each separate predictor,

# we convert the binary variable 'own_phone' to numeric otherwise we will 
# receive the error: Binary variables should be of class 'numeric'
# dat_survey$own_phone = as.numeric(dat_survey$own_phone)

# compute the min and max age
MIN_age = min(dat_survey$age)
MAX_age = max(dat_survey$age)

# seq_ages = seq(from = MIN_age, to = MAX_age, by = 10)
# seq_ages = seq(from = MIN_age, to = MAX_age, length.out = 8)

# we selected the following age splits based on a search related to market segmentation and marketing
seq_ages = c(18, 25, 35, 45, 55, 65)
str(seq_ages)

# The "AMR::age_groups()" function creates the age classes based on the numeric age column
vec_ages = AMR::age_groups(x = dat_survey$age,
                           split_at = seq_ages,
                           na.rm = FALSE)
str(vec_ages)
table(vec_ages)

# the next line of code includes multiple functions. R evaluates each expression starting from inside. That means:
# - first will be evaluated the "table(vec_ages)"
# - then it will be evaluated the "prop.table(x = table(vec_ages))"
# - then it will be evaluated the "round(x = prop.table(x = table(vec_ages)), digits = 4)"
round(x = prop.table(x = table(vec_ages)), digits = 4) * 100

# we create a new column named as "age_class"
dat_survey$age_class = vec_ages
str(dat_survey)
table(dat_survey$age)

# we compute the distance object using the "cluster::daisy()" function
gwd = cluster::daisy(x = dat_survey[, !'age'], metric = 'gower')

# # compute the distance matrix
# gwd = FD::gowdis(x = dat_survey)
str(gwd)

# mt_gwd = as.matrix(gwd)
# str(mt_gwd)

# mt_gwd[1:10, 1:10]
# mt_gwd[1, 2:ncol(mt_gwd)]

# information about the method used for hierarchical clustering [ function documentation - details section ]
#
# A number of different clustering methods are provided. Ward's minimum variance method aims at finding compact, spherical clusters. 
# The complete linkage method finds similar clusters. The single linkage method (which is closely related to the minimal spanning tree) 
# adopts a ‘friends of friends’ clustering strategy. 
# 'Complete' & 'Average' is preferred compared to a 'Single' Linkage.

# we can think about the 'ward' method as trying to minimize the variance within each cluster and the distance among clusters
hcl = stats::hclust(d = gwd, method = 'complete')

# ?stats::hclust
# help(hclust)
str(hcl)

# plot the dendrogram
# dend <- gwd %>% hclust %>% as.dendrogram
plot(hcl)

# in practice people often look at the dendrogram and select by eye a sensible number of clusters 
# based on the height of the fusion and the number of clusters desired

# the vertical lines depict the distance between 2 nodes or clusters. The taller the line the more 
# dissimilar the clusters are. The distance (information) between clusterscan guide our choice of the 
# number of clusters. A good partition belongs to a cut that has a good enough room to move up and down.

# based on this information we select 4 clusters and plot also the rectangles around the clusters
# k = 4
# k = 7
# k = 13
k = 20
stats::rect.hclust(tree = hcl, k = k, border = 'red')

# we compute the clusters using the "stats::cutree()" function
clusts_h = stats::cutree(tree = hcl, k = k)
str(clusts_h)
table(clusts_h)

# # copying the clusters
# clusts_h_previous = clusts_h

df_clusts = list(thirteen = clusts_h_previous,
                 twenty = clusts_h) |>
  data.table::setDT()

# subset of clusters
subs_first = subset(x = df_clusts, subset = thirteen == 1)
table(subs_first$twenty)

# indices
idx_first = which(x = df_clusts$thirteen == 1)
str(idx_first)

# subset tapply
gender_tbl_subs = tapply(X = dat_survey$gender[idx_first], INDEX = clusts_h[idx_first], FUN = table)
tapply(X = dat_survey$gender, INDEX = clusts_h_previous, FUN = table)[[1]]


# We have to find any pattern using the "tapply()" function, each column of the data and the computed clusters with the "table()" function
# We create a new object for each one of the columns (ending in "_tbl") so that we can run one by one the clusters and see if there are any groups.
gender_tbl = tapply(X = dat_survey$gender, INDEX = clusts_h, FUN = table)
# age_tbl = tapply(X = dat_survey$age_class, INDEX = clusts_h, FUN = function(x) round(prop.table(table(x)), digits = 4) * 100)
age_tbl = tapply(X = dat_survey$age_class, INDEX = clusts_h, FUN = table)
phone_tbl = tapply(X = dat_survey$own_phone, INDEX = clusts_h, FUN = table)
pref_tbl = tapply(X = dat_survey$region, INDEX = clusts_h, FUN = table)

# # for instance for the first cluster
# CLUSTER_idx = 1
# gender_tbl[[CLUSTER_idx]]
# age_tbl[[CLUSTER_idx]]
# phone_tbl[[CLUSTER_idx]]
# 
# we can also split the data based on the output clusters
spl = split(x = dat_survey, f = clusts_h)
str(spl)
# 
# # we keep a subset based on the first cluster
# sub_clust = spl[[CLUSTER_idx]]
# sub_clust_out = subset(sub_clust, age_class == '65+')
# dim(sub_clust_out)
# 
# table(sub_clust_out$own_phone)
# 
# # we use ggplot2 to create a stacked bar-plot
# plt = ggplot2::ggplot(data = sub_clust, ggplot2::aes(x = own_phone, fill = age_class)) +
#   ggplot2::geom_bar() +
#   ggplot2::facet_wrap(~gender)


# we can also include the clusters in the initial data
dat_survey$clusters = clusts_h
dat_survey_keep = dat_survey[, !'age']
dat_survey_keep

# survey_melt = reshape2::melt(data = dat_survey_keep, id.vars = c('clusters', 'own_phone'))
# # warning : attributes are not identical across measure variables; they will be dropped

# a ggplot2 function to avoid repetitive code  [ the 'fill' and 'facet_wrap' default to the same columns, i.e. 'age_class' and 'clusters'
ggplot_visualize = function(data,
                            split_obj,
                            x_column,
                            angle = 0,
                            vjust = NULL, 
                            hjust = NULL) {
  
  plt = ggplot2::ggplot(data = data, 
                          mapping = ggplot2::aes(x = .data[[x_column]], fill = age_class)) +
    ggplot2::geom_bar() +
    ggplot2::facet_wrap(facets = ~clusters, scales = "free_y") +
    ggplot2::scale_y_continuous(breaks = seq(from = 0, to = max(unlist(lapply(split_obj, function(x) max(table(x[[x_column]]))))), by = 100)) +  # workaround with lapply() to get the maximum of the counts
    ggplot2::theme(panel.background = element_rect(fill = "white", colour = "white"),     # the background becomes white
                   axis.text.x = ggplot2::element_text(face = "bold", angle = angle, vjust = vjust, hjust = hjust),
                   panel.grid.major = ggplot2::element_line(color = "black", linewidth = 0.2, linetype = "solid"),  # the 'major' line is thicker (0.2)
                   panel.grid.minor = ggplot2::element_line(color = "black", linewidth = 0.1, linetype = "solid"))  # the 'minor' line is thiner (0.1)
  return(plt)
}

# Bar-plot ("own_phone")
plt_1 = ggplot_visualize(data = dat_survey_keep,
                       split_obj = spl,
                       x_column = 'own_phone')

# Bar-plot ("gender")
plt_2 = ggplot_visualize(data = dat_survey_keep,
                         split_obj = spl,
                         x_column = 'gender')

# Bar-plot ("region")
plt_2a = ggplot_visualize(data = dat_survey_keep,
                          split_obj = spl,
                          x_column = 'region',
                          angle = 35,
                          vjust = 1,
                          hjust = 1)

# Bar-plot ("gender+ facet_wrap:region")
plt_2b = ggplot2::ggplot(data = dat_survey_keep, ggplot2::aes(x = gender, fill = age_class)) +
  ggplot2::geom_bar() +
  ggplot2::facet_wrap(~region, scales = "free")

# Bar-plot ("gender+ facet_wrap:region")
plt_2c = ggplot2::ggplot(data = dat_survey_keep, ggplot2::aes(x = own_phone, fill = age_class)) +
  ggplot2::geom_bar() +
  ggplot2::facet_wrap(~region, scales = "free")

# Pie-Chart ("own_phone")
cp <- ggplot2::coord_polar(theta = "y")
cp$is_free <- function() TRUE

plt_3_true = ggplot2::ggplot(data = subset(dat_survey_keep, own_phone == TRUE), 
                             mapping = ggplot2::aes(x = own_phone, fill = age_class)) +
  ggplot2::geom_bar(position="fill") +
  ggplot2::geom_text(stat='count', ggplot2::aes(y=ggplot2::after_stat(..count..),
                 label=ggplot2::after_stat(..count..)),
    position=ggplot2::position_fill(0.5)) +
  cp +
  ggplot2::facet_wrap(~clusters, scales = "free") +
  ggplot2::theme(aspect.ratio = 1) +
  ggplot2::labs(x=NULL, y=NULL)

plt_3_false = ggplot2::ggplot(data = subset(dat_survey_keep, own_phone == FALSE), 
                              mapping = ggplot2::aes(x = own_phone, fill = age_class)) +
  ggplot2::geom_bar(position="fill") +
  ggplot2::geom_text(stat='count', ggplot2::aes(y=ggplot2::after_stat(..count..),
                                                label=ggplot2::after_stat(..count..)),
                     position=ggplot2::position_fill(0.5)) +
  cp +
  ggplot2::facet_wrap(~clusters, scales = "free") +
  ggplot2::theme(aspect.ratio = 1) +
  ggplot2::labs(x=NULL, y=NULL)

  
# Pie-Chart ("gender")
plt_4 = ggplot2::ggplot(data = dat_survey_keep, ggplot2::aes(x = gender, fill = age_class)) +
  ggplot2::geom_bar(position="fill") +
  ggplot2::geom_text(stat='count', ggplot2::aes(y=ggplot2::after_stat(..count..),
                                                label=ggplot2::after_stat(..count..)),
                     position=ggplot2::position_fill(0.5)) +
  cp +
  ggplot2::facet_wrap(~clusters, scales = "free") +
  ggplot2::theme(aspect.ratio = 1) +
  ggplot2::labs(x=NULL, y=NULL)


# Base R pie charts take as input only a vector which means we can not add the same information we add in a ggplot2 plot
# reference: http://www.sthda.com/english/wiki/pie-charts-r-base-graphs
# 

# #........................................................................................................................................................ commented out
# # lapply(spl, colMeans)                      # this command would have return the cluster centroids if all columns of the data were of type numeric
# 
# # potential questions that we can answer based on the resulted clusters:
# #
# #  - which clusters are dominated by Females and which by males?
# #  - in which clusters the females & males own a phone and in which they don't?
# #  - does the histogram distribution of the column age show any differences between the clusters?
# #  - how can we generally describe the survey participants based on the clusters?
# #  - are there any differences in gender, age & owing a phone for the prefecutres of each cluster?
# 
# # in case we want to focus on a specific cluster or group
# 
# # Subset the distance matrix to keep only the 1st cluster
# viz_clust = 1
# 
# first_cluster_samples <- which(clusts_h == viz_clust)
# dist_mat_subset <- as.matrix(gwd)[first_cluster_samples, first_cluster_samples]
# dim(dist_mat_subset)
# 
# # Perform hierarchical clustering again on the subset
# hc_first_cluster <- hclust(as.dist(dist_mat_subset), method = "ward")
# 
# # Plot the dendrogram of the first cluster
# plot(hc_first_cluster, main = "Dendrogram of the First Cluster")
# 
# 
# #........................................................... computationally intensive  [ it takes too long ]
# # # Prune the dendrogram to keep only the first cluster
# # dend <- stats::as.dendrogram(hcl)
# # labels_dend <- labels(dend)
# # labels_to_keep <- labels_dend[clusts_h == 1]
# # dend1 <- dendextend::prune(dend, labels_to_keep)
# # 
# # # Plot the dendrogram of the first cluster
# # plot(dend1, main = "Dendrogram of the First Cluster")
# #...........................................................

