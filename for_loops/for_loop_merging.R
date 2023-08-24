
# Compare output merged data.frames using a for-loop and the data.table R package

# Required R packages
if (!require(pacman)) install.packages("pacman");
pacman::p_load(data.table,
               glue,
               install = TRUE)

# number of locations to use
city = 6
district = (city / 2)

# create a gender & location data.table
city_district = lapply(X = 1:city, FUN = function(x) {
  list(city = rep(x = glue::glue("city_{x}"), length.out = district)) |>
    data.table::setDT() 
  }) |>
  data.table::rbindlist()

city_district$district = glue::glue("district_{1:nrow(city_district)}")
city_district$ID = 1:nrow(city_district)
city_district

# # we want to create a duplicated item
# vect1 = as.character(city_district$district)
# # vect1[4] = "district_3"
# 
# idx_vect = c(4:6, 8)
# vect1[idx_vect] = "district_3"
# 
# dups = duplicated(x = vect1)
# sum(dups)

# set the seed (for age mainly) and create the age & location data.table
set.seed(seed = 1)
age_district = list(age = sample(x = 18:99, size = nrow(city_district), replace = TRUE),
                    district = city_district$district) |>
  data.table::as.data.table()

# merging using the 'merge()' function
merg_dtbl = merge(x = city_district, y = age_district, by = 'district')

# order the merged data.table
merg_dtbl = merg_dtbl[order(merg_dtbl$ID, decreasing = FALSE), ]

# remove the ID column after we sorted the data
merg_dtbl$ID = NULL

# we will merge the 2 data.frames using a for-loop by the common district column
district_names = city_district$district
str(district_names)

#......................................................... create a time-series objects
# set.seed(seed = 1)
# vec_rnorm = rnorm(n = 24, mean = 0, sd = 1)
# 
# df = data.frame(year = 2000:2023, price = vec_rnorm)
# 
# ts_obj = stats::ts(data = df$price, frequency = 1)
# attr(x = ts_obj, which = 'year') = df$year
#.........................................................

# list to save each iteration's merged subset row
merg_loop = list()

# rep(x = c(TRUE, FALSE), length.out = 18)
# sample(x = c(TRUE, FALSE), size = 18, replace = TRUE)

# for (i in ) {
# for (i in letters[1:18]) {
# for (i in c(1,2,2,3,4,5)) {
for (i in 1:length(district_names)) {
#   print(i)
# }
  # the unique district name of this iteration
  district_nam_iter = district_names[i]
  
  # the subset rows of the city and age data.frames using the district name
  subs_city_df = subset(x = city_district, subset = district == district_nam_iter)
  subs_age_df = subset(x = age_district, district == district_nam_iter)
  
  # we checking that we have only 1 row in both data.sets
  if (nrow(subs_city_df) != 1) stop("I expect 1 row for the 'city' data.frame!")
  if (nrow(subs_age_df) != 1) stop("I expect 1 row for the 'age' data.frame!")
  
  # we include the age data of this iteration's age-subset to the city-subset
  subs_city_df$age = subs_age_df$age
  
  # append the merged subset to the list outside of the for-loop
  merg_loop[[i]] = subs_city_df
}

# class(merg_loop)

# rbind() all rows
merg_loop = do.call(what = rbind, args = merg_loop) |>
  data.table::as.data.table()

# # order the merged for-loop data.table
# merg_loop = merg_loop[order(merg_loop$ID, decreasing = FALSE), ]

# remove the ID column after we sorted the data
merg_loop$ID = NULL

# set the order of the column names the same as is in the first merged data.frame
# colnames_merg = colnames(merg_dtbl)
colnames_merg = c("district", "city", "age")
merg_dtbl_loop = merg_loop[, ..colnames_merg]

# check if we have identical merged output data.frames
identical(x = merg_dtbl, y = merg_dtbl_loop)
# TRUE
