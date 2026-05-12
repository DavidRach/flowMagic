test_that("flowMagic installs and produces ggplot2 plots", {

#library(sp) 
#library(stringr)
#library(ggplot2)
#library(parallel) 
#library(doParallel)
#library(randomForest) 
#library(caret)
#library(concaveman)
#library(sm)
#library(pracma)
#library(sf)
#library(stats)
#library(grDevices)
#library(flowMagic)
  
#------------ using template model with 1 template
# get path to directory with files to analyze
path_dir<-system.file("extdata",package = "flowMagic")
# import data with labels that we use as template data.
list_data_ref<-import_reference_csv(path_results = path_dir,n_cores = 1)
# import data without labels
list_test_data<-import_test_set_csv(path_data = path_dir,n_cores = 1)
# Note that it is possible to provide also directly the paths to each file. 
# See functions manual for additional details.
# data preprocessing to generate the template model using first file as template
# we select first element of the imported list of dataframes
ref_train<-get_train_data(paths_file = list_data_ref[1],n_cores = 1) 
# generate template model using out-of-the-bag validation
ref_model_info<-magicTrain(df_train = ref_train,n_cores = 1,train_model = "rf")
# perform automated gating (gates boundaries prediction step)
list_dfs_pred<-magicPred_all(list_test_data = list_test_data,magic_model = NULL,ref_data_train = ref_train,
ref_model_info = ref_model_info,n_cores = 8)
# Note that providing the training set is optional (ref_data_train = ref_train is optional).
# Providing the training set allows the user to calculate the target-template distance for each plot to analyze.
# list_dfs_pred contains a list of dataframes for each plot analyzed. In other words, it is a nested list (e.g.,
# downsampled dataset and original dataset with predicted labels for each plot). 
# See the functions manual for the full list of dataframes returned.
# visualize gated data
df_temp<-list_dfs_pred[[1]] $ df_test_original # dataframe of first gated plot
plot1 <- magicPlot(df = df_temp,type = "ML",size_points = 1)
plot2 <- magicPlot(df = df_temp,type = "dens",size_points = 1)

expect_s3_class(plot1, "ggplot")
expect_s3_class(plot2, "ggplot")

expect_true(ggplot2::is_ggplot(plot1))
expect_true(ggplot2::is_ggplot(plot2))  
  
})