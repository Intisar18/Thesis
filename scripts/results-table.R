
readRDS(file.path("results", "test_mse_rf_sif_sentence.RDS")


result_type <- c("lasso", "rf", "rf-sif")

result_paths <- 
	map(dir(file.path("results")), ~file.path("results", .x)) 

results <- map(results_paths, readRDS)


results_df <- 
	results %>% 
	do.call(cbind, .) %>% 
	data.frame

names(results_df) <- result_type

results_df