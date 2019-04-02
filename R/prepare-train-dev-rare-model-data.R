# This function gets the training and dev data for a rare 
# model
prepare_train_dev_rare_model_data <- 
	function(emotion, all_data, word_features){

    emotions = c("anger", "joy", "sadness", "fear")
	stopifnot(emotion %in% emotions)

	# Get training sets
	training_sets <- purrr::map(all_data, ~.x$train )
	dev_sets <- purrr::map(all_data, ~.x$dev)
	# test_sets <- purrr::map(all_data, ~.x$test)

	# Get test data with intensity score
	test_sets <- lapply(emotions, 
					   get_data_set, 
					   data_dir = 'semeval-test-data', 
					   data_names = 'test') 

	test_sets <- unlist(test_sets, recursive = FALSE)
	names(test_sets) <- emotions

	data_train <- training_sets[[emotion]]
	data_dev <- dev_sets[[emotion]]
	data_test <- test_sets[[emotion]]


	names(data_train) <- c("ID", "Tweet", "Affect", "Intensity")
	names(data_dev) <- c("ID", "Tweet", "Affect", "Intensity")
	names(data_test) <- c("ID", "Tweet", "Affect", "Intensity")

	t_data_train <- tidy_tweets(data_train)
	t_data_dev <- tidy_tweets(data_dev)
	t_data_test <- tidy_tweets(data_test)


	# Get saved features

	# word_features <- readRDS(file.path("data","word_features.RDS"))

	train_res <- prepare_rare_model_data(t_data_train, word_features)
	dev_res <- prepare_rare_model_data(t_data_dev, word_features)
	test_res <- prepare_rare_model_data(t_data_test, word_features)

	train_prep <- train_res$augmented_data
	dev_prep <- dev_res$augmented_data
	test_prep <- test_res$augmented_data

	# Get the DTM of the filtered data
	train_dtm <-
		train_prep %>%
		# document ID, term column, values
		tidytext::cast_dtm(ID, word, n)

	# Get the DTM of the filtered data
	dev_dtm <-
		dev_prep %>%
		# document ID, term column, values
		tidytext::cast_dtm(ID, word, n)


	# Get the DTM of the filtered data
	test_dtm <-
		test_prep %>%
		# document ID, term column, values
		tidytext::cast_dtm(ID, word, n)

	# THe last ID is the dummy which we give zero intensity
	train_dtm <- train_dtm[1:(nrow(train_dtm) - 1), ]
	dev_dtm <- dev_dtm[1:(nrow(dev_dtm) - 1), ]
	test_dtm <- test_dtm[1:(nrow(test_dtm) - 1), ]
	
	# ----------------------------------------------------------------------
	# Get Response
	# -----------------------------------------------------------------------
	train_intensity <-
		data_train %>%
		group_by(ID) %>%
		dplyr::filter(ID %in% train_res$retained_ID) %>%
		pull(Intensity)


	dev_intensity <-
		data_dev %>%
		group_by(ID) %>%
		dplyr::filter(ID %in% dev_res$retained_ID) %>%
		pull(Intensity)

	test_intensity <-
		data_test %>%
		group_by(ID) %>%
		dplyr::filter(ID %in% test_res$retained_ID) %>%
		pull(Intensity)

	return(
		list(
		 train_dtm = train_dtm, 
		 dev_dtm = dev_dtm, 
		 test_dtm = test_dtm,
		 train_intensity = train_intensity, 
		 dev_intensity = dev_intensity, 
		 test_intensity = test_intensity
		 ))
}