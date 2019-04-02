
# Load fastText sub word based word vectors
get_fastText_word_vectors <- function(){

	word_vector_file <-
	file.path("..", "word-vecs", "wiki-news-300d-1M-subword.vec")

	ft_word_vec <- data.table::fread(word_vector_file, skip=1)

	names(ft_word_vec) <- c("word", paste0("V", 1:300))

	ft_word_vec
}
