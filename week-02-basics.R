library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(forcats)
library(tibble)
library(scales)

file_a <- "texts/A07594__Circle_of_Commerce.txt"
file_b <- "texts/B14801__Free_Trade.txt"

text_a <- read_file(file_a)
text_b <- read_file(file_b)

texts <- tibble(
  doc_title = c("Text A", "Text B"),
  text = c(text_a, text_b)
)

texts

data("stop_words")

custom_stopwords <- tibble(
  word = c("vnto", "haue", "doo", "hath", "bee", "ye", "thee")
)

all_stopwords <- bind_rows(stop_words, custom_stopwords) %>%
  distinct(word)

all_stopwords %>% slice(1:10)

word_counts <- texts %>% 
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word)) %>%
  anti_join(all_stopwords, by = "word") %>%
  count(doc_title, word, sort = TRUE)

word_counts

plot_n_words <- 20  


word_comparison_tbl <- word_counts %>%
  pivot_wider(
    names_from = doc_title,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(max_n = pmax(`Text A`, `Text B`)) %>%
  arrange(desc(max_n))

word_plot_data <- word_comparison_tbl %>%
  slice_head(n = plot_n_words) %>%
  pivot_longer(
    cols = c(`Text A`, `Text B`),
    names_to = "doc_title",
    values_to = "n"
  ) %>%
  mutate(word = fct_reorder(word, n, .fun = max))

ggplot(word_plot_data, aes(x = n, y = word)) + 
  geom_col() +
  facet_wrap(~ doc_title, scales = "free_x") +
  labs(
    title = "Most frequent words (stopwords removed)",
    subtitle = paste0(
      "Top ", plot_n_words,
      " words by maximum frequency across both texts"
    ),
    x = "Word frequency",
    y = NULL
  ) +
  theme_minimal()

bigrams <- texts %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams

bigrams_separated <- bigrams %>%
  separate(bigram, into = c("word1", "word2"), sep = " ")

bigrams_separated

bigrams_filtered <- bigrams_separated %>%
  filter(
    !word1 %in% all_stopwords$word,
    !word2 %in% all_stopwords$word
  )

bigrams_filtered

bigram_counts <- bigrams_filtered %>%
  count(doc_title, word1, word2, sort = TRUE)

bigram_counts

bigram_counts <- bigram_counts %>%
  unite(bigram, word1, word2, sep = " ")

bigram_counts

bigram_relative <- bigram_counts %>%
  group_by(doc_title) %>%
  mutate(
    total_bigrams = sum(n),
    proportion = n / total_bigrams
  ) %>%
  ungroup()

bigram_wide <- bigram_relative %>%
  select(doc_title, bigram, proportion) %>%
  pivot_wider(
    names_from = doc_title,
    values_from = proportion,
    values_fill = 0
  )

bigram_wide


bigram_diff <- bigram_wide %>%
  mutate(
    diff = `Text A` - `Text B`
  ) %>%
  arrange(desc(abs(diff)))

bigram_diff %>% slice(1:20)

words <- texts %>%
  unnest_tokens(word, text)
words

tokens_before_removal <- words %>%
  count(doc_title, name = "total_tokens")
tokens_before_removal

words_after_stopwords <- words %>%
  anti_join(all_stopwords, by = "word")
words_after_stopwords

tokens_after <- words_after_stopwords %>%
  count(doc_title, name = "tokens_after_stopwords")
tokens_after


corpus_diagnostics <- tokens_before_removal %>%
  left_join(tokens_after, by = "doc_title") %>%
  mutate(
    tokens_removed = total_tokens - tokens_after_stopwords,
    percent_removed = tokens_removed / total_tokens * 100
  )

corpus_diagnostics

#56.% of Text A, Circle of Commerce, was removed as stop words compared to 59.8% of Text B, Free Trade.

#Yes the percentage is approximately the same with only 3.2% more of Text B being removed compared to Text A.

#The percentages of the texts that were removed are quite high which tells me that the texts will be transformed significantly.
#Whether or not this will drastically change the interpretation of each text is yet to be seen. 
#However, due to the high percentage of removal, I believe the interpretation of the texts will be at least slightly altered.

word_counts_before <- words %>%
  count(doc_title, word, sort = TRUE)
word_counts_before

plot_n_words <- 15  


word_comparison_tbl <- word_counts_before %>%
  pivot_wider(
    names_from = doc_title,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(max_n = pmax(`Text A`, `Text B`)) %>%
  arrange(desc(max_n))

word_plot_data <- word_comparison_tbl %>%
  slice_head(n = plot_n_words) %>%
  pivot_longer(
    cols = c(`Text A`, `Text B`),
    names_to = "doc_title",
    values_to = "n"
  ) %>%
  mutate(word = fct_reorder(word, n, .fun = max))

ggplot(word_plot_data, aes(x = n, y = word)) + 
  geom_col() +
  facet_wrap(~ doc_title, scales = "free_x") +
  labs(
    title = "Most frequent words BEFORE stopword removal",
    subtitle = paste0(
      "Top ", plot_n_words,
      " words by maximum frequency across both texts"
    ),
    x = "Word frequency",
    y = NULL
  ) +
  theme_minimal()

plot_n_words <- 15  


word_comparison_tbl <- word_counts %>%
  pivot_wider(
    names_from = doc_title,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(max_n = pmax(`Text A`, `Text B`)) %>%
  arrange(desc(max_n))

word_plot_data <- word_comparison_tbl %>%
  slice_head(n = plot_n_words) %>%
  pivot_longer(
    cols = c(`Text A`, `Text B`),
    names_to = "doc_title",
    values_to = "n"
  ) %>%
  mutate(word = fct_reorder(word, n, .fun = max))

ggplot(word_plot_data, aes(x = n, y = word)) + 
  geom_col() +
  facet_wrap(~ doc_title, scales = "free_x") +
  labs(
    title = "Most frequent words AFTER stopword removal",
    subtitle = paste0(
      "Top ", plot_n_words,
      " words by maximum frequency across both texts"
    ),
    x = "Word frequency",
    y = NULL
  ) +
  theme_minimal()


#Before stopword removal, stopwords dominate the frequency results. 
#All 15 of the most frequent words among each texts before stopword removal are stopwords. 
#The highest occurring word "the" appears over 3500 times across both texts.

#After stopword removal, words you would expect to see in documents called "Free Trade" and "Circle of Commerce" become visible.
#The highest occurring word between both documents is "trade," which appears more than 350 times across both documents.

#Yes, removing stopwords makes it much easier to say something about the subject matter of these texts. 
#Before stopword removal, it would be nearly impossible to tell what these texts are about since every single one of the top
#15 most frequent words were stopwords such as articles like "the", "of" and "and."
#However, after stopword removal words like "trade", "exchange", "money" and "merchants" become clear in the text.
#This gives us an idea that the two texts are about or related to trade, commerce, and economics in some manner.

#When stopwords are removed, the way in which the meaningful words in the text interact with one another becomes less visible.
#Stopwords help to connect phrases and meaningful words together in a natural grammatical sense.

words_after_tidy <- words %>%
  count(word, sort = TRUE) %>%
  anti_join(stop_words, by = "word")

words_after_tidy %>%
  slice_head(n=15)

#"haue", "bee" and "hath" appear frequently across both texts and are a reason why we created a custom stopwords list
#These three words are unhelpful because they are common verbs which are mostly closely related to "have", "be" and "has" in modern english.

#These common verbs do not tell us anything about the content of the documents which we suspect are about economics and trade.
#A research question in which these words may prove useful would be one that relates to how an author uses middle english verbs compared to modern english verbs across two texts,
#such as "How does the use of middle english verbs compared to modern english verbs differ between The Circle of Commerce and Free Trade.

#There is no universally correct stopword list because the list entirely depends on what your research question is. 
#In our previous example of a research question, words that we would normally deem meaningful such as "money" or "kingdom"
#would not be relevant to our chosen research question and therefore could be deemed stopwords.

#Final Reflection

#I would tell that student that I disagree because stopword removal can structurally change the texts in a significant way.
#For example, in our documents in this assignment, removing stop words removed nearly 60% of the content from both texts.
#This not only makes our meaningful words more visible, but it also disrupts the naturally grammatical structure of the texts which can make 
#the meaningful words interpretation conveluted when evaluated next to one another.
#Additionally, the list of stopwords is entirely dependent on what research question we are attempting to answer, so 
#our interpretation of the text can sometimes be inherently reliant or completely isolated from the stopwords we choose.