library(tidyverse)


# Three of the Stack Overflow survey datasets are questions, question_tags, 
#and tags:

# questions: an ID and the score, or how many times the question has been 
# upvoted; the data only includes R-based questions

# question_tags: a tag ID for each question and the question's id

# tags: a tag id and the tag's name, which can be used to identify the subject 
# of each question, such as ggplot2 or dplyr

# Import the datasets
questions <- readRDS("Data/questions.rds")
question_tags <- readRDS("Data/question_tags.rds")
tags <- readRDS("Data/tags.rds")

# Join together questions and question_tags using the id and question_id columns

questions_with_tags <- questions %>%
  left_join(question_tags, by = c("id" = "question_id")) %>% 
  # Join in the tags table
  left_join(tags, by = c("tag_id" = "id")) %>% 
  # Replace the NAs in the tag_name column
  replace_na(list(tag_name = "only-r"))

# 1. Aggregate by the tag_name.
# 2. Summarize to get the mean score for each question, score, as well as the 
# total number of questions, num_questions.
# 3.Arrange num_questions in descending order to sort the answers by the most 
#asked questions.

questions_with_tags %>% 
  # Group by tag_name
  group_by(tag_name) %>%
  # Get mean score and num_questions
  summarize(score = mean(score),
            num_questions = n()) %>%
  # Sort num_questions in descending order
  arrange(desc(num_questions))

# Using a join, filter for tags that are never on an R question
tags %>%
  anti_join(question_tags, by = c("id" = "tag_id"))


## Finding gaps between questions and answers:

answers <- readRDS("Data/answers.rds")

# 1. Use an inner join to combine the questions and answers tables using the 
# suffixes "_question" and "_answer", respectively.
# 2. Subtract creation_date_question from creation_date_answer within the 
# as.integer() function to create the gap column.

questions %>%
  # Inner join questions and answers with proper suffixes
  inner_join(answers, by = c("id" = "question_id"),
             suffix = c("_question", "_answer")) %>%
  # Subtract creation_date_question from creation_date_answer to create gap
  mutate(gap = as.integer(creation_date_question - creation_date_answer))


## Joining question and answer counts:
# 1. Count and sort the question_id column in the answers table to create the 
# answer_counts table.
# 2. Join the questions table with the answer_counts table and include all 
# observations from the questions table.
# 3. Replace the NA values in the n column with 0s.

# Count and sort the question id column in the answers table
answer_counts <- answers %>%
  count(question_id, sort = TRUE)

# Combine the answer_counts and questions tables
question_answer_counts <- questions %>%
  left_join(answer_counts, by = c("id" = "question_id")) %>%
  # Replace the NAs in the n column
  replace_na(list(n = 0))


## Joining questions, answers, and tags:
# 1. Combine the question_tags table with question_answer_counts using 
# an inner_join.
# 2. Now, use another inner_join to add the tags table.

tagged_answers <- question_answer_counts %>%
  # Join the question_tags tables
  inner_join(question_tags, by = c("id" = "question_id")) %>%
  # Join the tags table
  inner_join(tags, by = c("tag_id" = "id"))


## Average answers by question:
# 1. Aggregate the tagged_answers table by tag_name.
# 2. Summarize tagged_answers to get the count of questions and the 
# average_answers.
# 3. Sort the resulting questions column in descending order.

tagged_answers %>%
  # Aggregate by tag_name
  group_by(tag_name)  %>%
  # Summarize questions and average_answers
  summarize(questions = n(),
            average_answers = mean(n)) %>%
  # Sort the questions in descending order
  arrange(desc(questions))


# Joining questions and answers with tags:
# 1. Use two inner joins to combine the question_tags and tags tables with 
# the questions table.
# 2. Now, use two inner joins to combine the question_tags and tags tables 
# with the answers table.

# Inner join the question_tags and tags tables with the questions table
questions_with_tags <- questions %>%
  inner_join(question_tags, by = c("id" = "question_id")) %>%
  inner_join(tags, by = c("tag_id" = "id"))

# Inner join the question_tags and tags tables with the answers table
answers_with_tags <- answers %>%
  inner_join(question_tags, by = "question_id") %>%
  inner_join(tags, by = c("tag_id" = "id"))


## Binding and counting posts with tags:
# 1. Combine the questions_with_tags and answers_with_tags tables into 
# posts_with_tags.
# 2. Add a year column to the posts_with_tags table, then count posts by type, 
# year, and tag_name.

# Combine the two tables into posts_with_tags
posts_with_tags <- bind_rows(questions_with_tags %>% 
                               mutate(type = "question"),
                             answers_with_tags %>% mutate(type = "answer"))

# Add a year column, then count by type, year, and tag_name
by_type_year_tag <- posts_with_tags %>%
  mutate(year = year(creation_date)) %>%
  count(type, year, tag_name)


## Visualizing questions and answers in tags:
# 1. Filter the by_type_year_tag table for the dplyr and ggplot2 tags.
# 2. Create a line plot with that filtered table that plots the 
# frequency (n) over time, colored by question/answer and faceted by tag.

# Filter for the dplyr and ggplot2 tag names 
by_type_year_tag_filtered <- by_type_year_tag %>%
  filter(tag_name == "dplyr" | tag_name == "ggplot2")

# Create a line plot faceted by the tag name 
ggplot(by_type_year_tag_filtered, aes(year, n, color = type)) +
  geom_line() +
  facet_wrap(~ tag_name) + 
  labs(x = "Year",
       y = "Frequency",
       color = "Type:",
       title = "Asked and answered - dplyr vs ggplot2") +
  scale_x_continuous(breaks=2009:2019) +
  theme(plot.title = element_text(hjust = 0.5))



