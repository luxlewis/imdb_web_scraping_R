# Title     : IMBD Web Scraping
# Objective :
# Created by: alexandralewis
# Created on: 11/06/2021
library('tidyverse')
library('rvest')


url <- "https://www.imdb.com/search/title/?count=100&release_date=2019,2019&title_type=feature"
webpage <- read_html(url)

films <- webpage %>%
  html_nodes('.mode-advanced')
head(films)

rank_html <- films %>%
  html_nodes('.text-primary')
rank <- as.numeric(html_text(rank_html))
head(rank)

title_html <- films %>%
  html_nodes('.lister-item-content > h3 > a')
title <- html_text(title_html)
head(title)

description_html <- films %>%
  html_nodes('.ratings-bar+ .text-muted')
description <- html_text(description_html)  %>%
    {gsub('\n','', description)}

head(description)

runtime_html <- films %>%
  html_nodes('.runtime')
runtime <- html_text(runtime_html) %>%
    {as.numeric(gsub('min', '', runtime))}
head(runtime)

genre_html <- films %>%
  html_nodes('.genre')
genre <- html_text(genre_html) %>%
  {gsub('\n', '', genre)} %>%
  str_trim()
genre <- gsub(",.*", "", genre)
genre <- as.factor(genre)
head(genre)

rating_html <- films %>%
  html_nodes('.ratings-imdb-rating > strong')
rating <- html_text(rating_html) %>%
  {as.numeric(rating)}
head(rating)

votes_html <- films %>%
  html_nodes('p.sort-num_votes-visible > span:nth-child(2)')
votes <- html_text(votes_html) %>%
{gsub(",", "", votes)} %>%
  as.numeric()
head(votes)

director_html <- films %>%
  html_nodes('div.lister-item-content > p:nth-child(5) > a:nth-child(1)')
director <- html_text(director_html) %>%
  {as.factor(director)}
head(director)

actors_html <- films %>%
  html_nodes('div.lister-item-content .ghost +a')
actors <- html_text(actors_html) %>%
  as.factor()
head(actors)

metascore_html <- films %>%
  html_nodes('.metascore')
metascore <- html_text(metascore_html)

for (i in c(6, 10, 87)) {
  a <- metascore[1:(i-1)]
  b <- metascore[i:length(metascore)]
  metascore <- append(a,list("NA"))
  metascore <- append(metascore, b)
}

metascore <- metascore %>%
  as.numeric()
head(metascore)

gross_html <- films %>%
  html_nodes('.ghost~ .text-muted+ span')

gross <- html_text(gross_html)
gross <- gsub("[^0-9]*", "", gross)

for (i in c(6, 10, 11, 12, 14, 16, 17, 25, 34, 40, 45, 46, 50, 51, 53, 56,
            57, 62, 65, 67, 69, 70, 71, 73, 74, 76, 78, 79, 80, 83, 88, 89,
            90, 93, 94, 96, 97, 98, 100)){
a <- gross[1:(i-1)]
b <- gross[i:length(gross)]
gross <- append(a,list("NA"))
gross <- append(gross, b)
}

gross <- unlist(gross) %>%
  as.numeric()
gross <- gross[-(101)]
head(gross)

# # Combining all the lists to form a dataframe
movies_df <- data.frame(Rank = rank, Title = title, Description = description,
                        Runtime = runtime, Genre = genre, Rating = rating, Metascore = metascore,
                        Votes = votes, Gross_Earning_in_Mil = gross, Director = director, Actor = actors)

str(movies_df)

ggplot(movies_df, aes(Genre, Gross_Earning_in_Mil, fill = genre)) +
  geom_col()