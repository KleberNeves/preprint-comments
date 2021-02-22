library(glue)
library(dplyr)
library(purrr)
library(jsonlite)
library(rvest)
library(lubridate)
library(ggplot2)

get_comments_from_biorxiv = function () {
  # Get number of comments
  endpoint = "https://connect.biorxiv.org/api/disqus/2020/0"
  response = fromJSON(endpoint)
  if (response$message$status == "ok") {
    n_comments = response$message$total
  } else if (response$message$status == "no posts found") {
    stop ("API call failed.")
  }
  
  print(glue("Comments to get: {n_comments}"))
  
  # Gets the content of comments, by 100s (API limitation)
  cursors = seq(0, n_comments, 100)
  responses = list()
  for (i in 1:length(cursors)) {
    cursor = cursors[i]
    endpoint = glue("https://connect.biorxiv.org/api/disqus/2020/{cursor}")
    response = fromJSON(endpoint)
    if (response$message$status == "ok") {
      print(glue("Comments {cursor} to {min(n_comments,cursor+100)}"))
      responses[[i]] = response$`bioRxiv Disqus comments`
    } else if (response$message$status == "no posts found") {
      break
    }
  }
  bind_rows(responses)
}

comments = get_comments_from_biorxiv()

start_date = "2020-1-1"
end_date = "2021-1-1"

papers_with_comments = comments %>%
  mutate(date = ymd(date)) %>%
  filter(date > ymd(start_date), date < ymd(end_date)) %>%
  group_by(doi, url) %>%
  summarise(comments = n())

write.table(papers_with_comments, "preprints with comments (2020, biorxiv).csv", row.names = F, sep = ";")

write.table(comments, "comments (2020, biorxiv).csv", row.names = F, sep = ";")
