library(tidyverse)

# Disqus API key
key <- "EeRboWOJDLHSHIoJQ70GWQzpzLpAiZdC26DpFBaFzgKAfvfclY2WU1AO2FHrszn3"

getThreadsFromUrl <- function(url) {
  request <- httr::GET(url)
  data <- httr::content(request)
  return(data)
}

# Extraction of relevant information
extractCommentCounts <- function(item){
  if(length(item)>1){
    tibble(
      doi = str_replace(str_extract(item$link, "10.1101.*"), "v.*", ""),
      version = str_extract(str_extract(item$link, "10.1101.*"), "v.*"),
      url = paste0(item$link, "?versioned=true"),
      disqus_thread_id = item$id,
      comments_count = item$posts,
      year = str_extract(item$link, "10[.]1101/[0-9]{4}")
    )
  }
}


has_next <- T
medrxiv_data <- list()
cursor <- NULL
n <- 0
calls_made <- 0

while(has_next == T) {
  if(length(cursor)) {
    url <- paste0("https://disqus.com/api/3.0/forums/listThreads.json?forum=medrxiv&limit=100&api_key=", key, "&cursor=", cursor)
  } else {
    url <- paste0("https://disqus.com/api/3.0/forums/listThreads.json?forum=medrxiv&limit=100&api_key=", key)
  }
  
  d <- getThreadsFromUrl(url)
  medrxiv_data <- c(medrxiv_data, d$response)
  
  if(d$cursor$hasNext == T) {
    cursor <- d$cursor$`next`
  } else {
    has_next <- F
  }
  n <- n + 100
  print (n)
}

save(medrxiv_data, file = "medrxiv_data.RData")

medrxiv_comment_counts <- map_dfr(medrxiv_data, extractCommentCounts) %>%
  distinct() %>%
  group_by(doi) %>%
  mutate(comments = sum(comments_count)) %>%
  ungroup() %>%
  distinct() %>%
  mutate(Year = as.numeric(str_extract(str_extract(doi, "10[.]1101/[0-9]{4}"),"[0-9]{4}$")))

papers_with_comments = medrxiv_comment_counts %>% filter(comments_count > 0, Year == 2020)

write.table(papers_with_comments, "preprints with comments (2020, medrxiv).csv", row.names = F, sep = ";")
