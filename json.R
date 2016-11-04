library(jsonlite)
library(tidyjson)
# https://cran.r-project.org/web/packages/tidyjson/vignettes/introduction-to-tidyjson.html

edges_json <- fromJSON("data/ox-ox-edges.json")

readLines("data/ox-ox-edges.json") %>% head()


minimal_edges <- edges_json %>%
  select(Source, Target, Collaborations)

import_edge_json <- readLines("data/ox-ox-edges.json", warn = F) %>%
  gather_array() %>%
  spread_values(Source = jstring("Source")) %>%
  spread_values(Target = jstring("Target")) %>%
  spread_values(collaborations = jstring("Collaborations")) %>%
  enter_object("authorships") %>% 
  gather_array() %>%
  spread_values(title = jstring("Title")) %>%
  spread_values(coauthors = jstring("Co-authors")) %>%
  spread_values(publication.name = jstring("Publication Name")) %>%
  spread_values(weights = jstring("Weights")) %>%
  spread_values(keywords = jstring("Keywords")) %>%
  spread_values(publication.type = jstring("Publication Type")) %>%
  spread_values(publication.date = jstring("Publication Date")) %>%
  select(Source, Target, title, coauthors, publication.name, weights, keywords, publication.type, publication.date)

## Test works!

head(minimal_edges)

test_json %>%
  filter(Source == "clne0024" & Target == "univ1102")

## Remove duplicates

test_json %>%
  duplicated



