## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Claire Bloomfield orcid.org/0000-0002-3122-3069 and Kevin McGlynn orcid.org/0000-0002-0257-7749
## Data Source: NA
## ================================================================================

output$PI_people_directory <- DT::renderDataTable({
  pi_directory <- institution_nodes %>%
    select(id, name, institution, department)
  
  all_pubs <- institution_edges %>%
    select(from, to, publication.type, publication.date, title, co.authors, keywords)
  
  pubs_per_author <- all_pubs %>%
    gather(key, id, which(colnames(all_pubs) %in% c("from" ,"to"))) %>%
    select(id) %>%
    count(id)
  
  pi_directory <- left_join(pi_directory, pubs_per_author) %>%
    mutate(
      Degree = revalue(name, degree(institution_igraph)),
      Betweeness = revalue(name, betweenness(institution_igraph)),
      Closeness = revalue(name, round(closeness(institution_igraph), digits = 4))
    )
  
  ## Beautify 
  pi_directory %>%
    select(-id, -institution) %>%
    rename(interactions.in.database = n) %>%
    arrange(name) %>%
    rename_("Name" = "name", "Department" = "department", "Interactions in Database" = "interactions.in.database")
  
}, rownames = FALSE,escape = FALSE,
extensions = "Responsive",
options = list("language" = list("sSearch" = "Filter:")))