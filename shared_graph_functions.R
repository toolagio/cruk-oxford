## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Claire Bloomfield orcid.org/0000-0002-3122-3069 and Kevin McGlynn orcid.org/0000-0002-0257-7749
## Data Source: NA
## ================================================================================


## ============ Contract institute network ==========================================
## ==============================================================================

contract_instition_network <- function(uncontracted_graph = NA){
  contracted_graph <- contract(uncontracted_graph, mapping = as.numeric(
    mapvalues(
      V(uncontracted_graph)$color,
      from = department_colours$colours,
      to = 1:nrow(department_colours),
      warn_missing = F
    )
  ))
  
  contracted_graph <- simplify(delete.vertices(contracted_graph, degree(contracted_graph)==0))
  
  
  V(contracted_graph)$name <- unique(V(uncontracted_graph)$department)
  V(contracted_graph)$title <- V(contracted_graph)$name
  V(contracted_graph)$color <-
    mapvalues(unlist(V(contracted_graph)$name),
              from = department_colours$department,
              to = department_colours$colours,
              warn_missing = F)
  
  contracted_graph <-
    simplify(as.undirected(contracted_graph))
  contracted_graph
  
}

## ============ Remove duplicate vertex attributes ==========================================
## ==============================================================================

igraph_deduplicate_vertex_attr <- function(igraph_object){
  igraph_object <- igraph_object
  ## Get color_* attributes, paste together as a vector and use gsub to remove NAs
  
  colours <- trimws(gsub("NA","",do.call("paste", vertex.attributes(igraph_object)[names(vertex.attributes(igraph_object))[grepl("color", names(vertex.attributes(igraph_object)))]])
  ))
  
  V(igraph_object)$color <- unlist(lapply(colours, function(x)unique(unlist(strsplit(x," "))[1])))
  
  titles <- trimws(gsub("NA","",do.call("paste", vertex.attributes(igraph_object)[names(vertex.attributes(igraph_object))[grepl("title", names(vertex.attributes(igraph_object)))]])
  ))
  
  V(igraph_object)$title <-  unlist(lapply(titles, function(x)unique(unlist(strsplit(x," "))[1])))
  
  ids <- trimws(gsub("NA","",do.call("paste", vertex.attributes(igraph_object)[names(vertex.attributes(igraph_object))[grepl("id", names(vertex.attributes(igraph_object)))]])
  ))
  
  V(igraph_object)$id <- unlist(lapply(ids, function(x)unique(unlist(strsplit(x," "))[1])))
  
  departments <- trimws(gsub("NA","",do.call("paste", vertex.attributes(igraph_object)[names(vertex.attributes(igraph_object))[grepl("department", names(vertex.attributes(igraph_object)))]])
  ))
  
  V(igraph_object)$department <- unlist(lapply(departments, function(x)unique(unlist(strsplit(x," "))[1])))
  
  igraph_object
}