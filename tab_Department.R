## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Claire Bloomfield orcid.org/0000-0002-3122-3069 and Kevin McGlynn orcid.org/0000-0002-0257-7749
## Data Source: NA
## ================================================================================


## ================= selected department details  ===============================
## ==============================================================================

output$select_department_UI <- renderUI(
  selectInput(
    "selected_department",
    label = "Select Department",
    choices = sort(unique(institution_nodes$department)),
    width = "100%"
  )
)


output$department_app_title <-
  renderUI(h1(paste(input$selected_department, "Profile")))

output$department_app_description <- renderUI(wellPanel(
  p(
    paste(
      "This lorem ipsum content is about the ",
      input$selected_department,
      "department and will describe what the department does and how amazing they are - it might also include links."
    )
  ),
  p(
    "The content may also include hyperlinks, and could be pulled into the app via an external source (such that it may be updateable) - however, such functionality would be beyond a Live Data Case Study."
  )
))

output$department_app_collapsile_info <- renderUI({
  if(is.null(input$selected_department)){
    return()
  }
  
  
  department_graph <- induced_subgraph(institution_igraph,
                                       institution_nodes[institution_nodes$department == input$selected_department, "name"])
  
  fluidRow(column(p(paste0(
    "Number of PIs: ",
    vcount(department_graph)
  )),
  
  
  p(
    paste0("Highest Degree of Seperation: ",
           {
             shrt_paths <- shortest.paths(department_graph, 2)
             max(shrt_paths[shrt_paths < Inf])
           })
  ),
  p(paste0(
    "Average Path Length: ",
    round(mean_distance(department_graph), digits = 2)
  )),
  width = 6),
  column(p(
    paste0("Average Degree: ", round(mean(
      degree(department_graph())
    ))), digits = 2
  ),
  p(
    paste0("Number of Collaborations: ", ecount(department_graph))
  ),
  width = 6))
})


## ================= People Directory DT/UI =====================================
## ==============================================================================

output$department_people_directory_DT <- DT::renderDataTable({
  # institution_nodes[institution_nodes$department == input$selected_department, c("name", "institution", "department")]
  # 
  institution_nodes %>%
    filter(department == input$selected_department) %>%
    select(name, institution, department) %>%
    arrange(name)
  
}, rownames = FALSE,
# filter = FALSE,
escape = FALSE,
extensions = "Responsive",
options = list("language" = list("sSearch" = "Filter:")))

output$department_people_directory_UI <- renderUI(fluidPage(
  paste("These are the people in the ", input$selected_department),
  DT::dataTableOutput("department_people_directory_DT")
))

## ================= Department Network DT/UI =====================================
## ==============================================================================

department_graph <-
  eventReactive(
    c(
      input$selected_department,
      input$people_or_departments,
      input$vertex_degree
    ),
    switch(
      input$people_or_departments,
      "within department" = {
        departmental_nodes <-
          institution_nodes[institution_nodes$department == input$selected_department, "name"]
        
        induced_subgraph(institution_igraph, departmental_nodes)
        
      },
      
      "within whole network" = {
        graph.union(make_ego_graph(institution_igraph,
                                   order = 2,
                                   nodes = institution_nodes[institution_nodes$department == input$selected_department, "name"])) %>% igraph_deduplicate_vertex_attr()
      },
      
      ## Note that this does not work, hence unavailable in options
      "unavailable_department_level_interactions" = {
        if (is.null(input$vertex_degree)) {
          return()
        }
        department_ego_networks <-
          graph.union(
            make_ego_graph(
              institution_igraph,
              order = input$vertex_degree,
              nodes = institution_nodes[institution_nodes$department ==
                                          input$selected_department, "name"]
            )
          )
        
        
        igraph_deduplicate_vertex_attr(department_ego_networks)
        
      }
    )
  )

output$department_network <- renderVisNetwork({
  if (input$people_or_departments == "within whole network" &
      is.null(input$vertex_degree)) {
    return()
  }
  department_graph <- as.undirected(department_graph())
  
  
  switch(
    input$people_or_departments,
    "within department" = {
      visIgraph(department_graph,
                idToLabel = F)
    },
    "within whole network" = {
      visIgraph(department_graph,
                idToLabel = F,
                layout = "layout_with_lgl")
    }
  ) %>%
    visOptions(highlightNearest = TRUE,
               nodesIdSelection = list(enabled = TRUE)) %>%
    visLayout(hierarchical = FALSE) %>%
    visEvents(selectNode = "function(nodes) {
              Shiny.onInputChange('current_node_id', nodes);
              ;}")
  })

output$department_network_edge_degree_UI <- renderUI({
  if (input$people_or_departments == "within whole network") {
    wellPanel(
      sliderInput(
        "vertex_degree",
        label = "Vertex Degree",
        min = 1,
        max = 5,
        step = 1,
        value = 1
      )
    )
  }
})


## ================= Legend =====================================
## ==============================================================================

output$department_highchart_node_legened <- renderHighchart(
  highchart_legend(
    legend_names = department_colours$department,
    legend_colours = department_colours$colours
  )
)



## =========================== Generate Graph ====================================
## ==============================================================================




output$department_displayed_network <- renderVisNetwork({
  department_graph <- as.undirected(department_graph())
  
  visIgraph(department_graph,
            idToLabel = F,
            layout = "layout_nicely") %>%
    visOptions(highlightNearest = TRUE,
               nodesIdSelection = list(enabled = TRUE)) %>%
    visLayout(hierarchical = FALSE) %>%
    visInteraction(dragNodes = FALSE) %>%
    visEvents(selectNode = "function(nodes) {
              Shiny.onInputChange('current_node_id', nodes);
              ;}")
  })
output$department_displayed_network_properties <- renderUI({
  wellPanel(p(paste0(
    "Average path length: ", round(average.path.length(department_graph()), 2)
  )),
  p(paste0(
    "Number of nodes: ", vcount(department_graph())
  )))
})

observeEvent(
  input$department_refocus_network,
  visNetworkProxy("department_displayed_network") %>%
    visFit(nodes = NULL, animation = list(duration = 500))
)

output$department_selected_node_sidePanel <- renderUI({
  if (is.null(input$department_displayed_network_selected)) {
    return()
  }
  
  if (input$department_displayed_network_selected == "") {
    return()
  }
  onClickInputCheck(
    never_Clicked = return(),
    show_Details = {
      wellPanel(
        p(
          paste0(
            "Selected Principal Investigator: ",
            input$department_displayed_network_selected
          )
        ),
        
        p(paste0("Department: ", institution_nodes[institution_nodes$name == input$department_displayed_network_selected, "department"])),
        
        
        p(paste0(
          "Vertex Degree: ", as.numeric(degree(department_graph())[which(V(department_graph())$name == input$department_displayed_network_selected)])
        )),
        
        p(paste0(
          "Vertex Betweeness: ", round(as.numeric(betweenness(
            department_graph()
          )[which(V(department_graph())$name == input$department_displayed_network_selected)]), digits = 4)
        )),
        
        p(paste0(
          "Vertex Closeness: ", round(as.numeric(closeness(
            department_graph()
          )[which(V(department_graph())$name == input$department_displayed_network_selected)]), digits = 4)
        )),
        
        actionButton("scroll_down_department", "Scroll down for details", width = "100%")
      )
    },
    destructive_Change = return()
  )
})

observeEvent(input$scroll_down_department, {
  session$sendCustomMessage(type = "scrollDown", 1)
})


department_within_department_table <- reactive({
  if (is.null(input$department_displayed_network_selected)) {
    return()
  }
  
  if (input$department_displayed_network_selected == "") {
    return()
  }
  
  department_members <- V(department_graph())$id
  
  subsetted_edges <-
    filter(institution_edges,
           from %in% department_members &
             to %in% department_members)
  
  selected_id <-
    institution_nodes[institution_nodes$name == input$department_displayed_network_selected, "id"]
  
  
  subsetted_edges <-
    filter(subsetted_edges, from == selected_id |
             to == selected_id)
  
  
  subsetted_edges$from <-
    mapvalues(
      subsetted_edges$from,
      from = institution_nodes$id,
      to = institution_nodes$name,
      warn_missing = FALSE
    )
  subsetted_edges$to <-
    mapvalues(
      subsetted_edges$to,
      from = institution_nodes$id,
      to = institution_nodes$name,
      warn_missing = FALSE
    )
  select(
    subsetted_edges,
    from,
    to,
    collaborations,
    title,
    publication.name,
    publication.date
  )
})

department_within_whole_table <- reactive({
  if (input$department_displayed_network_selected == "") {
    return()
  }
  
  department_members <- V(department_graph())$id
  
  subsetted_edges <-
    filter(institution_edges,
           from %in% department_members &
             to %in% department_members)
  
  
  selected_id <-
    institution_nodes[institution_nodes$name == input$department_displayed_network_selected, "id"]
  
  
  subsetted_edges <-
    filter(subsetted_edges, from == selected_id |
             to == selected_id)
  
  subsetted_edges$from <-
    mapvalues(
      subsetted_edges$from,
      from = institution_nodes$id,
      to = institution_nodes$name,
      warn_missing = FALSE
    )
  subsetted_edges$to <-
    mapvalues(
      subsetted_edges$to,
      from = institution_nodes$id,
      to = institution_nodes$name,
      warn_missing = FALSE
    )
  select(
    subsetted_edges,
    from,
    to,
    collaborations,
    title,
    publication.name,
    publication.date
  )
})

output$department_selected_node_table <- DT::renderDataTable({
  if (is.null(input$department_displayed_network_selected)) {
    return()
  }
  
  selected_node_table <- onClickInputCheck(show_Details = {
    switch(
      input$people_or_departments,
      "within department" = {
        data_to_show <- department_within_department_table()
        
        value_for_col_1 <-
          input$department_displayed_network_selected
        
        rows_to_change <-
          which(data_to_show[, 1] != value_for_col_1)
        
        lapply(rows_to_change, function(x) {
          first_col_value <- which(data_to_show[x, c(1, 2)] == value_for_col_1)
          colorder <-
            c(first_col_value, setdiff(1:ncol(data_to_show), first_col_value))
          # print(colorder)
          data_to_show[x,] <<- data_to_show[x, colorder]
        })
        
        data_to_show %>%
          rename_(
            "Selected PI" = "from",
            "Collaborator" = "to",
            "# of collaborations" = "collaborations",
            "Title" = "title",
            "Journal Name" = "publication.name",
            "Publication date" = "publication.date"
          )
      },
      "within whole network" = {
        data_to_show <- department_within_whole_table()
        
        value_for_col_1 <-
          input$department_displayed_network_selected
        
        rows_to_change <-
          which(data_to_show[, 1] != value_for_col_1)
        
        lapply(rows_to_change, function(x) {
          first_col_value <- which(data_to_show[x, c(1, 2)] == value_for_col_1)
          colorder <-
            c(first_col_value, setdiff(1:ncol(data_to_show), first_col_value))
          # print(colorder)
          data_to_show[x,] <<- data_to_show[x, colorder]
        })
        
        data_to_show %>%
          rename_(
            "Selected PI" = "from",
            "Collaborator" = "to",
            "# of collaborations" = "collaborations",
            "Title" = "title",
            "Journal Name" = "publication.name",
            "Publication date" = "publication.date"
          )
      }
    )
  })
  
  selected_node_table
})

output$department_selected_node_table_UI <- renderUI({
  if (input$department_displayed_network_selected == "") {
    wellPanel("Select a node for more details")
  } else {
    onClickInputCheck(
      never_Clicked = {
        wellPanel("Select a node for more details")
      },
      show_Details = {
        print("in showdetails")
        wellPanel(DT::dataTableOutput("department_selected_node_table"))
        # print(institution_nodes[institution_nodes$name == input$department_displayed_network_selected, "id"])
      },
      destructive_Change = wellPanel("Select a node for more details")
    )
  }
  
})
