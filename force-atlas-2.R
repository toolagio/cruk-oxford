

departments_g <- contract_instition_network(institution_igraph)

invisible(network <- visIgraph(
  # as.undirected(departments_g),
  as.undirected(institution_igraph),
  layout = "layout.forceatlas2",
  directed = FALSE,
  k = 200, # repulsion
  delta = 5, # attraction
  ks = 10, # speed constant
  ksmax = 20, # limits speed
  # autostab parameters can't be found
  gravity = 0.1,
  iterations = 400,
  nohubs = FALSE,
  randomSeed = 8
  # linlog = TRUE
))

network %>%
  visEdges(smooth = list("enabled" = TRUE,
                         "type" = "curvedCCW")) %>%
  visPhysics(solver = "forceAtlas2Based", 
             forceAtlas2Based = list(gravitationalConstant = -500)) %>%
  visEdges(smooth = list("enabled" = TRUE,
                         "type" = "curvedCCW"))

help("layout.forceatlas2")


## Direct?

visIgraph(as.undirected(institution_igraph), randomSeed = 1) %>%
  visPhysics(solver = "forceAtlas2Based",
             forceAtlas2Based = list(gravitationalConstant = -30,
                                     springConstant = 10,
                                     damping = 0.8,
                                     avoidOverlap = 0.2)) %>%
  visEdges(smooth = list("enabled" = TRUE,
                         "type" = "curvedCCW")) %>% visFit()

## shinyApp


shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
    sliderInput("forceAtlas_gravConstant", "Gravitational Constant",
                min = -100, max = 0, value = -30),
    sliderInput("forceAtlas_centralGravity", "Central Gravity",
                min = 0, max = 1, value = 0.01),
    sliderInput("forceAtlas_sprintContant", "Spring Constant",
                min = 0, max = 20, value = 1),
    sliderInput("forceAtlas_damping", "Damping",
                min = 0, max = 1, value = 0.3),
    sliderInput("forceAtlas_avoidOverlap", "Overlap",
                min = 0, max = 1, value = 0.2),
    sliderInput("maxVelocity", "MAx Velocity",
                min = 1, max = 100, value = 50),
    sliderInput("minVelocity", "Min Velocity",
                min = 0, max = 2, value = 0.1),
    actionButton("refocus", "refocus")),
    mainPanel(visNetworkOutput("visN")))
  ),
  server = function(input, output){
    
    output$visN <- renderVisNetwork({
      # visIgraph(as.undirected(institution_igraph), randomSeed = 1) %>%
      institution_visN <- toVisNetworkData(institution_igraph)
      visNetwork(nodes = institution_visN$nodes, edges = institution_visN$edges, improvedLayout = TRUE) %>%
        visNodes(size = 8) %>%
        visPhysics(solver = "forceAtlas2Based",
                   repulsion = list(nodeDistance = 200,
                                    centralGravity = 0.2,
                                    springLength = 200),
                   maxVelocity = input$maxVelocity, 
                   minVelocity = input$minVelocity,
                   forceAtlas2Based = list(gravitationalConstant = input$forceAtlas_gravConstant,
                                           springConstant = input$forceAtlas_sprintContant,
                                           centralGravity = input$forceAtlas_centralGravity,
                                           damping = input$forceAtlas_damping,
                                           avoidOverlap = input$forceAtlas_avoidOverlap),
                   stabilization = list(enabled = TRUE,
                                        iterations = 10,
                                        updateInterval = 5)
                   ) %>%
        # visEdges(smooth = list("enabled" = TRUE,
        #                        "type" = "curvedCCW")) %>%
        visInteraction(dragNodes = FALSE)
    })
    
    
    observeEvent(input$refocus, {
      
      visNetworkProxy("visN") %>%
        visFit(nodes = NULL, animation = list(duration = 500))
    })
    
  }
)


institution_visN <- toVisNetworkData(institution_igraph)
visNetwork(nodes = institution_visN$nodes, edges = institution_visN$edges)


