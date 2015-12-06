#' tourbrush
#'
#' @param df a data frame
#' @param aps angles per second
#' @param palette an RColorBrewer palette
#' @export
#' @examples \dontrun{
#' library(mvtnorm)
#' s <- matrix(c(1, .5, .9, .5, 1, .5, .9, .5, 1), ncol = 3)
#' m <- rmvnorm(500, sigma = s)
#' # take 1D slice of the 3D density
#' d <- setNames(data.frame(m), c("x", "y", "z"))
#' d$density <- dmvnorm(m)
#' tourbrush(d)
#'
#' vars <- c("pitch_type", "start_speed", "spin_dir", "break_y",
#'   "break_angle", "break_length")
#' tourbrush(yu[vars])
#'
#'
#' }
tourbrush <- function(df, aps = 2, palette = "Dark2") {
  # m is columns we can tour, d will keep track of columns we can use as labels
  is_num <- sapply(df, is.numeric)
  m <- df[is_num]
  d <- if (all(is_num)) {
    data.frame(none = rep("black", nrow(df)), stringsAsFactors = FALSE)
  } else {
    df[!is_num]
  }
  # find universe of possible colors (needed to use identity scale in ggvis)
  # firstly, set brush colors
  cols <- c("red", "blue", "yellow", "green")
  # max number of labels/colors
  n <- max(unlist(lapply(d, function(x) length(unique(x)))))
  pal <- RColorBrewer::brewer.pal(n, palette)
  colz <- c(pal, "black", cols)
  # scales::col_factor 'spans' the palette which is not really what we want
  scaleDat <- function(x) pal[as.integer(as.factor(x))]

  # mechanism for maintaining brush state
  initSelection <- function(labels = NULL) {
    color <- if (is.null(labels)) rep("black", nrow(m)) else labels
    # x is a logical vector, y is a color
    function(x, y) {
      if (missing(x) || missing(y)) return(color)
      color[x] <<- y
      color
    }
  }

  selection <- if (identical("none", names(d))) {
    initSelection()
  } else {
    initSelection(scaleDat(d[, 1]))
  }


  ui <- bootstrapPage(
    # controls
    sidebarPanel(
      plotOutput("scatter1", brush = brushOpts("brush", direction = "xy")),
      br(),
      div(style = "display:inline-block", checkboxInput("play", "Play", FALSE)),
      div(style = "display:inline-block", checkboxInput("persistent", "Persistent Brush", FALSE)),
      br(),
      selectInput("color", "Paint brush color", choices = cols),
      selectizeInput("tourVars", "Tour variables", names(m), names(m), multiple = TRUE),
      div(style = "display:inline-block", selectInput("x", "Choose an x", names(m), names(m)[1], width = 150)),
      div(style = "display:inline-block", selectInput("y", "Choose a y", names(m), names(m)[2], width = 150)),
      div(style = "display:inline-block", selectizeInput("label", "Choose a label", names(d), width = 150, multiple = FALSE)),
      width = 4
    ),
    # views
    mainPanel(
      ggvisOutput("tour")
    )
  )

  server <- function(input, output, session) {

    scaleLabel <- reactive({
      lab <- d[, input$label]
      if (input$label == "none") lab else scaleDat(lab)
    })

    updateSelection <- reactive({
      br <- input$brush
      m2 <- m[c(input$x, input$y)]
      if (!is.null(br)) {
        # I dare you to insist on a persistent brush by default
        if (!input$persistent) selection <- initSelection(scaleLabel())
        x <- br$xmin <= m2[, 1] & m2[, 1] <= br$xmax
        y <- br$ymin <= m2[, 2] & m2[, 2] <= br$ymax
        selection(x & y, input$color)
      } else {
        selection()
      }
    })

    initTour <- reactive({
      mat <- rescale(as.matrix(m[input$tourVars]))
      tour <- new_tour(mat, grand_tour(), NULL)
      list(
        mat = mat,
        tour = tour,
        step = tour(0)
      )
    })

    # standing on the shoulders of giants, as they say
    # https://github.com/rstudio/ggvis/blob/master/demo/tourr.r
    iterTour <- reactive({
      tr <- initTour()
      if (input$play) invalidateLater(500 / 30, NULL)
      tr$step <- tr$tour(aps / 30) # you always want 30 frames/second, right?
      list(
        mat = tr$mat,
        tour = tr$tour,
        step = tr$step
      )
    })

    tourDat <- reactive({
      tr <- iterTour()
      tDat <- data.frame(center(tr$mat %*% tr$step$proj))
      tDat$color <- updateSelection()
      setNames(tDat, c("x", "y", "color"))
    })

    output$scatter1 <- renderPlot({
      s <- m[c(input$x, input$y)]
      labs <- d[, input$label]
      s[input$label] <- labs
      colorz <- updateSelection()
      s[colorz %in% cols, input$label] <- "selection"
      v <- setNames(unique(colorz), unique(s[, input$label]))
      ggplot(s, aes_string(input$x, input$y, color = input$label)) +
        geom_point(size = 5) + scale_color_manual(values = v) + theme_bw() +
        theme(legend.position = if (input$label == "none") "none" else "bottom")
    }, width = 400)

    tourDat %>%
      ggvis(~x, ~y, fill = ~color) %>%
      layer_points() %>%
      scale_ordinal("fill", domain = colz, range = colz) %>%
      scale_numeric("x", domain = c(-1, 1), label = "") %>%
      scale_numeric("y", domain = c(-1, 1), label = "") %>%
      hide_legend("fill") %>%
      set_options(width = 400, height = 400, keep_aspect = TRUE, duration = 0) %>%
      bind_shiny("tour")
  }
  shinyApp(ui, server)
}
