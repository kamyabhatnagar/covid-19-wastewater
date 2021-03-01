ww_visualization <-
  function(xaxis,
           yaxis,
           yaxis2 = NULL,
           yaxis_button = FALSE,
           yaxis2_button = FALSE,
           y_button_name = "",
           y2_button_name = "",
           titles,
           width = 800,
           height = 500,
           data) {
    # ---------- PRESETS ----------
    tickvals <- floor_date(as_date(data$date), "month")
    trace_presets <- list(
      avg_data = list(
        type = "scatter",
        mode = "line",
        showlegend = TRUE,
        line = list(
          width = 4
        )
      ),
      observed_data = list(
        type = "bar",
        showlegend = TRUE,
        line = NULL,
        mode = NULL
      ),
      signal_data = list(
        type = "scatter",
        showlegend = TRUE
      )
    )
    attr(trace_presets$avg_data, "class") <-
      "avg_data"
    attr(trace_presets$observed_data, "class") <-
      "observed_data"
    attr(trace_presets$signal_data, "class") <-
      "signal_data"
    
    # initiate plotly object
    library(plotly)
    p <- plot_ly()
    
    # base parameters for buttons
    base_params <- 'list(
  list(
  active = 0,
  x = -0.2,
  type= "dropdown",
  direction = "down",
  xanchor = "center",
  yanchor = "top",
  pad = list("r"= 0, "t"= -25, "b" = 0),
  buttons = list(
  %s)
  )
  )'
    base_params_y2 <- 'list(
  list(
  active = 0,
  x = 1.20,
  y = 0.86,
  type= "dropdown",
  direction = "down",
  xanchor = "center",
  yanchor = "top",
  pad = list("r"= 5, "t"= 0, "b" = 0),
  buttons = list(
  %s)
  )
  )'
    updated <- NULL
    menu <- ""
    updated_y2 <- NULL
    menu_y2 <- ""
    
    # Create traces 
    for (i in 1:length(yaxis)) {
      var_to_map <- yaxis[[i]]
      curr_temp <- trace_presets[[var_to_map$type]]
      if (!is_null(var_to_map$color)) {
        curr_temp <-
          change_color(template = trace_presets[[var_to_map$type]],
                       color = var_to_map$color)
      }
      if (isTRUE(yaxis_button)){
        vis_logical <- c(rep(NA, length(yaxis)), rep(T, length(yaxis2)))
        vis_logical[i] <- T
        vis_logical[is.na(vis_logical)] <- F
        vis_logical <- paste0("c(",stringr::str_flatten(vis_logical, ","),")")
        menu_item <- sprintf('
      list(
        label = "%s",
        method = "update",
        args = list(list(visible = %s),
                    list(title = "%s")))',
                             yaxis[[i]][["short_name"]],
                             vis_logical,
                             titles[["title"]])
        
        if (i < length(yaxis)){
          menu <- stringr::str_glue(stringr::str_glue(menu,menu_item),",")
        } else {
          menu <- stringr::str_glue(menu,menu_item)
        }
        if(i == 1){
          p <-
            do.call(add_trace, c(
              list(p = p, name = var_to_map$name),
              curr_temp,
              list(x = data[, xaxis],
                   y = data[, var_to_map$y_column]),
              hovertemplate = paste('%{x|%b %d, %Y}:',
                                    '%{y}')
            ))
        }else{
          p <-
            do.call(add_trace, c(
              list(p = p, name = var_to_map$name),
              curr_temp,
              list(x = data[, xaxis],
                   y = data[, var_to_map$y_column]),
              hovertemplate = paste('%{x|%b %d, %Y}:',
                                    '%{y}'),
              visible = FALSE
            ))
        }
      }
      else{
        p <-
          do.call(add_trace, c(
            list(p = p, name = var_to_map$name),
            curr_temp,
            list(x = data[, xaxis],
                 y = data[, var_to_map$y_column]),
            opacity = var_to_map$opacity,
            hovertemplate = paste('%{x|%b %d, %Y}:',
                                  '%{y}')
          ))
      }
    }
    
    updated <- sprintf(base_params, menu)
    updated <- eval(parse(text = updated))
    
    # Create traces if second y-axis is specified
    if(!is.null(yaxis2)){
      for (i in 1:length(yaxis2)) {
        var_to_map <- yaxis2[[i]]
        curr_temp <- trace_presets[[var_to_map$type]]
        if (!is_null(var_to_map$color)) {
          curr_temp <-
            change_color(template = trace_presets[[var_to_map$type]],
                         color = var_to_map$color)
        }
        
        if (isTRUE(yaxis2_button)){
          vis_logical <- c(rep(T, length(yaxis)))
          vis_logical2 <- c(rep(NA, length(yaxis2)))
          vis_logical2[i] <- T
          vis_logical2[is.na(vis_logical2)] <- F
          vis_logical2 <- c(vis_logical, vis_logical2)
          vis_logical2 <-
            paste0("c(",stringr::str_flatten(vis_logical2, ","),")")
          menu_item2 <- sprintf('
      list(
        label = "%s",
        method = "update",
        args = list(list(visible = %s),
                    list(title = "%s",
                          yaxis2.range = c(0,"%s"))))',
                                yaxis2[[i]][["short_name"]],
                                vis_logical2,
                                titles[["title"]],
                                2*max(data[,yaxis2[[i]][["y_column"]]],
                                      na.rm = TRUE))
          
          if (i < length(yaxis2)){
            menu_y2 <-
              stringr::str_glue(stringr::str_glue(menu_y2,menu_item2),",")
          } else {
            menu_y2 <-
              stringr::str_glue(menu_y2,menu_item2)
          }
          if(i == 1){
            p <-
              do.call(add_trace, c(
                list(p = p, name = var_to_map$name),
                curr_temp,
                list(x = data[, xaxis],
                     y = data[, var_to_map$y_column],
                     yaxis = "y2"),
                opacity = var_to_map$opacity,
                hovertemplate = paste('%{x|%b %d, %Y}:',
                                      '%{y}')
              ))
          }else{
            p <-
              do.call(add_trace, c(
                list(p = p, name = var_to_map$name),
                curr_temp,
                list(x = data[, xaxis],
                     y = data[, var_to_map$y_column],
                     yaxis = "y2"),
                opacity = var_to_map$opacity,
                hovertemplate = paste('%{x|%b %d, %Y}:',
                                      '%{y}'),
                visible = FALSE
              ))
          }
          
        }
        else{
          p <-
            do.call(add_trace, c(
              list(p = p, name = var_to_map$name),
              curr_temp,
              list(x = data[, xaxis],
                   y = data[, var_to_map$y_column],
                   yaxis = "y2"),
              opacity = var_to_map$opacity,
              hovertemplate = paste('%{x|%b %d, %Y}:',
                                    '%{y}')
            ))
        }
      }
    }
    
    updated_y2 <- sprintf(base_params_y2, menu_y2)
    updated_y2 <- eval(parse(text = updated_y2))
    
    # Specify plot layout
    if(is.null(yaxis2)){
      if(!isTRUE(yaxis_button)){
        p <-
          layout(
            p,
            title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
            xaxis = list(type = "date",
                         title = list(text = as.character(titles[["x"]])),
                         automargin = TRUE, tickvals = tickvals, 
                         tickformat = "%b"),
            yaxis = list(title = list(text = as.character(titles[["y"]])), 
                         automargin = TRUE),
            barmode =  "relative",
            bargap = 0,
            autosize = FALSE,
            width = width,
            height = height,
            legend = list(x = 0.025, y = 0.9),
            dragmode = "pan"
          )
      }
      else{
        p <-
          layout(
            p,
            title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
            xaxis = list(type = "date",
                         title = list(text = as.character(titles[["x"]])),
                         automargin = TRUE, tickvals = tickvals, 
                         tickformat = "%b"),
            yaxis = list(title = list(text = as.character(titles[["y"]])), 
                         automargin = TRUE),
            barmode =  "relative",
            bargap = 0,
            autosize = FALSE,
            width = width,
            height = height,
            legend = list(x = 0.05, y = 1),
            updatemenus = updated
          )
      }
    }
    else{
      if(isTRUE(yaxis_button)){
        p <-
          layout(
            p,
            title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
            xaxis = list(type = "date",
                         title = list(text = as.character(titles[["x"]])),
                         automargin = TRUE, tickvals = tickvals, 
                         tickformat = "%b"),
            yaxis = list(title = list(text = as.character(titles[["y"]])), 
                         automargin = TRUE, overlaying = "y2",
                         zeroline = FALSE),
            yaxis2 = list(
              side = "right",
              title = list(text = as.character(titles[["y2"]])),
              automargin = TRUE, 
              showgrid = FALSE
            ),
            barmode =  "relative",
            bargap = 0,
            autosize = FALSE,
            width = width,
            height = height,
            legend = list(x = 0.05, y = 0.9),
            updatemenus = updated
          )
      }
      else if(isTRUE(yaxis2_button)){
        tmp <- 2*max(data[,yaxis2[[1]][["y_column"]]], na.rm = TRUE)
        p <-
          layout(
            p,
            title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
            xaxis = list(type = "date",
                         title = list(text = as.character(titles[["x"]])),
                         automargin = TRUE, tickvals = tickvals, 
                         tickformat = "%b"),
            yaxis = list(title = list(text = as.character(titles[["y"]])), 
                         automargin = TRUE, overlaying = "y2",
                         zeroline = FALSE),
            yaxis2 = list(
              side = "right",
              title = list(text = as.character(titles[["y2"]])),
              automargin = TRUE, 
              showgrid = FALSE,
              range = c(0, tmp)
            ),
            barmode =  "relative",
            bargap = 0,
            autosize = FALSE,
            width = width,
            height = height,
            annotations = list(
              x = 1.26, y = 0.95, text = y2_button_name, 
              showarrow = F, xref='paper', yref='paper',
              font=list(size=15)
            ),
            legend = list(x = 0.05, y = 0.9),
            updatemenus = updated_y2
          )
      }
      else{
        tmp <- 2*max(data[,yaxis2[[1]][["y_column"]]], na.rm = TRUE)
        p <-
          layout(
            p,
            title = list(text = titles[["title"]], x = 0.5, autosize = TRUE),
            xaxis = list(type = "date",
                         title = list(text = as.character(titles[["x"]])),
                         automargin = TRUE, tickvals = tickvals, 
                         tickformat = "%b"),
            yaxis = list(title = list(text = as.character(titles[["y"]])), 
                         automargin = TRUE, overlaying = "y2",
                         zeroline = FALSE),
            yaxis2 = list(
              side = "right",
              title = list(text = as.character(titles[["y2"]])),
              automargin = TRUE, 
              showgrid = FALSE,
              range = c(0, tmp)
            ),
            barmode =  "relative",
            bargap = 0,
            autosize = FALSE,
            width = width,
            height = height,
            legend = list(x = 0.05, y = 0.9)
          )
      }  
    }
    return(p)
    
  }
# ---------- COLOR CHANGE FUNCTIONS ---------
change_color <- function(template, ...) {
  UseMethod("change_color", template)
}
change_color.default <- function(template, color) {
  stop("Unspecified template passed")
}

change_color.avg_data <- function(template, color){
  return(list(
    type = "scatter",
    mode = "line",
    showlegend = TRUE,
    line = list(
      color = color,
      width = 4
    )
  ))
}

change_color.observed_data <- function(template, color) {
  return(list(
    type = "bar",
    showlegend = TRUE,
    marker = list(color = color)
  ))
}

change_color.signal_data <- function(template, color) {
  return(list(
    type = "scatter",
    showlegend = TRUE,
    marker = list(color = color)
  ))
}