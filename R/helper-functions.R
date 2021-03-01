rolling_avg <- function(data,
                        param,
                        length,
                        type = "midpoint"){
  if(type == "midpoint"){
    zoo::rollapply(
      data[[as.character(param)]], width = length,
      FUN=function(x) mean(x, na.rm = TRUE),
      by=1, by.column=TRUE, partial=FALSE,
      fill=NA, align="center"
    )
  }
  else if(type == "right"){
    zoo::rollapply(
      data[[as.character(param)]], width = length,
      FUN=function(x) mean(x, na.rm = TRUE),
      by=1, by.column=TRUE, partial=FALSE,
      fill=NA, align="right"
    )
  }
  else if(type == "left"){
    zoo::rollapply(
      data[[as.character(param)]], width = length,
      FUN=function(x) mean(x, na.rm = TRUE),
      by=1, by.column=TRUE, partial=FALSE,
      fill=NA, align="left"
    )
  }
  else{
    stop(
      paste(as.character(type), "is not a valid type. Aborting operation!")
    )
  }
}