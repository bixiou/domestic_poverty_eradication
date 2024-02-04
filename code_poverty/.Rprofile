# Load packages
library(utils)
package <- function(p, version = NULL, remove = FALSE, github = '') {
  if (remove) {
    detach(paste0("package:", p), unload = T)
    remove.packages(p)
  }
  if (!is.element(p, installed.packages()[,1])) {
    if (missing(version)) {
      if (github != '') {
        package("devtools")
        install_github(paste0(github, '/', p))
      } else install.packages(p) # , repos='https://cran.rstudio.com/', type = 'source' may help in case of bug
    } else {
      try({detach("package:devtools", unload = T)})
      package("remotes")
      install_version(p, version = version, repos = "http://cran.us.r-project.org", upgrade = "never", dependencies = TRUE)
      package("devtools")
    }
  }
  else { if(!missing(version)) warning(paste("'", p, "' is already installed with a (potentially) newer version. You may want to install the required version (", version, ") to avoid bugs.", sep=""))}
  library(p, character.only = TRUE)
} # loads packages with automatical install if needed

package("dplyr")
package("readr")
package("readxl")
package("tidyverse")
package("Hmisc")
package("memisc")
package("ggplot2")
package("ggalt") # maps
package("openxlsx") # write.xlsx
package("RColorBrewer") # color
package("magick") # image_write
package("knitr") # plot_crop, representativeness_table
package("beepr")
package("kableExtra") # kbl
package("spatstat") # weighted.median

# install.packages("devtools")
# devtools::install_github("thomasblanchet/gpinter")
# library(gpinter)


# Functions
decrit <- function(variable, data = p, miss = TRUE, weights = NULL, numbers = FALSE, which = NULL, weight = T) { # TODO!: allow for boolean weights
  # if (!missing(data)) variable <- data[[variable]]
  if (is.character(variable) & length(variable)==1) variable <- data[[variable]]
  if (!missing(which)) variable <- variable[which]
  if (weight) {
    # if (length(variable) > 1) warning("Field 'variable' is a vector instead of a character, weight will not be used.")
    if (missing(weights)) weights <- data[["weight"]]  #  if (missing(data)) warning("Field 'data' is missing, weight will not be used.") else {
    if (!missing(which)) weights <- weights[which]
    if (length(weights)!=length(variable)) {
      warning("Lengths of weight and variable differ, non-weighted results are provided")
      weights <- NULL
    } }
  if (length(annotation(variable))>0 & !numbers) {
    if (!miss) {
      # if (is.element("Oui", levels(as.factor(variable))) | grepl("(char)", annotation(variable)) | is.element("quotient", levels(as.factor(variable)))  | is.element("Pour", levels(as.factor(variable))) | is.element("Plutôt", levels(as.factor(variable))) ) { describe(as.factor(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
      # else { describe(variable[variable!="" & !is.na(variable)], weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) }
      if (length(which(!is.na(suppressWarnings(as.numeric(levels(as.factor(variable)))))))==0) { describe(as.factor(variable[variable!=""]), weights = weights[variable!=""], descript=Label(variable)) } # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
      else { describe(as.numeric(as.vector(variable[variable!=""])), weights = weights[variable!=""], descript=Label(variable)) } # avant:  & !is.na(variable)
    }
    else {
      if (length(which(suppressWarnings(!is.na(as.numeric(levels(as.factor(variable)))))))>10) describe(include.missings(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)], descript=Label(variable)) # encore avant:  & !is.na(variable), avant: (length(which(is.numeric(levels(as.factor(variable)))))==0)
      else describe(as.factor(include.missings(variable)[include.missings(variable)!="" & !is.na(variable)]), weights = weights[include.missings(variable)!="" & !is.na(variable)], descript=Label(variable)) }
  }
  else {
    if (length(annotation(variable))>0) {
      if (miss) describe(variable[variable!=""], weights = weights[variable!=""], descript=Label(variable))
      else describe(variable[variable!="" & !is.missing(variable)], weights = weights[variable!="" & !is.missing(variable)], descript=paste(length(which(is.missing(variable))), "missing obs.", Label(variable)))
    } else describe(variable[variable!=""], weights = weights[variable!=""])  }
}

Label <- function(var) {
  if (length(annotation(var))==1) { annotation(var)[1] }
  else { label(var)  }
}
max_gap <- function(vec1, vec2, epsilon = 1e-15) return(max(2*abs(vec1 - vec2)/(abs(vec1 + vec2) + epsilon), na.rm = T))
mean_gap <- function(vec1, vec2, epsilon = 1e-15) return(mean(2*abs(vec1 - vec2)/(abs(vec1 + vec2) + epsilon), na.rm = T))
agg_thresholds <- function(vec, thresholds, labels = NULL, sep = " - ", begin = "", end = "", shift = 0, strict_ineq_lower = T, return = "vec" # min = 0, max = Inf,
) { 
  # strict_ineq_lower == T means intervals 50,60 are of type ];] while == F means [;[.
  # shift = 1 (with strict_ineq_lower == T) means levels ]50;60] will be displayed as "[begin]51[sep]60[end]".
  # thresholds <- c(min, thresholds, max)
  min <- thresholds[1]
  max <- thresholds[length(thresholds)]
  shift_left <- ifelse(strict_ineq_lower, shift, 0)
  shift_right <- ifelse(strict_ineq_lower, 0, shift)
  vec_agg <- rep(NA, length(vec))
  values <- c()
  if (missing(labels)) levels <- c()
  else levels <- labels
  for (i in 2:length(thresholds)) {
    values <- c(values, (thresholds[i] + thresholds[i-1])/2)
    min_i <- ifelse(i > 2, thresholds[i-2], min)
    max_i <- ifelse(i <= length(thresholds) - 2, thresholds[i+2], max)
    next_i <- ifelse(i <= length(thresholds) - 1, thresholds[i+1], max)
    if (missing(labels)) levels <- c(levels, if (thresholds[i-1]==thresholds[i]) paste0(begin, thresholds[i], end) else paste0(begin, thresholds[i-1] + (shift_left*(i < length(thresholds)) + shift_right*(thresholds[i-1] == min_i))*(i > 2), sep,
                                                                                                                               thresholds[i] - (shift_right*(i > 2) + shift_left*(thresholds[i] == next_i))*(i < length(thresholds)), end))
    if (strict_ineq_lower) vec_agg[(vec <= thresholds[i] & vec < max_i & vec > thresholds[i-1]) | (vec == max_i & i == length(thresholds)) | (vec == thresholds[i] & i < length(thresholds) & vec < max_i) | (i == 2 & vec == min)] <- (thresholds[i] + thresholds[i-1])/2
    else vec_agg[(vec < thresholds[i] & vec >= thresholds[i-1] & vec > min_i) | (vec == min_i & i == 2) | (vec == thresholds[i-1] & i > 2 & vec > min_i) | (i == length(thresholds) & vec == max)] <- (thresholds[i] + thresholds[i-1])/2
  }
  if (min == -Inf & strict_ineq_lower) levels[1] <- sub(paste0("-Inf", sep), "≤ ", levels[1])
  if (min == -Inf & !strict_ineq_lower) levels[1] <- sub(paste0("-Inf", sep), "< ", levels[1])
  if (max == Inf & strict_ineq_lower) levels[length(levels)] <- sub(paste0("(.*)", sep, "Inf"), "> \\1", levels[length(levels)]) # sub(" ", "", sep)
  if (max == Inf & !strict_ineq_lower) levels[length(levels)] <- sub(paste0("(.*)", sep, "Inf"), "≥ \\1", levels[length(levels)]) # sub(" ", "", sep)
  levels <- gsub("000 ", ",000 ", gsub("-", "–", levels))
  vec_agg[is.na(vec)] <- NA
  vec_agg <- as.item(vec_agg, labels = structure(values, names = levels), missing.values = c("",NA), annotation=Label(vec))
  if (return == "vec") return(vec_agg)
  else if (return %in% c("levels", "labels")) return(levels)
  else if (return == "values") return(values)
}

Levels <- function(variable, data = p, miss = TRUE, numbers = FALSE, values = TRUE, concatenate = FALSE, max_values = 13, names = FALSE) {
  if (values) {
    if (length(variable)==1 & is.character(variable)) variable <- data[[variable]]
    # if (length(annotation(variable))==1 & !is.null(labels(variable))) Levs <- as.character(labels(variable)) # old, gave numbers instead of labels for double.item
    if ("double.item" %in% class(variable) & !is.null(labels(variable))) Levs <- names(as.character(labels(variable)))
    else if (("character.item" %in% class(variable) | length(annotation(variable))==1) & !is.null(labels(variable))) Levs <- as.character(labels(variable))
    else if (is.factor(variable)) Levs <- levels(variable)
    else if (is.numeric(variable)) {
      if (length(unique(variable)) > max_values) {
        Levs <- round(c(min(variable, na.rm = T), max(variable, na.rm = T)), 3)
        names(Levs) <- c("min", "max")
      } else Levs <- round(sort(unique(variable)), 3) }
    else if (is.character(variable)) Levs <- if (length(unique(variable)) > max_values) "[string variable]" else as.character(unique(variable))
    else if (is.logical(variable)) Levs <- if (any(is.pnr(variable))) "TRUE / FALSE / NA" else "TRUE / FALSE"
    if (concatenate) {
      if (is.null(names(Levs))) Levs <- paste(Levs, collapse = " / ")
      else Levs <- paste(sapply(1:length(Levs), function(i) return(paste0(names(Levs)[i], ": ", Levs[i]))), collapse = " / ")
    }
  } else {
    Levs <- decrit(variable, miss = miss, numbers = numbers, data = data)$values$value
    if (is.null(Levs)) {
      if (is.character(variable) & length(variable)==1) variable <- data[[variable]]
      Levs <- unique(variable)
      if (concatenate) Levs <- paste(Levs, collapse = " / ") } }
  if (names & length(names(Levs)) == length(Levs)) Levs <- names(Levs)
  return(Levs)
  # if (is.character(var) & length(var)==1) var <- data[[var]]
  # if (length(annotation(var))>0) { # works but cubmbersome and doesn't allow to get rid of missings
  #   if (is.character(var)) levels(as.factor(include.missings(var)))
  #   else return(as.vector(labels(var))) }
  # else return(levels(as.factor(var))) # as.factor may cause issues as it converts to string
}


plot_world_map <- function(var, condition = "", df = p, on_control = FALSE, save = FALSE, continuous = FALSE, width = dev.size('px')[1], height = dev.size('px')[2], legend_x = .05, rev_color = FALSE, add_folder = '',
                           breaks = NULL, labels = NULL, legend = NULL, limits = NULL, fill_na = FALSE, format = "png", trim = T, sep = "% to ", end = "%", strict_ineq_lower = FALSE) {
  if (is.null(breaks)) breaks <- c(-Inf, seq(0, 1, .2), Inf)
  if (is.null(labels)) labels <- sub("≤", "<", sub("≥", ">", agg_thresholds(c(0), breaks, sep = sep, end = end, strict_ineq_lower = strict_ineq_lower, return = "levels")))
  if (is.null(limits)) limits <- c(-.01, 100.01)
  
  df <- data.frame(country_map = df$country, mean = pmin(limits[2], pmax(limits[1], df[[var]])))
  if (continuous) df$mean <- pmax(pmin(df$mean, limits[2]), limits[1])
  
  world_map <- map_data(map = "world")
  world_map <- world_map[world_map$region != "Antarctica",] #
  world_map <- world_map[!world_map$region %in% c("Antarctica", "American Samoa", "Micronesia", "Guam", "Niue", "Pitcairn Islands", "Cook Islands", "Tonga", "Kiribati", "Marshall Islands", "French Polynesia", "Fiji", "Samoa", "Wallis and Futuna", "Vanuatu"),]
  # world_map$region <- iso.alpha(world_map$region)
  world_map$region[world_map$region %in% c("French Guiana", "New Caledonia")] <- "France"
  
  df_na <- data.frame(country_map = setdiff(world_map$region, df$country), mean = if (fill_na) breaks[2] else NA)
  df <- merge(df, df_na, all = T)
  
  df$group <- cut(df$mean, breaks = breaks, labels = labels, right = strict_ineq_lower)
  
  if (!continuous) {
    (plot <- ggplot(df) + geom_map(aes(map_id = country_map, fill = fct_rev(group)), map = world_map) + coord_proj("+proj=robin", xlim = c(-135, 178.5), ylim = c(-56, 84)) + #geom_sf() + #devtools::install_github("eliocamp/ggalt@new-coord-proj") update ggplot2 xlim = c(162, 178.5) for mercator
       geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'grey', size = 0,  fill = NA) + expand_limits(x = world_map$long, y = world_map$lat) + theme_void() + theme(legend.position = c(legend_x, .29)) + # coord_fixed() +
       scale_fill_manual(name = legend, drop = FALSE, values = color(length(breaks)-1, rev_color = rev_color))) #, na.value = "grey50" +proj=eck4 (equal area) +proj=wintri (compromise) +proj=robin (compromise, default) Without ggalt::coord_proj(), the default use is a sort of mercator
  } else {
    (plot <- ggplot(df) + geom_map(aes(map_id = country_map, fill = mean), map = world_map) + coord_proj("+proj=robin") + #geom_sf() + #devtools::install_github("eliocamp/ggalt@new-coord-proj")
       geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'grey', fill = NA) + expand_limits(x = world_map$long, y = world_map$lat) + theme_void() + coord_fixed() +
       scale_fill_manual(palette = "RdBu", direction = 1, limits = limits, na.value = "grey50")) #scale_fill_viridis_c(option = "plasma", trans = "sqrt"))
  }
  
  print(plot)
  if (save) for (f in format) save_plot(plot, filename = ifelse(continuous, paste0(var, "_cont"), var), folder = paste0('../figures/', add_folder), width = width, height = height, format = f, trim = trim)
  # return(plot)
}

color <- function(v, grey=FALSE, grey_replaces_last = T, rev_color = FALSE, theme='RdBu') { # TODO! whitout white
  if (is.matrix(v)) n <- nrow(v)
  else if (length(v) > 1) n <- length(v)
  else n <- v # cf. http://research.stowers.org/mcm/efg/R/Color/Chart/ColorChart.pdf
  if (grey & grey_replaces_last & n > 1) n <- n-1
  if (theme=='rainbow') {
    if (n == 1) cols <- c("#66B3B3") # "brown": #A52A2A Presentation Teal: #008096 (title) #1A8C8C (dark) #66B3B3 #99CCCC (light)
    else if (n == 2) cols <- c("#66B3B3", "#A52A2A") # c("lightgreen", "plum") = c("#90EE90", "#DDA0DD")
    else if (n == 3) cols <- color5[c(1,3,5)]
    else if (n == 4) cols <- c(rainbow(4, end=4/15)[1:3], "#228B22")
    else if (n == 5) cols <- c(rainbow(4, end=4/15)[1:3], "#00FF00", "#228B22") # the last two are: green, forestgreen
    else if (n == 6) cols <- rainbow(6)
    else if (n == 7) cols <- c("#000000", rainbow(7)[c(1:3,5:7)])
    else cols <- rainbow(n) # diverge_hcl green2red brewer.pal(n, Spectral/RdBu...)  https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
  } else if (theme=='default') {
    cols <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))(n)
  } else {
    cols <- rev(brewer.pal(max(n, 3), theme))
    if (n == 1) cols <- cols[1]
    # if (n == 2) cols <- cols[c(1,3)]
    else if (n %% 2 == 0) cols <- rev(brewer.pal(n+2, theme))[c(1:(n/2),(n/2+2):(n+1))] }
  if (n > 10) cols <- colorRampPalette(cols)(n)
  if (rev_color) cols <- rev(cols)
  if (grey & n > 1) return(c(cols, "#D3D3D3")) # lightgrey
  else return(cols)
}

save_plot <- function(plot=NULL, filename = deparse(substitute(plot)), folder = '../figures/', width = dev.size('px')[1], height = dev.size('px')[2], method='dev', trim = T, format = 'png') {
  if (any(class(plot) %in% c("data.frame", "array"))) {
    # file <- paste(folder, "xls/", filename, ".xlsx", sep='')
    file <- paste(sub("figures", "xlsx", folder), filename, ".xlsx", sep='') # xlsx
    write.xlsx(as.data.frame(plot), file, row.names = T, overwrite = T)
  } else {
    file <- paste0(folder, filename, ".", format)
    # print(file)
    if (grepl('dev', method)) {
      if (format == 'png') {
        dev.copy(png, filename = file, width = width, height = height) # save plot from R (not plotly)
        dev.off() }
      else if (format == 'svg') {
        dev.copy(svg, filename = file, width = width/100, height = height/100) # save plot from R (not plotly)
        dev.off() } # TODO choose width height with PDF
      else if (format == 'pdf') dev.print(pdf, file = file) # because dev.size('px')[1]/dev.size('in')[1] = 105 , width = width/105, height = height/105
    }
    else {
      server <- orca_serve() # doesn't work within a function because requires admin rights
      server$export(plot, file = file, width = width, height = height)
      server$close()
    }
    if (trim & format %in% c('png')) image_write(image_trim(image_read(file)), file) # , 'svg'
    if (trim & format == 'pdf') plot_crop(file) } # to crop pdf, see also code_oecd/crop_pdf.sh and run it in the desired folder
}
