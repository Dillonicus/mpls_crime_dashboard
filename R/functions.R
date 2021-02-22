#==============================================================================#
# functions.R
#
# This script contains functions that are used repeatedly in the operation of
# the application. These functions are used primarily to reactively build plots
# and tables using dynamic values from the inputs tab.
#==============================================================================#

# Themes/Icons -----------------------------------------------------------------

dillon_theme <- function(base_size = 21, ...){
  ggplot2::theme(axis.text = element_text(color = "white"),
                 plot.title = element_text(size = rel(1.3)),
                 plot.subtitle = element_text(size = rel(.8)),
                 plot.background = element_rect(fill = "#212121", size = 0),
                 
                 panel.background = element_rect(fill = "#212121", size = 0),
                 panel.grid.major = element_line(color = "grey", linetype = 3),
                 panel.grid.minor = element_blank(),
                 
                 axis.line = element_line(color = "grey"),
                 axis.title = element_text(size = rel(1)),
                 axis.text.x = element_text(size = rel(0.8), angle = 45, hjust = 1), 
                 axis.text.y = element_text(size = rel(0.8), angle = 45),
                 
                 legend.direction = "horizontal",
                 legend.text = element_text(size = rel(0.8)),
                 legend.position = "bottom",
                 legend.key = element_rect(fill = NA),
                 legend.title = element_blank(),
                 legend.background = element_rect(fill = "#212121"),
                 
                 text = element_text(color = "white", size = base_size)
  )
}

test_pal <- manual_pal(
  c(
    "#4f8de9",
    "#658D56",
    "#428F9B",
    "#D78359",
    "#FCC731",
    "#A9E2B6",
    "#e74c3c",
    "#a07efa",
    "#c16ea4",
    "#bf5e78",
    "#BDC3C7",
    "#F0EEE3",
    "#584432"))
pal <- colorRampPalette(c("#e74c3c", "#4f8de9","#A9E2B6"))

t3 <- colorRampPalette(c("#F0EEE3", "#FCC731", "#e74c3c"))

# Customize icons
crimeIcons <- iconList(
  Arson = makeIcon(iconUrl ="./icon/Arson.png", iconWidth = 35, iconHeight = 35),
  `Assault` = makeIcon(iconUrl = "./icon/Assault.png", iconWidth = 35, iconHeight = 35),
  `Auto Theft` = makeIcon(iconUrl = "./icon/Auto Theft.png", iconWidth = 35, iconHeight = 35),
  `Burglary` = makeIcon(iconUrl = "./icon/Burglary.png", iconWidth = 35, iconHeight = 35),
  Larceny = makeIcon(iconUrl = "./icon/Larceny.png", iconWidth = 35, iconHeight = 35),
  Murder = makeIcon(iconUrl ="./icon/Murder.png" ,iconWidth =35 ,iconHeight = 35),
  `Rape` = makeIcon(iconUrl = "./icon/Rape.png", iconWidth = 35, iconHeight = 35),
  Robbery = makeIcon(iconUrl = "./icon/Robbery.png",iconWidth = 35, iconHeight = 35)
)


# Crime Statistics -------------------------------------------------------------

# Generate a mosaic plot using dynamic inputs for geographical areas 
# (communities, neighborhoods) and temporal units (year, month, day, hour) of
# interest.

mosaic_plot <- function(data, timeunit, areaunit, selected){
  
  if(is.null(selected)){
    areafilter <- unique(data[[areaunit]])
  }
  else{
    areafilter <- selected
  }
  
  byfilt = c(timeunit, "crime category")
  
  mos_data <- data[data[[areaunit]] %in% areafilter, .N, by = byfilt
                    ][, `:=`(prop = N/sum(N)), by = c(timeunit)
                      ][, `crime category` := fct_reorder(`crime category`, 
                                                          .fun = mean, 
                                                          .x = prop)]
  ggplot(
    data = mos_data, 
    aes(x = !!sym(timeunit), fill = `crime category`, weight = N)) +
    geom_bar(position = position_fill()) +
    scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
    scale_fill_manual(values = pal(9)) +
    labs(x = str_to_title(timeunit), y = "Percent", 
         title = glue("Average Percent of Crimes per {str_to_title(timeunit)} by Crime Category")) +
    dillon_theme(base_size = 21)
  
}

top_n_plot <- function(data, n, dates, grouping) {
  
  dat <- data[date >= dates[1] & date <= dates[2], ][, .N, by = c(grouping)]
  setorder(dat, -N)
  dat <- dat[1:n, .(x = .SD[[1]], y = .SD[[2]])]
  dat[, `:=`(x = forcats::fct_reorder(.f = x, .x = y, .fun = min, .desc = F))]
  
  subtitle <- glue("{dates[1]} to {dates[2]}")
  
  ggplot(dat, aes(x = x, y = y)) + 
    geom_bar(stat = "identity", alpha = .8, fill = pal(2)[2], width = .5) + 
    ggplot2::coord_flip() +
    labs(x = "",
         y = "Number of Incidents",
         title = "Most Frequent Crimes:", 
         subtitle = subtitle) +
    dillon_theme() + 
    theme(axis.text.x = element_text(angle = 0),
          axis.text.y = element_text(angle = 0))
}

change_from <- function(data) {
  today <- Sys.Date() - years(1)
  yr <- year(today)
  mo <- month(today)
  day <- mday(today)
  
  dat <- data[, .N, by = .(date, violent)
  ][, `:=`(year = year(date), month = month(date), mday = mday(date))
  ]
  dat <- dat[year %in% c(yr, yr-1), 
  ][month <= mo, 
  ][mday <= day, .(total = sum(N)), by = .(year, violent)]
  
  out <- dcast(dat, violent ~ year, value.var = 'total')
  out[, `:=`(pct_change = 100 * (.SD[[3]] - .SD[[2]])/.SD[[2]])
  ][, `:=`(neg = ifelse(pct_change < 0, 1, 0),
           pct_change = paste0(round(pct_change), "%"))
  ][, `:=`(color = ifelse(neg == 1, 'green', 'red'),
           symbol = ifelse(neg == 1, 'angle-down', 'angle-up'))
  ][]
}

# Community Profiles -----------------------------------------------------------

monthly_plot <- function(data, year, areaunit, selected) {
  
  byfilter <- c("year", "month", "violent", areaunit)
  if(is.null(selected)){
    areafilter <- unique(data[[areaunit]])
  }
  else{
    areafilter <- selected
  }
  
  newdata <- data[data[[areaunit]] %in% areafilter, .N, by = byfilter
                  ][, .(total = sum(N)), by = .(year, month, violent)
                    ][, date := make_date(year, month)][]

  ggplot(newdata, aes(x = date, y = total, color = violent, group = violent)) +
    labs(title = "Monthly Crime Count", subtitle = paste(c(selected, " 2010 - 2010"), collapse = ":")) +
    scale_x_date(date_breaks = "1 year", labels = function(x) year(x)) +
    geom_line(alpha = 0.4, size  = 2) +
    dillon_theme()
}

# monthly_plot <- function(.data, .year, .community, .community_filt, .areaunit) {
#   
#   by_filt <- c("year", "month", "violent", .areaunit)
#   
#   subdat <- .data[community %in% .community_filt, .N, by = by_filt
#   ][, .(total = sum(N)), by = .(year, month, violent)
#   ][, date:=make_date(year, month)]
#   
#   ggplot(subdat, aes(x = date, y = total, color = violent, group = violent)) +
#     labs(title = "Monthly Crime Count", subtitle = paste0(.community, ": 2010 - 2020")) +
#     scale_x_date(date_breaks = "1 year", labels = function(x) year(x)) +
#     geom_line(alpha = 0.4, size  = 2) +
#     dillon_theme()
#   }

summary_table <- function(data, years, areaunit, selected) {
  
  if(is.null(selected)){
    areafilter <- unique(data[[areaunit]])
  }
  else{
    areafilter <- selected
  }
  
  table_data <- data[data[[areaunit]] %in% areafilter & year %in% c(years), .N, by = .(year, violent, `crime category`)]
  table_data <- dcast(table_data, violent + `crime category` ~ year, value.var = "N")
  
  stat <- names(table_data)[c(1,2)]
  yrs <- names(table_data)[-c(1,2)]
  
  reactable::reactable(
    table_data,
    groupBy = "violent",
    defaultExpanded = T,
    defaultSorted = yrs,
    defaultSortOrder = "desc",
    defaultColDef = colDef(
      footer = function(values) if(is.numeric(values)) sum(values, na.rm = T),
      footerStyle = list(fontWeight = "bold")),
    columnGroups = list(
      colGroup(name = "", columns = stat),
      colGroup(name = "Year", columns = yrs)
      ),
    columns = list(
      violent = colDef(footer = "Total", name = "Category"),
      `crime category` = colDef(name = "Offense")
      )
    )
}
# summary_table <- function(.data, .year, .community){
#   
#   table_data <- .data[year %in% .year,][community %in% .community, .N, by = .(year, violent, `crime category`)]
#   table_data <- dcast(table_data, violent + `crime category` ~ year, value.var = "N")
#   stat <- names(table_data)[c(1,2)]
#   yrs <- names(table_data)[-c(1,2)]
#   
#   reactable::reactable(
#     table_data, 
#     groupBy = "violent", 
#     defaultExpanded = T,
#     defaultSorted = yrs,
#     defaultSortOrder = "desc",
#     defaultColDef = colDef(
#       footer = function(values) if(is.numeric(values)) sum(values, na.rm = T),
#       footerStyle = list(fontWeight = "bold")),
#     columnGroups = list(
#       colGroup(name = "", columns = stat),
#       colGroup(name = "Year", columns = yrs)
#       ),
#     columns = list(
#       violent = colDef(footer = "Total", name = "Category"),
#       `crime category` = colDef(name = "Offense")
#       )
#     )
# }


setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL,
                           options = NULL, label = NULL, highlight = NULL
){
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip
               )))
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1] # drop layer column
  
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style, label);
}
# Unused/Other Plots -----------------------------------------------------------

# # Generates a ridgeline plot showing the crime counts/distributional shape 
# # using dynamic inputs for geographical areas (communities, neighborhoods) and 
# # temporal units (year, month, day, hour) of interest. 
# ridgeline_plot <- function(.data, .timeunit, .community){
#   
#   community_filt <- when(.community,
#                          . == "All Communities" ~ crime[, unique(community)],
#                          . != "All Communities" ~ .)
# 
#   areaunit <- ifelse(.community == "All Communities",
#                      "community",
#                      "neighborhood")
#   
#   byfilt = c(.timeunit, areaunit)
#   ridge_data <- .data[community %in% community_filt, .N, by = byfilt
#                       ][, `:=`(mean = mean(N)), by = areaunit
#                         ][, `:=`(area = fct_reorder(.SD[[1]], .x = mean, max)), 
#                           .SDcols = areaunit]
#   
#   ridge_data %>%
#     ggplot(data = ., aes(x = N, y = area)) +
#     geom_density_ridges_gradient(aes(rel_min_height = 0.01), jittered_points = T) +
#     labs(x = "Number of Crimes",
#          y = areaunit) +
#     dillon_theme()
# }
# 
# # Generates a lollipop plot showing the number of standard deviations above or
# # below the mean number of crimes ng dynamic 
# # inputs for geographical areas (communities, neighborhoods) and temporal units 
# # (year, month, day, hour) of interest. 

lolli_plot <- function(data, areaunit, selected = NULL) {
  
 if(areaunit == "community") {
   byfilt <- "community"
   } 
  else { 
    byfilt <- c("community", "neighborhood")
  }
  
  if(is.null(selected)) {
    areafilt <- unique(data[[areaunit]])
  }
  else {
    areafilt <- selected
  }
  
  lolli_data <- data[, .N, by = .(community, neighborhood)
                     ][, `:=`(total = sum(N)), by = byfilt
                       ][, `:=`(standardized = round((total - mean(total))/sd(total, na.rm = TRUE), 2))
                         ][, `:=`(pos = factor(standardized > 0))
                           ][, `:=`(area = fct_reorder(.SD[[1]], standardized, max)), .SDcols = areaunit]
  print(lolli_data[order(-standardized)])
  out <- ggplot(data = lolli_data,
         aes(x = area, y = standardized, label = standardized)) +
    geom_point(stat = "identity", aes(col = pos), size = 7) +
    geom_segment(
      aes(
        y = 0,
        xend = area,
        yend = standardized,
        col = pos
        )) +
    scale_color_manual(values = c("#e74c3c", "#428F9B")) +
    geom_text(color="white", size=4) +
    labs(title="Standardized Number of Crimes",
         y = "Standard Deviations",
         x = areaunit) +
    theme(legend.position = "none") +
    ylim(-3, 3) +
    coord_flip() +
    dillon_theme()

  out
}

# lolli_plot <- function(.data, .community){
#   
#   community_filt <- when(.community,
#                          . == "All Communities" ~ crime[, unique(community)],
#                          . != "All Communities" ~ .)
#   
#   areaunit <- ifelse(.community == "All Communities",
#                      "community",
#                      "neighborhood")
#   
#   lolli_data <- .data[community %in% community_filt, .N, by = c(areaunit)
#                       ][, `:=`(standardized = round((N - mean(N))/sd(N, na.rm = T),2))
#                         ][, `:=`(pos = factor(standardized > 0))
#                           ][, `:=`(area = fct_reorder(.SD[[1]], standardized, max)),
#                             .SDcols = c(areaunit)]
#   
#   ggplot(data = lolli_data,
#          aes(x = area, y = standardized, label = standardized)) +
#     geom_point(stat = "identity", aes(col = pos), size = 7) +
#     geom_segment(aes(y = 0,
#                      xend = area,
#                      yend = standardized,
#                      col = pos
#     )) +
#     scale_color_manual(values = c("#e74c3c", "#428F9B")) +
#     geom_text(color="white", size=4) +
#     labs(title="Standardized Number of Crimes",
#          y = "Standard Deviations",
#          x = areaunit) +
#     theme(legend.position = "none") +
#     ylim(-3, 3) +
#     coord_flip() +
#     dillon_theme()
# }
# 
# monthly_plot2 <- function(.data, .year, .community){
#   n_cols <- length(.year)
#   community_filt <- when(.community,
#                          . == "" ~ crime[, unique(community)],
#                          . != "" ~ .)
#   
#   table_data <- .data[year %in% .year,
#                       ][community %in% community_filt, .N, by = .(year, month)]
#   
#   ggplot(data = table_data, aes(x = month, y = N, group = factor(year), color = factor(year))) +
#     geom_point(size = 3, alpha = 0.7) +
#     geom_line(size = 3, alpha = 0.3) +
#     scale_color_manual(values = pal(n_cols)) +
#     dillon_theme(base_size = 21)
# }
# # Generates a table with various summary measures/crime statistics using dynamic 
# # inputs for geographical areas (communities, neighborhoods) and temporal units 
# # (year, month, day, hour) of interest. 
# summary_table2 <- function(.data, .timeunit, .community){
#   
#   community_filt <- when(.community,
#                          . == "All Communities" ~ crime[, unique(community)],
#                          . != "All Communities" ~ .)
#   
#   areaunit <- ifelse(.community == "",
#                      "community",
#                      "neighborhood")
#   
#   names <- c(areaunit, "# of crimes", glue("avg # crimes/{.timeunit}"), 
#              "# most common crime", "most common crime", "max crime date", 
#              "# crimes max crime date")
#   
#   data <- .data[community %in% community_filt,]
#   
#   data1 <- data[, .N, by = areaunit
#                 ][, 
#                   `:=`(
#                     avg = N/time_length(
#                       x = interval(min(.data$date), 
#                                    max(.data$date)), 
#                       unit = tolower(.timeunit)))][]
#   data2 <- data[, .(M = .N), by = c(areaunit, "crime description")
#                 ][, `:=`(N = sum(M)), by = c(areaunit)]
#   setorderv(data2, c("M", areaunit), c(-1, 1))
#   data2 <- data2[, .SD[1], .SDcols = c("M", "crime description"), by = c(areaunit)]
#   data3 <- data[, .(O = .N), by = c(areaunit, "date")][order(-O), .SD[1], by = c(areaunit)]
#   table <- data1[data2, on = c(areaunit)][data3, on = c(areaunit)]
#   setnames(table, old = names(table), new = str_to_title(names))
#   table
# }
# 
# heatcal <- function(dates, week.start = 7, xvar, yvar, calc.segments = TRUE) {
#   replace.na <- function(DT, var, val) {
#     for (i in 1:length(var))
#       DT[is.na(get(var[i])), (var[i]):=val[i]][]
#   }
#   
#   dat <- data.table(date = dates)[, .N, keyby = date]
#   max.date <- max(dates)
#   max.year.date <- as.Date(paste0(year(max.date), "-12-31"))
#   
#   if (max.date < max.year.date) {
#     dat <- rbind(dat, data.table(date = seq(max.date + 1, max.year.date, by = 1)), fill = T)
#   }
#   
#   clean_dat <- dat[, `:=`(year = lubridate::year(date), 
#                           month = lubridate::month(date),
#                           month_lab = lubridate::month(date, label = T), 
#                           day = lubridate::wday(date, week_start = week.start),
#                           day_lab = lubridate::wday(date, label = T, week_start = week.start))
#   ][, week := cumsum(ifelse(day == 1, 1, 0)) + 1, keyby = .(year)
#   ][, diffmean := (N - mean(N, na.rm = T)), keyby = .(year, month)
#   ][, diffmean := ifelse(is.na(diffmean), 0, diffmean)][]
#   
#   if(calc.segments == FALSE) return(clean_dat)
#   
#   perim_dat <- clean_dat[, .(year, month, x = .SD[[1]], y = .SD[[2]]), .SDcols = c(xvar, yvar)]
#   
#   perim_dat[, `:=`(x_ind = ifelse(x == min(x), "start", ifelse(x == max(x), "end", "middle"))), by = .(year, month, y)
#   ][, `:=`(y_ind = ifelse(y == min(y), "start", ifelse(y == max(y), "end", "middle"))), by = .(year, month, x)
#   ][, `:=`(middle = ifelse(x_ind == "middle" & y_ind == "middle", 1, 0))]
#   
#   x <- perim_dat[middle == 0, .(x0 = min(x) - 0.5, x1 = max(x) + 0.5), by = .(year, month, y)
#   ][, `:=`(y0 = y - 0.5, y1 = y + 0.5)][, .SD, .SDcols = -c("y")]
#   x <- melt(x, measure.vars = c("x0", "x1"), variable.name = "xvar", value.name = "x")
#   x <- melt(x, measure.vars = c("y0", "y1"), variable.name = "yvar", value.name = "y")
#   x <- unique(x[, .(year, month, x, y)])[, `:=`(grp = "x",seg = .GRP), by = .(x)]
#   
#   y <- perim_dat[middle == 0, .(y0 = min(y) - 0.5, y1 = max(y) + 0.5), by = .(year, month, x)
#   ][, `:=`(x0 = x - 0.5, x1 = x + 0.5)][, .SD, .SDcols = -c("x")]
#   y <- melt(y, measure.vars = c("x0", "x1"), variable.name = "xvar", value.name = "x")
#   y <- melt(y, measure.vars = c("y0", "y1"), variable.name = "yvar", value.name = "y")
#   y <- unique(y[, .(year, month, x, y)])[, `:=`(grp = "y", seg = .GRP), by = .(y)]
#   
#   xy <- rbind(x, y)[order(year, month, grp, seg, x, y)]
#   
#   xy[, `:=`(seg_ind = seg - shift(seg, 1)), by = .(year, month, grp)
#   ][grp == "x", `:=`(lag = y - shift(y, 1)), by = .(year, month, grp, seg)
#   ][grp == "y", `:=`(lag = x - shift(x, 1)), by = .(year, month, grp, seg)]
#   
#   replace.na(xy, c("seg_ind", "lag"), c(1, 1))
#   
#   out <- xy[, `:=`(seg_ind = cumsum(ifelse(lag > 1, 1, seg_ind))), by = .(year, month, grp)
#   ][, `:=`(grp = paste0(grp, seg_ind))
#   ][, .(x0 = min(x), x1 = max(x), y0 = min(y), y1 = max(y)), by = .(year, month, grp)]
#   return(unique(out))
# }
# 
# heatcal_plot <- function(dates, week.start = 7, xvar, yvar) {
#   border.data <- heatcal(dates = dates, week.start = week.start, xvar = xvar, yvar = yvar, calc.segments = T)
#   tile.data <- heatcal(dates = dates, week.start = week.start, xvar = xvar, yvar = yvar, calc.segments = F)
#   
#   ggplot() +
#     geom_tile(data = tile.data, aes(x = week, y = day, fill = diffmean), color = "grey") +
#     geom_segment(data = border.data, aes(x = x0, y = y0, xend = x1, yend = y1)) +
#     facet_wrap(year~., strip.position = "top", ncol = 1) +
#     scale_fill_gradient2(high = "red", low = "green",mid = "white", midpoint = 0, na.value = "white") + 
#     theme_void() + 
#     theme(axis.text.y = element_text(size = rel(.8)),
#           legend.key.height = unit(150, "native"),
#           legend.key.width = unit(8, "native"),
#           legend.text = element_text(rel(.8)),
#           legend.title = element_blank(),
#           axis.text.x = element_text(size = rel(.8))) 
# }
