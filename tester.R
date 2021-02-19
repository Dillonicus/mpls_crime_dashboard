
grobjects <- function(plot, data, areaunit, dimensions = c(200, 200), theme = theme_void()){
  dat <- split(data, data[[areaunit]])
  grobject <- lapply(dat, function(x) ggplotGrob(ggplot(data = x, aes(x = aggunit, y = N)) + geom_line() + theme_void() + theme))
  
  cent_data <- st_centroid(plot$data)
  centroids <- st_coordinates(cent_data)
  bbox <- data.table(
    xmin = centroids[,1] - dimensions[1],
    xmax = centroids[,1] + dimensions[1],
    ymin = centroids[,2] - dimensions[2],
    ymax = centroids[,2] + dimensions[2]
  )
  
        
  for(i in 1:nrow(plot$data)) {
        new_layer <- annotation_custom(grob = grobject[[i]],
                                       xmin = bbox[i,xmin],
                                       xmax = bbox[i,xmax],
                                       ymin = bbox[i,ymin],
                                       ymax = bbox[i,ymax])
        
        plot$layers <- append(plot$layers, new_layer)
    
      }
  plot
  
}




ggplot(crime[, date2 := make_date(year = year, month = month)][, .N, by = .(`crime category`, date2)][], aes(x = date2, y = N, group = `crime category`, color = `crime category`)) + geom_line()

crime_summary[, .(total = sum(N)), by = .(year, community)][order(community, year)][, prev := shift(total, 1)][, pct_chg := 100 * ((total - prev) / prev)][]

subdat <- crime_summary[, .(total = sum(N)), by = .(year, month)][, date:=make_date(year, month)][]

ggplot(subdat, aes(x = month, y = total, color = factor(year), group = factor(year))) +
  geom_point(size = 3, alpha = 0.7) +
  geom_line(size = 3, alpha = 0.3) +
  dillon_theme()

subdat <- crime[, .N, by = .(year, month, violent, community)][, .(total = sum(N)), by = .(year, month, violent)][, date:=make_date(year, month)]

ggplot(subdat, aes(x = date, y = total, color = violent, group = violent)) +
  labs(title = "Monthly Crime Count", subtitle = "2010 - 2020", caption = "uh huh") +
  scale_x_date(date_breaks = '1 year', labels = function(x) year(x)) +
  geom_line(alpha = 0.4, size  = 2) +
  dillon_theme()

library(data.table)
library(ggplot2)
a <- crime[, .(.N), by = .(date)
           ][, `:=`(month = month(date), 
                    year = year(date), 
                    day = lubridate::wday(date,label = T))
             ][order(date), `:=`(num = seq_len(.N), 
                                 week = cumsum(ifelse(day == "Sun", 1, 0)) + 1), by = .(month, year)][]

crime[, .N, by = .(year, month, day, date)]

b <- ggplot(a[year == 2020,], aes(y = day, x = week, fill = N)) + geom_tile() + facet_grid(year ~ month) + scale_fill_gradient(low = "green", high = "red")


c <- ggplotGrob(b)

filler2 <- function(n){
obj <- c$grobs[[n]]$children[[3]]
width <- as.numeric(obj$width[1])
height <- as.numeric(obj$height[1])
obj_dim <- data.table(x = as.numeric(obj$x), y = as.numeric(obj$y))

d <- obj_dim[, .(x = max(x) + width), by = .(y)]
e <- d[, .(y = (y - height), x)]
f <- obj_dim[, .(x = min(x)), by = .(y)]
g <- f[, .(y = (y - height), x)]

j <- unique(rbind(d,e,f,g))
return(j)
# datx <- split(j, j$x)
# purrr::walk(1:length(datx),
#      ~{
#        #layer <- linesGrob(x = datx[[.x]][['x']], y = datx[[.x]][['y']], default.units = 'native', gp = gpar(col = 'black'))
#        #c$grobs[[n]] <<- addGrob(gTree = c$grobs[[n]], layer)
#        grid.draw(linesGrob(x = datx[[.x]][['x']], y = datx[[.x]][['y']], default.units = 'native', gp = gpar(col = 'black')))
# 
#        
#      })

# d <- obj_dim[, .(y = max(y)), by = .(x)]
# e <- d[, .(x = (x + width), y)]
# f <- obj_dim[, .(y = min(y)-height), by = .(x)]
# g <- f[, .(x = (x + width), y)]
# 
# j <- unique(rbind(d,e,f,g))
# 
# datx <- split(j, j$y)
# 
# purrr::walk(1:length(datx),
#             ~{
#               layer <- linesGrob(x = datx[[.x]][['x']], y = datx[[.x]][['y']], default.units = 'native', gp = gpar(col = 'black'))
#               c$grobs[[n]] <<- addGrob(gTree = c$grobs[[n]], layer)
#             })
# 
# plot(c)



}

calendarGrob <- function(x, y, fill, linetype = 1, arrow = NULL, color = "black") {
  segmentsGrob(x0 = ,
               x1 = ,
               y0 = ,
               y1 = )
}
