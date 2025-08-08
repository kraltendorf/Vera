# Registration of 'USDA-ARS Vera': A new public hop cultivar with tropical, stone-fruit aroma and powdery mildew resistance
# Last Updated: August 7, 2025
# see: https://www.datanovia.com/en/blog/beautiful-radar-chart-in-r-using-fmsb-and-ggplot-packages/

# set working directory for the location of the data files 
setwd("/users/kayla.altendorf/OneDrive - USDA/Documents/2025/W1108-333 Vera Release/Data/")

# set working directory
#install.packages('fmsb')
library(fmsb)
library(tidyverse)
library(readxl)

# read in the data for dry hop rubs
hopsource <- read_excel("./HopSource Results.xlsx") # sheet 1

# arrange data
spider <- hopsource %>% dplyr::select(-Sample, -Type, -Location, -Hedonic, -Evaluators)
spider <- spider %>% column_to_rownames(var="Year")

# set function 
create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

# reduce plot margin using par()
op <- par(mar = c(1, 0.5, 0.5, 1))
create_beautiful_radarchart(spider, caxislabels = c(0, 0.25, 0.50, 0.75, 1), vlcex = 0.4)
par(op)

op <- par(mar = c(1, 2, 1, 2), xpd = TRUE)

# create the radar charts
create_beautiful_radarchart(
  data = spider, caxislabels =  c(0, 0.25, 0.50, 0.75, 1),
  color = c("#0072B2", "#E69F00", "#CC79A7", "#D55E00"),
)

# add an horizontal legend
legend(
  x = "bottom", legend = rownames(spider[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#0072B2", "#E69F00", "#CC79A7", "#D55E00"),
  text.col = "black", cex = 1, pt.cex = 2
)
par(op)

# three separate figures
# define colors and titles
colors <- c("#0072B2", "#E69F00", "#CC79A7")
titles <- c("2022 (38)", "2023 (127)", "2024 (33)")

# deduce plot margin using par()
# split the screen in 3 parts
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(1,3))

# create the radar chart
for(i in 1:3){
  create_beautiful_radarchart(
    data = spider[c(1, 2, i+2), ], caxislabels = c(0, 0.25, 0.50, 0.75, 1),
    color = colors[i], title = titles[i], vlcex = 0.65
  )
}
par(op)


## spider plot of the beer
beer <- read_excel("./HopSource Results.xlsx", sheet = 2)
beer <- beer %>% dplyr::select(-Year, -Sample, -Event, -n_evals, -hedonic)

# filter out Golden Road, New Belgium, and Odell 
beer <- beer %>% filter(Brewer %in% c("Min", "Max", "Golden Road (43)", "New Belgium (47)", "Odell (48)"))
beer <- beer %>% column_to_rownames(var="Brewer")

op <- par(mar = c(1, 2, 1, 2), xpd = TRUE)

# create the radar charts
create_beautiful_radarchart(
  data = beer, caxislabels =  c(0, 0.25, 0.50, 0.75, 1),
  color = c("#0072B2", "#E69F00", "#CC79A7", "#D55E00"),
)

# add an horizontal legend
legend(
  x = "bottom", legend = rownames(beer[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#0072B2", "#E69F00", "#CC79A7", "#D55E00"),
  text.col = "black", cex = 1, pt.cex = 2
)
par(op)
