  # User specification
  dataFile <- "stopData/Join_Output.dbf"

  # program
  library(foreign)
  library(ggplot2)
  library(dplyr)
  
  df <- read.dbf(dataFile)
  df <- df %>% 
        mutate(diff = DAILYBOARD - DAY_ON) %>%
        mutate(bin = cut(diff, 
                        breaks = c(min(diff), -500, -100, -50, -20, 0, 20, 50, 100, 500, max(diff))),
                        include.lowest = TRUE) %>%
        mutate(bin = paste(
                       as.numeric(sub("\\((.+),.*", "\\1", bin )), 
                       as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", bin )),
                       sep=" to ")) %>%
        select(ROUTE, STOPNAME, STATION, DAILYBOARD, DAY_ON, diff, bin)
  
  View(df)
  
  # scatterplot
  scatterplot <- ggplot(df, aes(DAILYBOARD, DAY_ON)) +
    geom_point(aes(colour = factor(ROUTE))) +
    geom_smooth(method ="lm") +
    coord_cartesian() +
    scale_fill_gradient(low = "red", high = "yellow")+
    theme_bw()
  
  # Compute R-Squared
  m <- lm(DAILYBOARD ~ DAY_ON, df)
  rsq <- paste0("R-Squared = ", round(summary(m)$r.squared,4))
  
  # Add R-Squared to plot    
  scatterplot <- scatterplot + 
                 geom_text(x = 2000, y = 500, label =rsq, parse = TRUE)
    
   