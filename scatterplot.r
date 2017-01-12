  # User specification
  dataFile <- "stopData/Join_Output.dbf"
  xField <- "DAILYBOARD"
  yField <- "DAY_ON"
    
  # program
  library(foreign)
  df <- read.dbf(dataFile)
  View(df)

  #