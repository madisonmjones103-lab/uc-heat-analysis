analysis_df <- data.frame(
  temperature = values(temperature_fahrenheit),
  ndvi = values(ndvi)
)

analysis_df <- na.omit(analysis_df)

write.csv(
  analysis_df,
  "web/data/temperature_ndvi.csv",
  row.names = FALSE
)
