library(vetiver)
library(pins)
library(plumber)

house_fit <- readRDS("model_artifacts/house_fit.rds")

v <- vetiver_model(
  house_fit,
  model_name = "fm_house_price_model",
  description = "Random forest predicting Sold Price for Fargo-Moorhead area homes"
)

print(v)

board <- board_folder(path = "vetiver_board", versioned = TRUE)
vetiver_pin_write(board, v)

vetiver_write_plumber(
  board = board,
  name  = "fm_house_price_model",
  file  = "plumber.R"
)

cat(
  "\nA plumber.R file has been generated in your working directory.\n",
  "The API will be available at: http://127.0.0.1:8080\n",
  "Prediction endpoint: http://127.0.0.1:8080/predict\n",
  "Interactive docs (Swagger UI): http://127.0.0.1:8080/__docs__/\n\n"
)


pr("plumber.R") %>%
  pr_run(port = 8080)
