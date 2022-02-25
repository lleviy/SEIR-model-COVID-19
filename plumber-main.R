if (!require("plumber")) install.packages("plumber")

library(plumber)

pr <- plumb("plumber-api.R")

swaggerFile <- pr$getApiSpec()
swaggerFile$info$title <- "SEIR-модель для прогнозирования распространения COVID-19"
swaggerFile$info$description <- "Открытый API для предоставления прогнозных данных о заболеваемости COVID-19."
pr_set_api_spec(pr, swaggerFile)

handler_error <- function(req, res, err){
  res$status <- 400
  list(error = "Недопустимые значения в запросе.")
}


pr_set_error(pr, handler_error)
pr$run(port=8000)

