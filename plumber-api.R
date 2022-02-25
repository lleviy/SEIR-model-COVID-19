SEIR <- readRDS("seir_model.rds")


#* Возвращает модельные и реальные данные по заболеваемости COVID-19
#* @param region субъект РФ
#* @param start_date начальная дата периода, на котором должна быть обучена модель
#* @param end_date конечная дата периода, на котором должна быть обучена модель
#* @param predict_up_to_date дата, до которой строить прогноз
#* @get /model
function(region, start_date, end_date, predict_up_to_date) {
  SEIR(region, start_date, end_date, predict_up_to_date)
}