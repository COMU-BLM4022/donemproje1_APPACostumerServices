
#' Title
#'
#' @return
#' @export
kutuphaneler <- function() {
  library("readxl")
  library("dplyr")
  library("lubridate")
  library("ggplot2")
  library('scales')
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
factor_cevirme <- function(data) {
  data[] <- sapply(data, as.factor)
  return(data)
}

#' Title
#'
#' @param path_to_data
#'
#' @return us_data
#' @export
get_data <- function(path_to_data) {
  data <- readxl::read_xlsx(path_to_data, skip = 0)
  selected_data <- data %>% select(3,6,9,10,11,22)
  colnames(selected_data) <- c("order_Date", "customerID", "city", "state", "country", "profit")
  us_data <- selected_data %>% filter(country == "United States")
  return(us_data)
}

#' Title
#'
#' @param path_to_data
#'
#' @return us_data
#' @export
get_cof_data <- function(path_to_data) {
  data <- readxl::read_xlsx(path_to_data, skip = 0)
  us_data <- data %>% filter(country == "United States")
  us_data <- numeric_yap_cof(us_data)
  return(us_data)
}


#' Title
#'
#' @param data
#'
#' @return
#' @export
filter_data <- function(data) {
  missing_percentage <- sapply(data, function(x) {
    mean(is.na(x)) * 100
  })
  data_filtered <- data[, which(missing_percentage <= 50)]

  for (col in colnames(data_filtered)) {
    if (mean(is.na(data_filtered[[col]])) > 0) {
      data_filtered[[col]][is.na(data_filtered[[col]])] <- mean(data_filtered[[col]], na.rm = TRUE)
    }
  }
  return(data_filtered)
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
numeric_yap_cof <- function(data) {
  sapply(3:ncol(data), function(x) {
    data[[x]] <<- gsub("\\.","",data[[x]])
    data[[x]] <<- gsub(",",".",data[[x]])
    data[[x]] <<- as.numeric(data[[x]])
  })
  return(data)
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
calculate_average_for_cof <- function(data) {
  data <- data %>%
    rowwise() %>%
    mutate(average = mean(c_across(where(is.numeric)), na.rm = TRUE)) %>%
    ungroup() %>%
    select(city, country, average)
  print(data)
}


#' Title
#'
#' @param data
#'
#' @return
#' @export
char_to_date <- function(data) {
  data$order_Date <- dmy(data$order_Date)
  data <- data %>% filter(order_Date >= "2011-01-01")
  return(data)
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
usd_to_tl <- function(data) {
  dolar_kuru = 32
  data$profit <- sapply(data$profit, function(x) { x * dolar_kuru})
  return(data)

}

#' Title
#'
#' @param data
#'
#' @return
#' @export
total_profit_by_state_5 <- function(data) {
  data <- data %>% group_by(state) %>% summarise(total_profit = sum(profit)) %>% arrange(desc(total_profit))
  return(data[1:5,])
}


#' Title
#'
#' @param total_prof
#'
#' @return
#' @export
make_graphic <- function(total_prof) {
  ggplot2::ggplot(total_prof, aes(x = reorder(state, total_profit), y = total_profit)) +
    geom_bar(stat = "identity",aes(fill = state)) +
    labs(title = "Eyaletlerin Toplam Kar Değerleri", x = "Eyalet", y = "Kar Değeri") +
    theme_minimal() +
    scale_y_continuous(labels = comma)
}



#' Title
#'
#' @param data
#'
#' @return
#' @export
total_profit_by_city_asc <- function(data) {
  data <- data %>% group_by(city) %>% summarise(total_profit = sum(profit)) %>% arrange(total_profit)
  return(data[1:5,])
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
total_profit_by_city_desc <- function(data) {
  data <- data %>% group_by(city) %>% summarise(total_profit = sum(profit)) %>% arrange(desc(total_profit))
  return(data[1:6,])
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
calculate_percentage_of_total <- function(data) {
  state_profit = total_profit_by_state_5(data)
  total_profit_sum <- sum(state_profit$total_profit)
  state_profit <- state_profit %>%
    mutate(percentage_of_total = (total_profit / total_profit_sum) * 100)
}



#' Title
#'
#' @param profit
#' @param cost_of_living
#'
#' @return NA
#' @export
cof_profit_grafik <- function(profit,cost_of_living) {
  merged_data <- merge(cost_of_living, profit, by = "city")
  ggplot(merged_data, aes(x = city)) +
    geom_col(aes(y = total_profit), fill = "blue", alpha = 0.5) +
    geom_col(aes(y = average), color = "red",alpha = 0.5) +
    scale_y_continuous(sec.axis = sec_axis(~.*100, name = "Cost of Living", labels = comma)) +
    labs(y = "Total Profit", x = "City", labels= comma) +
    theme_minimal()
}



#' Title
#'
#'
#' @export
read_state_data <- function(file_path) {
  data <- read.csv(file_path, header = TRUE)

  names(data)[names(data) == "X...state"] <- "state"

  return(data)
}
