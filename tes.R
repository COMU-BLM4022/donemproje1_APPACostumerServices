#' Title
#'
#' @return
#' @export
#'
#' @examples
get_data <- function() {
  library("readxl")
  library("dplyr")
  library("lubridate")
  library("ggplot2")
  library('scales')
  
  data <- readxl::read_xlsx("./data/data.xlsx", skip = 0)
  selected_data <- data %>% select(3,6,9,10,11,22)
  colnames(selected_data) <- c("Order_Date", "CustomerID", "City", "State", "Country", "Profit")
  us_data <- selected_data %>% filter(Country == "United States")
  sapply(1:6,function(x) {
    us_data[[x]] <- as.factor(us_data[[x]])
  })
  return(us_data)
}


char_to_date <- function(data) {
  data$Order_Date <- dmy(data$Order_Date)
  data <- data %>% filter(Order_Date >= "2011-01-01")
  return(data)
}

usd_to_tl <- function(data) {
  dolar_kuru = 32
  data$Profit <- sapply(data$Profit, function(x) { x * dolar_kuru})
  return(data)
}

total_profit_by_state <- function(data) {
  data <- data %>% group_by(State) %>% summarise(total_profit = sum(Profit)) %>% arrange(desc(total_profit))
  return(data)
}

data = get_data()
data = char_to_date(data)
data = usd_to_tl(data)
total_prof = total_profit_by_state(data)[1:5,]

make_graphic <- function(total_prof) {
  ggplot(total_prof, aes(x = reorder(State, total_profit), y = total_profit)) +
    geom_bar(stat = "identity",aes(fill = State)) +
    labs(title = "Total Profit by State", x = "State", y = "Total Profit") +
    theme_minimal() +
    scale_y_continuous(labels = comma) 
}
make_graphic(total_prof)


total_profit_by_city_asc <- function(data) {
  data <- data %>% group_by(City) %>% summarise(total_profit = sum(Profit)) %>% arrange(total_profit)
  return(data[1:5,])
}

calculate_percentage_of_total <- function(data) {
  state_profit = total_profit_by_state(data)
  total_profit_sum <- sum(state_profit$total_profit)
  state_profit <- state_profit %>%
    mutate(percentage_of_total = (total_profit / total_profit_sum) * 100)
  print(state_profit)
}

calculate_percentage_of_total(data)
