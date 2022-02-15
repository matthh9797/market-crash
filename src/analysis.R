library(plotly)
library(tidyquant)
library(tidyverse)
library(fs)

# Get current list of stocks in an index
get_stock_list <- function(stock_index = "SP500") {
  tq_index(stock_index) %>%
    select(symbol, company) %>%
    arrange(symbol) %>%
    mutate(label = str_c(symbol, company, sep = ", ")) %>%
    select(label)
}

# Get historic returns
get_returns <- function(ticker, frequency, from = Sys.Date() - 365, to = Sys.Date()) {
  ticker %>% 
    tq_get(get = "stock.prices",
           periodicity = frequency,
           from = from,
           to = to)%>% 
    tq_transmute(
      select = adjusted,
      mutate_fun = periodReturn,
      period = frequency,
      col_rename = "returns"
    )
}

# Get historic returns
get_price <- function(ticker, frequency, from = Sys.Date() - 365, to = Sys.Date()) {
  ticker %>% 
    tq_get(get = "stock.prices",
           periodicity = frequency,
           from = from,
           to = to)%>% 
    select(date, close) %>% 
    rename(price = close)
}

# S and P 500 
# EDA
(sp500_returns <- get_returns("^GSPC", "weekly", from="1980-01-01"))
(sp500_price <- get_price("^GSPC", "weekly", from="1980-01-01"))

sp500 <- left_join(sp500_price, 
                   sp500_returns,
                   by = c("date" = "date")) %>% 
  pivot_longer(
    cols = c(price, returns)
  )

crashes <- tribble(
  ~crash, ~from, ~to,
  "Black Monday", "1987-10-19", "1988-10-19",
  "Dot-com bubble", "2000-03-10", "2001-03-10",
  "Housing market", "2007-10-11", "2008-10-11",
  "Corona crash", "2020-02-24", "2021-02-24"
) %>% 
  mutate(
    from = ymd(from),
    to = ymd(to)
  )

dts <- crashes %>% select(crash, from)
text <- tibble(
  name = rep("returns", count(crashes)$n),
  value = rep(-0.25, count(crashes)$n)
) %>% 
  bind_cols(dts)

labs <- c(
  "Price" = "price",
  "Returns" = "returns"
)
  
# EDA
# S&P500 historic index with market crashes
g1a <- ggplot(sp500) +
  geom_rect(
    aes(xmin = from, xmax = to),
    ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red",
    data = crashes
    ) +
  annotate("rect", xmin=ymd('2021-11-01'), xmax=Sys.Date(), ymin=-Inf, ymax=Inf, alpha=0.2, fill="blue") +
  geom_vline(
    aes(xintercept = as.numeric(from)),
    data = crashes,
    colour = "grey50", alpha = 0.5
  ) +
  geom_text(
    aes(from, value, label = str_replace(crash, " ", "\n"), group = name),
    size = 3, vjust = 0, hjust = 0, nudge_x = 30,
    data = text
  ) +
  geom_line(aes(date, value)) +
  geom_smooth(aes(date, value), se=FALSE) +
  facet_wrap(vars(name),
             ncol = 1,
             scales = "free",
             labeller = labeller(name = str_to_title)) +
  labs(
    title = "Stock market crash 2022?",
    subtitle = "S & P 500 Index with historic crashes marked red"
  ) +
  theme_light() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

ggsave("assets/plots/sp500-historic.png", g1a, height = 8, width = 14)














