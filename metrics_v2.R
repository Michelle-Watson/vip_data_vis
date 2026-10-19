library(rsconnect)
library(dplyr)
library(lubridate)
library(readr)

# 1. Authenticate
rsconnect::setAccountInfo(
  name = 'evidencefoundation',
  token = '9C246635E3408005C2E8A60688D05C3C',
  secret = 'L8hTrH+noIQNCTU+yaWDARJIMmjHrkXmRfU5G244'
)

# 2. Configuration
app_name <- "vip_data_vis"
account <- "evidencefoundation"
server <- "shinyapps.io"

# Pull from SEPTEMBER 1 to today
start_date <- as.Date("2026-09-01")
end_date <- Sys.Date()
from_ts <- as.numeric(as.POSIXct(start_date))
until_ts <- as.numeric(as.POSIXct(paste0(end_date, " 23:59:59")))

metrics_dir <- "metrics"
if (!dir.exists(metrics_dir)) {
  dir.create(metrics_dir)
}
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# 3. Helper function to pull a metric
pull_metric <- function(series, names, label) {
  message("Pulling ", label, "...")
  df <- rsconnect::showMetrics(
    metricSeries = series,
    metricNames = names,
    appName = app_name,
    account = account,
    server = server,
    from = from_ts,
    until = until_ts,
    interval = "1m"
  )
  df$metric_label <- label
  df$pulled_at <- Sys.time()
  return(df)
}

# 4. Pull connection and system metrics
conn_raw <- pull_metric("container_status", "connect_count", "connections")
procs_raw <- pull_metric("container_status", "connect_procs", "worker_procs")
cpu_raw <- pull_metric("docker_container_cpu", "usage_in_usermode", "cpu_usage")
mem_raw <- pull_metric("docker_container_mem", "total_rss", "memory_rss")
net_raw <- pull_metric(
  "docker_container_net",
  c("tx_bytes", "rx_bytes"),
  "network_traffic"
)

# 5. Export raw minute-by-minute data
write_csv(
  conn_raw,
  file.path(metrics_dir, paste0("connections_raw_", timestamp, ".csv"))
)
write_csv(
  procs_raw,
  file.path(metrics_dir, paste0("worker_procs_raw_", timestamp, ".csv"))
)
write_csv(
  cpu_raw,
  file.path(metrics_dir, paste0("cpu_raw_", timestamp, ".csv"))
)
write_csv(
  mem_raw,
  file.path(metrics_dir, paste0("memory_raw_", timestamp, ".csv"))
)
write_csv(
  net_raw,
  file.path(metrics_dir, paste0("network_raw_", timestamp, ".csv"))
)

# 6. Build daily summaries
conn_daily <- conn_raw %>%
  mutate(date = as.Date(time)) %>%
  group_by(date) %>%
  summarise(
    peak_concurrent_users = max(connect_count, na.rm = TRUE),

    total_connection_minutes = sum(connect_count, na.rm = TRUE),
    total_connection_hours = round(sum(connect_count, na.rm = TRUE) / 60, 1),

    active_app_usage_minutes = sum(connect_count > 0, na.rm = TRUE),
    active_app_usage_hours = round(
      sum(connect_count > 0, na.rm = TRUE) / 60,
      1
    ),

    .groups = "drop"
  )

sys_daily <- bind_rows(
  procs_raw %>%
    mutate(date = as.Date(time)) %>%
    group_by(date) %>%
    summarise(
      mean_worker_procs = mean(connect_procs, na.rm = TRUE),
      max_worker_procs = max(connect_procs, na.rm = TRUE),
      .groups = "drop"
    ),
  cpu_raw %>%
    mutate(date = as.Date(time)) %>%
    group_by(date) %>%
    summarise(
      mean_cpu = mean(usage_in_usermode, na.rm = TRUE),
      max_cpu = max(usage_in_usermode, na.rm = TRUE),
      .groups = "drop"
    ),
  mem_raw %>%
    mutate(date = as.Date(time)) %>%
    group_by(date) %>%
    summarise(
      mean_mem = mean(total_rss, na.rm = TRUE),
      max_mem = max(total_rss, na.rm = TRUE),
      .groups = "drop"
    )
)

master_daily <- conn_daily %>% left_join(sys_daily, by = "date")

# 7. Export daily summaries
write_csv(
  conn_daily,
  file.path(metrics_dir, paste0("connections_daily_", timestamp, ".csv"))
)
write_csv(
  master_daily,
  file.path(metrics_dir, paste0("master_daily_summary_", timestamp, ".csv"))
)

message("✅ All metrics exported successfully to the 'metrics/' folder.")
View(master_daily)


# --- GRAND TOTALS SINCE LAUNCH ---
total_minutes_all_time <- sum(conn_raw$connect_count, na.rm = TRUE)
total_hours_all_time <- round(total_minutes_all_time / 60, 1)

cat("\n======================================================\n")
cat("GRAND TOTAL SINCE LAUNCH\n")
cat(
  "Total Connection Minutes:",
  format(total_minutes_all_time, big.mark = ","),
  "\n"
)
cat(
  "Total Connection Hours:  ",
  format(total_hours_all_time, big.mark = ","),
  "\n"
)
cat("======================================================\n")
