# 01_get_bfs.R
# Downloads the BFS hotel statistics (HESTA) through the PxWeb API.
#   - px-x-1003020000_102: arrivals and overnight stays by year, month, canton and
#                          visitors' country of residence (demand)
#   - px-x-1003020000_202: supply and demand by year, month and canton (capacity)
# Output: data/raw/bfs_demand.csv, data/raw/bfs_supply.csv

library(httr)
library(jsonlite)
library(dplyr)
library(readr)

years   <- as.character(2015:2025)        # panel is 2016-2025; 2015 only for "vs. last year" columns
months  <- as.character(1:12)
cantons <- c("8100", as.character(1:26))  # Switzerland total + 26 cantons

# Origins: all-origins total, 11 markets, and the six Gulf countries summed into "GCC"
origins <- c(
  "00",                                   # Visitors' country of residence - total
  "11", "13", "14", "16", "18", "15",     # near: DE, FR, IT, UK, NL, AT
  "41", "72", "62", "64", "76",           # far:  US, CN, IN, JP, KR
  "60", "68", "69", "81", "88", "89"      # BH, QA, KW, OM, SA, AE (reported individually since 2011)
)

sel <- function(code, values) list(code = code, selection = list(filter = "item", values = as.list(values)))

query_pxweb <- function(table, query) {
  url  <- sprintf("https://www.pxweb.bfs.admin.ch/api/v1/en/%s/%s.px", table, table)
  meta <- fromJSON(url, simplifyVector = FALSE)
  body <- list(query = query, response = list(format = "json"))
  res  <- POST(url, body = toJSON(body, auto_unbox = TRUE), content_type_json())
  stop_for_status(res)
  out  <- fromJSON(content(res, "text", encoding = "UTF-8"))

  # translate codes to labels using the table metadata
  labels <- lapply(meta$variables, function(v) setNames(unlist(v$valueTexts), unlist(v$values)))
  names(labels) <- sapply(meta$variables, `[[`, "code")

  keys <- do.call(rbind, out$data$key)
  colnames(keys) <- out$columns$code[out$columns$type != "c"]
  df <- as.data.frame(keys, stringsAsFactors = FALSE)
  for (v in colnames(df)) df[[paste0(v, "_label")]] <- unname(labels[[v]][df[[v]]])
  df$value <- suppressWarnings(as.numeric(unlist(out$data$values)))  # "..." -> NA
  df
}

# The API rejects large requests (HTTP 403), so we query one year at a time.
demand <- bind_rows(lapply(years, function(y) {
  Sys.sleep(1)
  query_pxweb("px-x-1003020000_102", list(
    sel("Jahr", y), sel("Monat", months), sel("Kanton", cantons),
    sel("Herkunftsland", origins), sel("Indikator", c("1", "2"))))
}))
write_csv(demand, "data/raw/bfs_demand.csv")
message("demand: ", nrow(demand), " rows")

# Supply: beds (3) and bed occupancy in % (8)
supply <- bind_rows(lapply(years, function(y) {
  Sys.sleep(1)
  query_pxweb("px-x-1003020000_202", list(
    sel("Jahr", y), sel("Monat", months), sel("Kanton", cantons), sel("Indikator", c("3", "8"))))
}))
write_csv(supply, "data/raw/bfs_supply.csv")
message("supply: ", nrow(supply), " rows")
