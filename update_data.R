# ============================================================
# 0050 投資組合研究
# 自動更新股票日資料
# ============================================================

library(tidyquant)
library(dplyr)
library(purrr)


# ============================================================
# 1. 股票清單
# ============================================================

# 0050 + 臺灣50指數50檔成分股
# 共 51 個資產

stock_symbols <- c(
  "0050",
  "2330", "2454", "2308", "2317", "3711",
  "2303", "2383", "3037", "2881", "2891",
  "1303", "3017", "2882", "2887", "2345",
  "2382", "2327", "2360", "2885", "2884",
  "2059", "2408", "6669", "2357", "2883",
  "3443", "2886", "2890", "3008", "2301",
  "3231", "2412", "2344", "3653", "2892",
  "2880", "3665", "4958", "1216", "2368",
  "6446", "2449", "7769", "2395", "5880",
  "8046", "2603", "4904", "3045", "6505"
)


# ============================================================
# 2. 基本檢查
# ============================================================

if (length(stock_symbols) != 51) {
  stop("股票清單不是51檔，請檢查 stock_symbols。")
}

if (length(unique(stock_symbols)) != 51) {
  stop("股票清單有重複股票代號。")
}


# Yahoo Finance 股票代號
yahoo_symbols <- paste0(stock_symbols, ".TW")


# ============================================================
# 3. 更新日期
# ============================================================

taiwan_today <- as.Date(
  format(Sys.time(), tz = "Asia/Taipei")
)

cat("\n")
cat("============================================\n")
cat("0050 投資組合資料更新\n")
cat("============================================\n")
cat("更新日期：", taiwan_today, "\n")
cat("資產數量：", length(stock_symbols), "\n")
cat("============================================\n\n")


# ============================================================
# 4. 下載單一股票
# ============================================================

download_stock <- function(symbol) {

  cat("下載：", symbol, "\n")

  data <- tryCatch(

    tq_get(
      symbol,
      get = "stock.prices",
      from = "2016-01-01",
      to = taiwan_today + 1,
      complete_cases = FALSE
    ),

    error = function(e) {

      cat(
        "下載失敗：",
        symbol,
        "\n"
      )

      NULL
    }
  )


  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }


  data %>%
    mutate(
      symbol = sub(
        "\\.TW$",
        "",
        symbol
      )
    ) %>%
    select(
      symbol,
      date,
      open,
      high,
      low,
      close,
      volume,
      adjusted
    )
}


# ============================================================
# 5. 下載全部股票
# ============================================================

new_data <- map_dfr(
  yahoo_symbols,
  download_stock
)


if (nrow(new_data) == 0) {
  stop("沒有取得任何股票資料，更新停止。")
}


cat("\n")
cat("本次下載完成\n")
cat("資料筆數：", nrow(new_data), "\n")
cat(
  "成功股票數：",
  length(unique(new_data$symbol)),
  "\n"
)


# ============================================================
# 6. 確認51檔股票都有下載到
# ============================================================

missing_symbols <- setdiff(
  stock_symbols,
  unique(new_data$symbol)
)


if (length(missing_symbols) > 0) {

  cat(
    "\n缺少股票：",
    paste(missing_symbols, collapse = ", "),
    "\n"
  )

  stop("部分股票下載失敗，更新停止。")
}


# ============================================================
# 7. 移除完全沒有價格資料的空白列
# ============================================================

new_data <- new_data %>%
  filter(!is.na(adjusted))


cat(
  "移除空白資料後筆數：",
  nrow(new_data),
  "\n"
)


# ============================================================
# 8. 讀取舊資料
# ============================================================

if (file.exists("stock_daily_v2.rds")) {

  old_data <- readRDS(
    "stock_daily_v2.rds"
  )

} else {

  old_data <- NULL
}


# ============================================================
# 9. 合併新舊資料
# ============================================================

if (!is.null(old_data)) {

  # 新資料放前面
  # 同一天重複時優先保留新資料

  combined_data <- bind_rows(
    new_data,
    old_data
  )

} else {

  combined_data <- new_data
}


# ============================================================
# 10. 整理資料
# ============================================================

combined_data <- combined_data %>%

  # 只保留目前51個資產
  filter(
    symbol %in% stock_symbols
  ) %>%

  # 只保留有 adjusted price 的資料
  filter(
    !is.na(adjusted)
  ) %>%

  # 每檔股票每天只保留一筆
  distinct(
    symbol,
    date,
    .keep_all = TRUE
  ) %>%

  arrange(
    symbol,
    date
  )


# ============================================================
# 11. 最終資料檢查
# ============================================================

actual_symbols <- sort(
  unique(combined_data$symbol)
)

expected_symbols <- sort(
  stock_symbols
)


missing_symbols <- setdiff(
  expected_symbols,
  actual_symbols
)

extra_symbols <- setdiff(
  actual_symbols,
  expected_symbols
)


if (length(missing_symbols) > 0) {

  stop(
    paste(
      "資料缺少股票：",
      paste(missing_symbols, collapse = ", ")
    )
  )
}


if (length(extra_symbols) > 0) {

  stop(
    paste(
      "資料多出股票：",
      paste(extra_symbols, collapse = ", ")
    )
  )
}


# ============================================================
# 12. 檢查重複資料
# ============================================================

duplicate_count <- combined_data %>%

  count(
    symbol,
    date
  ) %>%

  filter(
    n > 1
  ) %>%

  nrow()


if (duplicate_count > 0) {

  stop(
    paste(
      "發現",
      duplicate_count,
      "筆重複的 symbol + date 資料。"
    )
  )
}


# ============================================================
# 13. 檢查日期
# ============================================================

min_date <- min(
  combined_data$date,
  na.rm = TRUE
)

max_date <- max(
  combined_data$date,
  na.rm = TRUE
)


# ============================================================
# 14. 儲存資料
# ============================================================

saveRDS(
  combined_data,
  "stock_daily_v2.rds"
)


# ============================================================
# 15. 建立資料版本資訊
# ============================================================

writeLines(

  c(
    paste0(
      "latest_date=",
      format(max_date, "%Y-%m-%d")
    ),

    paste0(
      "updated_at=",
      format(
        Sys.time(),
        tz = "Asia/Taipei",
        usetz = TRUE
      )
    ),

    paste0(
      "rows=",
      nrow(combined_data)
    ),

    paste0(
      "symbols=",
      length(unique(combined_data$symbol))
    )
  ),

  "data_version.txt"
)


# ============================================================
# 16. 完成
# ============================================================

cat("\n")
cat("============================================\n")
cat("資料更新完成！\n")
cat("============================================\n")
cat("資料筆數：", nrow(combined_data), "\n")
cat("股票數量：", length(unique(combined_data$symbol)), "\n")
cat("最早日期：", min_date, "\n")
cat("最新日期：", max_date, "\n")
cat("3661 是否存在：", "3661" %in% combined_data$symbol, "\n")
cat("RDS：stock_daily_v2.rds\n")
cat("版本檔：data_version.txt\n")
cat("============================================\n")
