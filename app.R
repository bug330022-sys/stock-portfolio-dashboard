
library(shiny)
library(ggplot2)
library(dplyr)
# ============================================================
# 1. 股票名稱與產業分類
# ============================================================

stock_info <- data.frame(
  
  symbol = c(
    "2330", "2454", "2308", "2317", "3711",
    "2303", "2383", "3037", "2881", "2891",
    "1303", "3017", "2882", "2887", "2345",
    "2382", "2327", "2360", "2885", "2884",
    "2059", "2408", "6669", "2357", "2883",
    "2886", "3008", "2890", "3443", "3231",
    "2301", "2344", "2412", "3653", "2880",
    "2892", "3665", "1216", "4958", "6446",
    "7769", "2368", "2449", "2395", "5880",
    "8046", "2603", "4904", "3045", "6505"
  ),
  
  name = c(
    "台積電", "聯發科", "台達電", "鴻海", "日月光投控",
    "聯電", "台光電", "欣興", "富邦金", "中信金",
    "南亞", "奇鋐", "國泰金", "台新新光金", "智邦",
    "廣達", "國巨", "致茂", "元大金", "玉山金",
    "川湖", "南亞科", "緯穎", "華碩", "凱基金",
    "兆豐金", "大立光", "永豐金", "創意", "緯創",
    "光寶科", "華邦電", "中華電", "健策", "華南金",
    "第一金", "貿聯-KY", "統一", "臻鼎-KY", "藥華藥",
    "鴻勁", "金像電", "京元電子", "研華", "合庫金",
    "南電", "長榮", "遠傳", "台灣大", "台塑化"
  ),
  
  industry = c(
    "半導體業",             # 2330 台積電
    "半導體業",             # 2454 聯發科
    "電子零組件業",         # 2308 台達電
    "其他電子業",           # 2317 鴻海
    "半導體業",             # 3711 日月光投控
    "半導體業",             # 2303 聯電
    "電子零組件業",         # 2383 台光電
    "電子零組件業",         # 3037 欣興
    "金融保險",             # 2881 富邦金
    "金融保險",             # 2891 中信金
    "塑膠工業",             # 1303 南亞
    "電腦及週邊設備業",     # 3017 奇鋐
    "金融保險",             # 2882 國泰金
    "金融保險",             # 2887 台新新光金
    "通信網路業",           # 2345 智邦
    "電腦及週邊設備業",     # 2382 廣達
    "電子零組件業",         # 2327 國巨
    "其他電子業",           # 2360 致茂
    "金融保險",             # 2885 元大金
    "金融保險",             # 2884 玉山金
    "電子零組件業",         # 2059 川湖
    "半導體業",             # 2408 南亞科
    "電腦及週邊設備業",     # 6669 緯穎
    "電腦及週邊設備業",     # 2357 華碩
    "金融保險",             # 2883 凱基金
    "金融保險",             # 2886 兆豐金
    "其他電子業",           # 3008 大立光
    "金融保險",             # 2890 永豐金
    "半導體業",             # 3443 創意
    "電腦及週邊設備業",     # 3231 緯創
    "電腦及週邊設備業",     # 2301 光寶科
    "半導體業",             # 2344 華邦電
    "通信網路業",           # 2412 中華電
    "電子零組件業",         # 3653 健策
    "金融保險",             # 2880 華南金
    "金融保險",             # 2892 第一金
    "其他電子業",           # 3665 貿聯-KY
    "食品工業",             # 1216 統一
    "電子零組件業",         # 4958 臻鼎-KY
    "生技醫療業",           # 6446 藥華藥
    "半導體業",             # 7769 鴻勁
    "電子零組件業",         # 2368 金像電
    "半導體業",             # 2449 京元電子
    "電腦及週邊設備業",     # 2395 研華
    "金融保險",             # 5880 合庫金
    "電子零組件業",         # 8046 南電
    "航運業",               # 2603 長榮
    "通信網路業",           # 4904 遠傳
    "通信網路業",           # 3045 台灣大
    "油電燃氣業"            # 6505 台塑化
  ),
  
  stringsAsFactors = FALSE
)


# ============================================================
# 2. 初始資料
#    只用來設定日期選擇器的範圍
# ============================================================

initial_data <- readRDS("stock_daily_v2.rds")

data_min_date <- min(initial_data$date)
data_max_date <- max(initial_data$date)

# ============================================================
# 固定研究期間
# ============================================================

research_start_date <- as.Date(
  "2016-01-04"
)

research_end_date <- as.Date(
  "2025-12-31"
)

# ============================================================
# 3. UI
# ============================================================

ui <- fluidPage(
  
  # ==========================================================
  # 1. 全站樣式
  # ==========================================================
  
  tags$style(HTML("
    
  body {
  background-color: #f3f6f9 !important;
  font-family: 'Microsoft JhengHei', sans-serif;
  color: #1f2937;
}
    
.dashboard-title {
  margin-top: 25px;
  margin-bottom: 6px;

  color: #111827;

  font-size: 30px;
  font-weight: 700;
  letter-spacing: 0.5px;
}
    
   .data-status {
  background-color: #ffffff;

  border: 1px solid #e1e5ea;
  border-radius: 10px;

  padding: 18px 20px;
  margin-bottom: 20px;

  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.04);
}
    
   .section-box {
  background-color: #ffffff;

  border: 1px solid #e1e5ea;
  border-radius: 10px;

  padding: 22px 24px;
  margin-bottom: 20px;

  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.04);
}
   
       /* ======================================================
       桌機版內容寬度
       ====================================================== */

    .container-fluid {
      max-width: 1400px;
      margin-left: auto;
      margin-right: auto;
    }
    
    
    .page-description {
      color: #666666;
      line-height: 1.7;
      margin-bottom: 18px;
    }
    
    .control-description {
      color: #666666;
      font-size: 14px;
      margin-top: 5px;
      margin-bottom: 12px;
    }
    
    .nav-tabs {
      margin-bottom: 20px;
    }
    
    #return_summary {
      overflow-x: auto;
      width: 100%;
    }
    /* ======================================================
       51 檔最新行情監控表
       ====================================================== */

    #realtime_monitor_table {
      width: 100%;
      overflow-x: auto;
    }

    #realtime_monitor_table table {
      width: 100%;
      min-width: 850px;
      border-collapse: collapse;
      background-color: #ffffff;
    }

    #realtime_monitor_table th {
      background-color: #f1f3f5;
      color: #343a40;

      font-weight: 700;
      text-align: center;
      white-space: nowrap;

      padding: 12px 14px;

      border-bottom: 2px solid #d9dee5;
    }

    #realtime_monitor_table td {
      text-align: center;
      white-space: nowrap;

      padding: 11px 14px;

      border-bottom: 1px solid #eceff2;
    }

    #realtime_monitor_table tbody tr:hover {
      background-color: #f8f9fa;
    }

    /* 代號 */

    #realtime_monitor_table th:nth-child(1),
    #realtime_monitor_table td:nth-child(1) {
      min-width: 70px;
      font-weight: 600;
    }

    /* 名稱 */

    #realtime_monitor_table th:nth-child(2),
    #realtime_monitor_table td:nth-child(2) {
      min-width: 100px;
      text-align: left;
    }

    /* 日期 */

    #realtime_monitor_table th:nth-child(3),
    #realtime_monitor_table td:nth-child(3) {
      min-width: 105px;
    }

    /* 收盤價 */

    #realtime_monitor_table th:nth-child(4),
    #realtime_monitor_table td:nth-child(4) {
      min-width: 95px;
    }

    /* 漲跌幅 */

    #realtime_monitor_table th:nth-child(5),
    #realtime_monitor_table td:nth-child(5) {
      min-width: 110px;
      font-weight: 600;
    }

    /* 成交量 */

    #realtime_monitor_table th:nth-child(6),
    #realtime_monitor_table td:nth-child(6) {
      min-width: 115px;
    }

    /* 近20日成交量 */

    #realtime_monitor_table th:nth-child(7),
    #realtime_monitor_table td:nth-child(7) {
      min-width: 115px;
      font-weight: 600;
    }
    #return_summary table {
      white-space: nowrap;
      width: max-content;
      min-width: 100%;
    }

    #return_summary th {
      white-space: nowrap;
      padding: 10px 14px;
      text-align: center;
    }

    #return_summary td {
      white-space: nowrap;
      padding: 9px 14px;
      text-align: center;
    }

    #return_summary th:nth-child(1),
    #return_summary td:nth-child(1) {
      min-width: 75px;
    }

    #return_summary th:nth-child(2),
    #return_summary td:nth-child(2) {
      min-width: 110px;
    }

    #return_summary th:nth-child(12),
    #return_summary td:nth-child(12),
    #return_summary th:nth-child(13),
    #return_summary td:nth-child(13) {
      min-width: 110px;
    }
    
    
    /* ======================================================
   Dashboard 右側內容美編
   ====================================================== */

/* ------------------------------------------------------
   右側主要內容區
   ------------------------------------------------------ */

.dashboard-tabs .tab-content {
  background: transparent;
  padding: 0 0 30px 0;
}

.dashboard-tabs .tab-content > .tab-pane {
  padding: 0;
}

/* ------------------------------------------------------
   右側主卡片
   ------------------------------------------------------ */

.dashboard-tabs .section-box {
  background: #ffffff;
  border: 1px solid #dce3ea;
  border-radius: 16px;
  padding: 30px 34px 40px 34px;
  margin-bottom: 24px;
  box-shadow: 0 4px 18px rgba(51, 65, 85, 0.06);
}

/* ------------------------------------------------------
   頁面主標題
   ------------------------------------------------------ */

.dashboard-tabs .section-box > h2 {
  color: #243447;
  font-size: 27px;
  font-weight: 700;
  margin-top: 0;
  margin-bottom: 10px;
  letter-spacing: 0.3px;
}

/* ------------------------------------------------------
   區塊標題
   ------------------------------------------------------ */

.dashboard-tabs .section-box h3 {
  color: #334e68;
  font-size: 20px;
  font-weight: 650;
  margin-top: 28px;
  margin-bottom: 10px;
  padding-left: 11px;
  border-left: 4px solid #6f8fae;
}

/* ------------------------------------------------------
   次標題
   ------------------------------------------------------ */

.dashboard-tabs .section-box h4 {
  color: #486581;
  font-size: 16px;
  font-weight: 600;
  margin-top: 18px;
  margin-bottom: 8px;
}

/* ------------------------------------------------------
   頁面說明文字
   ------------------------------------------------------ */

.dashboard-tabs .page-description {
  color: #526777;
  font-size: 15px;
  line-height: 1.75;
  margin-bottom: 24px;
}

/* ------------------------------------------------------
   一般說明文字
   ------------------------------------------------------ */

.dashboard-tabs .section-box p {
  color: #52606d;
  line-height: 1.7;
}

.dashboard-tabs .section-box .control-description {
  color: #7b8794;
  font-size: 13px;
  line-height: 1.7;
}

/* ------------------------------------------------------
   水平分隔線
   ------------------------------------------------------ */

.dashboard-tabs .section-box hr {
  border: 0;
  border-top: 1px solid #e7edf2;
  margin: 28px 0;
}

/* ------------------------------------------------------
   日期與選擇器區域
   ------------------------------------------------------ */

.dashboard-tabs .form-group {
  margin-bottom: 18px;
}

.dashboard-tabs .form-group > label {
  color: #334e68;
  font-weight: 600;
  margin-bottom: 7px;
}

/* ------------------------------------------------------
   Date Input / Date Range Input
   ------------------------------------------------------ */

.dashboard-tabs .form-control {
  border: 1px solid #d4dde6;
  border-radius: 8px;
  box-shadow: none;
  color: #334e68;
  background-color: #fbfcfd;
}

.dashboard-tabs .form-control:focus {
  border-color: #7897b5;
  box-shadow: 0 0 0 3px rgba(120, 151, 181, 0.12);
}

/* ------------------------------------------------------
   Selectize
   ------------------------------------------------------ */

.dashboard-tabs .selectize-input {
  border: 1px solid #d4dde6;
  border-radius: 8px;
  box-shadow: none;
  min-height: 40px;
  padding: 9px 12px;
  background: #fbfcfd;
}

.dashboard-tabs .selectize-input.focus {
  border-color: #7897b5;
  box-shadow: 0 0 0 3px rgba(120, 151, 181, 0.12);
}

/* ------------------------------------------------------
   按鈕
   ------------------------------------------------------ */

.dashboard-tabs .btn {
  border-radius: 8px;
  border: 1px solid #cbd5df;
  background: #f8fafc;
  color: #3e566b;
  font-weight: 600;
  padding: 7px 14px;
  margin-right: 5px;
  margin-bottom: 5px;
  transition: all 0.15s ease;
}

.dashboard-tabs .btn:hover {
  background: #eef3f7;
  border-color: #aebdca;
  color: #2f4858;
}

.dashboard-tabs .btn:focus {
  box-shadow: 0 0 0 3px rgba(120, 151, 181, 0.12);
}

/* ------------------------------------------------------
   Checkbox
   ------------------------------------------------------ */

.dashboard-tabs .checkbox {
  margin-top: 10px;
}

.dashboard-tabs .checkbox label {
  color: #4b6072;
  font-weight: 500;
}

/* ------------------------------------------------------
   表格整體
   ------------------------------------------------------ */

.dashboard-tabs table {
  border-collapse: separate;
  border-spacing: 0;
  background: #ffffff;
}

/* ------------------------------------------------------
   表頭
   ------------------------------------------------------ */

.dashboard-tabs table thead th {
  background: #edf2f6;
  color: #34495e;
  font-weight: 650;
  border-bottom: 1px solid #d8e0e7;
  padding: 10px 12px;
  white-space: nowrap;
}

/* ------------------------------------------------------
   表格內容
   ------------------------------------------------------ */

.dashboard-tabs table tbody td {
  color: #4b5d6b;
  padding: 9px 12px;
  border-bottom: 1px solid #edf1f4;
  vertical-align: middle;
}

/* ------------------------------------------------------
   表格 hover
   ------------------------------------------------------ */

.dashboard-tabs table tbody tr:hover {
  background: #f7f9fb;
}

/* ------------------------------------------------------
   表格偶數列
   ------------------------------------------------------ */

.dashboard-tabs table tbody tr:nth-child(even) {
  background: #fbfcfd;
}

/* ------------------------------------------------------
   圖表區
   ------------------------------------------------------ */

.dashboard-tabs .shiny-plot-output {
  background: #ffffff;
  border-radius: 10px;
  padding: 6px;
  margin-top: 8px;
  margin-bottom: 12px;
}

/* ------------------------------------------------------
   圖表與文字之間增加呼吸感
   ------------------------------------------------------ */

.dashboard-tabs .section-box .plotly,
.dashboard-tabs .section-box .shiny-plot-output {
  margin-top: 12px;
}

/* ------------------------------------------------------
   資料狀態區
   ------------------------------------------------------ */

.dashboard-tabs .data-status {
  background: #f6f8fa;
  border: 1px solid #e0e7ed;
  border-radius: 12px;
  padding: 18px 20px;
  margin-bottom: 22px;
}

.dashboard-tabs .data-status h3 {
  border-left: none;
  padding-left: 0;
  margin-top: 0;
}

/* ------------------------------------------------------
   fluidRow 欄位間距
   ------------------------------------------------------ */

.dashboard-tabs .row {
  margin-bottom: 6px;
}

/* ------------------------------------------------------
   累積報酬顯示
   ------------------------------------------------------ */

.dashboard-tabs #cumulative_return_display {
  background: #f8fafc;
  border: 1px solid #e2e8ee;
  border-radius: 10px;
  padding: 14px 18px;
  margin: 8px 0 12px 0;
}

.dashboard-tabs #cumulative_return_display strong {
  color: #334e68;
}


/* ------------------------------------------------------
   Responsive
   ------------------------------------------------------ */

@media (max-width: 1100px) {
  
  .dashboard-tabs .section-box {
    padding: 24px 24px 32px 24px;
  }
  
}

@media (max-width: 768px) {
  
  .dashboard-tabs .section-box {
    padding: 20px 16px 28px 16px;
    border-radius: 12px;
  }
  
  .dashboard-tabs .section-box > h2 {
    font-size: 23px;
  }
  
  .dashboard-tabs .section-box h3 {
    font-size: 18px;
  }
  
  .dashboard-tabs .section-box h4 {
    font-size: 15px;
  }
  
  .dashboard-tabs .page-description {
    font-size: 14px;
  }
  
}
    
    
    /* ======================================================
   投資組合交易明細表
   ====================================================== */

#portfolio_trade_table {
  width: 100%;
  max-width: 100%;
  overflow-x: auto;
  display: block;
}

#portfolio_trade_table table {
  width: max-content;
  min-width: 1600px;
  white-space: nowrap;
}

#portfolio_trade_table th,
#portfolio_trade_table td {
  white-space: nowrap;
  padding: 8px 10px;
  font-size: 13px;
}

    /* ======================================================
       Dashboard 左側導覽列
       ====================================================== */

    .dashboard-tabs .tabbable {
      display: flex;
      align-items: flex-start;
      gap: 24px;
    }

    /* ------------------------------------------------------
       左側導覽
       ------------------------------------------------------ */

    .dashboard-tabs .nav-tabs {
      flex: 0 0 220px;
      width: 220px;

      margin: 0;
      padding: 8px;

      background-color: #334155;

      border: none;
      border-radius: 12px;

      box-shadow: 0 3px 12px rgba(0, 0, 0, 0.08);
    }

    .dashboard-tabs .nav-tabs > li {
      float: none;
      width: 100%;
      margin-bottom: 5px;
    }

    .dashboard-tabs .nav-tabs > li:last-child {
      margin-bottom: 0;
    }

    .dashboard-tabs .nav-tabs > li > a {
      margin: 0;
      padding: 12px 15px;

      color: #e5e7eb;
      background-color: transparent;

      border: none;
      border-radius: 8px;

      font-size: 15px;
      font-weight: 500;
      text-align: left;

      transition: all 0.2s ease;
    }

    .dashboard-tabs .nav-tabs > li > a:hover {
      color: #ffffff;
      background-color: #475569;
    }

    .dashboard-tabs .nav-tabs > li.active > a,
    .dashboard-tabs .nav-tabs > li.active > a:hover,
    .dashboard-tabs .nav-tabs > li.active > a:focus {
      color: #ffffff;
      background-color: #6286a8;

      border: none;
      border-radius: 8px;

      font-weight: 700;
    }

    /* ------------------------------------------------------
       右側主內容
       ------------------------------------------------------ */

    .dashboard-tabs .tab-content {
      flex: 1;
      min-width: 0;
    }

    .dashboard-tabs .tab-pane {
      width: 100%;
    }

    /* ------------------------------------------------------
       右側內容卡片
       ------------------------------------------------------ */

    .section-box {
      background-color: #ffffff;

      padding: 28px 30px;
      margin-bottom: 24px;

      border: 1px solid #e1e7ee;
      border-radius: 12px;

      box-shadow: 0 2px 8px rgba(0, 0, 0, 0.04);
    }

    .section-box h2 {
      margin-top: 0;
      margin-bottom: 10px;

      color: #26364a;
      font-size: 26px;
      font-weight: 700;
    }

    .section-box h3 {
      margin-top: 22px;
      margin-bottom: 14px;

      color: #3b4d62;
      font-size: 19px;
      font-weight: 700;
    }

    .section-box h4 {
      color: #64748b;
      font-weight: 600;
    }

    /* ------------------------------------------------------
       右側說明文字
       ------------------------------------------------------ */

    .page-description {
      color: #6b7280;
      line-height: 1.7;
      margin-bottom: 20px;
    }

    .control-description {
      color: #7b8794;
      font-size: 14px;
      line-height: 1.7;
      margin-top: 5px;
      margin-bottom: 12px;
    }

    /* ------------------------------------------------------
       分隔線
       ------------------------------------------------------ */

    .section-box hr {
      border: 0;
      border-top: 1px solid #e6ebf0;
      margin: 26px 0;
    }

    /* ------------------------------------------------------
       輸入框
       ------------------------------------------------------ */

    .section-box .form-control {
      min-height: 40px;

      border: 1px solid #d6dee7;
      border-radius: 8px;

      box-shadow: none;
    }

    .section-box .form-control:focus {
      border-color: #87a3bf;
      box-shadow: 0 0 0 2px rgba(98, 134, 168, 0.10);
    }

    /* ------------------------------------------------------
       Selectize
       ------------------------------------------------------ */

    .section-box .selectize-input {
      min-height: 40px;

      border: 1px solid #d6dee7;
      border-radius: 8px;

      box-shadow: none;
    }

    .section-box .selectize-input.focus {
      border-color: #87a3bf;
      box-shadow: 0 0 0 2px rgba(98, 134, 168, 0.10);
    }

    /* ------------------------------------------------------
       按鈕
       ------------------------------------------------------ */

    .section-box .btn {
      border-radius: 7px;

      border: 1px solid #cbd5df;

      background-color: #f8fafc;
      color: #475569;

      font-weight: 500;

      transition: all 0.2s ease;
    }

    .section-box .btn:hover {
      background-color: #e9eef3;
      border-color: #b9c6d3;
    }

    .section-box .btn-primary {
      background-color: #6286a8;
      border-color: #6286a8;
      color: #ffffff;
    }

    .section-box .btn-primary:hover {
      background-color: #557894;
      border-color: #557894;
    }

    /* ------------------------------------------------------
       右側表格
       ------------------------------------------------------ */

    .section-box table {
      width: 100%;

      border-collapse: separate;
      border-spacing: 0;

      border: 1px solid #e1e7ee;
      border-radius: 8px;

      overflow: hidden;
    }

    .section-box table thead th {
      background-color: #eef3f7;

      color: #475569;

      font-weight: 700;

      border-bottom: 1px solid #dbe3eb;
    }

    .section-box table tbody td {
      background-color: #ffffff;
      color: #4b5563;

      border-top: 1px solid #edf1f5;
    }

    .section-box table tbody tr:hover td {
      background-color: #f7f9fb;
    }

    /* ------------------------------------------------------
       資料狀態卡片
       ------------------------------------------------------ */

    .data-status {
      background-color: #ffffff;

      padding: 22px 26px;
      margin-bottom: 22px;

      border: 1px solid #e1e7ee;
      border-radius: 12px;

      box-shadow: 0 2px 8px rgba(0, 0, 0, 0.04);
    }

    .data-status h3 {
      margin-top: 0;
      color: #3b4d62;
    }

    /* ------------------------------------------------------
       圖表
       ------------------------------------------------------ */

    .section-box .shiny-plot-output {
      margin-top: 10px;
    }

    /* ------------------------------------------------------
       較窄螢幕
       ------------------------------------------------------ */

    @media (max-width: 900px) {

      .dashboard-tabs .tabbable {
        display: block;
      }

      .dashboard-tabs .nav-tabs {
        width: 100%;
        margin-bottom: 20px;

        display: flex;
        flex-wrap: wrap;
      }

      .dashboard-tabs .nav-tabs > li {
        width: auto;
        margin-right: 5px;
        margin-bottom: 0;
      }

      .dashboard-tabs .nav-tabs > li > a {
        text-align: center;
      }

      .section-box {
        padding: 22px 20px;
      }

    }
    
  ")),
  
  
  # ==========================================================
  # 2. 網站標題
  # ==========================================================
  
  h1(
    class = "dashboard-title",
    "0050 投資組合研究 監測儀錶板 v2"
  ),
  
  p(
    class = "page-description",
    "整合 0050 成分股之價格、報酬、風險、產業與投資組合分析。"
  ),
  
  
  
  
  # ==========================================================
  # 5. 分頁
  # ==========================================================
  div(
    
    class = "dashboard-tabs",
    
    tabsetPanel(
      
      id = "main_tabs",
      
      type = "tabs",
      
      # ========================================================
      # 分頁 1：市場總覽
      # ========================================================
      
      tabPanel(
        
        "市場總覽",
        
        div(
          class = "section-box",
          
          h2("市場總覽"),
          
          p(
            class = "page-description",
            "掌握目前市場資料狀態、0050 主要市場指標，以及成分股整體表現與歷史走勢。"
          ),
          
          # ----------------------------------------------------
          # 1. 資料狀態
          # ----------------------------------------------------
          
          h3("資料狀態"),
          
          fluidRow(
            
            column(
              width = 3,
              
              h4("股票數量"),
              h3(
                textOutput(
                  "n_stock",
                  inline = TRUE
                )
              )
              
            ),
            
            column(
              width = 3,
              
              h4("資料筆數"),
              h3(
                textOutput(
                  "n_data",
                  inline = TRUE
                )
              )
              
            ),
            
            column(
              width = 3,
              
              h4("最早資料日"),
              h4(
                textOutput(
                  "min_date",
                  inline = TRUE
                )
              )
              
            ),
            
            column(
              width = 3,
              
              h4("最新交易日"),
              h4(
                textOutput(
                  "max_date",
                  inline = TRUE
                )
              )
              
            )
            
          ),
          
          p(
            class = "control-description",
            "資料每 60 秒檢查一次是否有新版本。網站顯示的最新交易日為資料來源目前可取得的最新有效交易日；若當日資料尚未完成更新，則維持前一個有效交易日的資料。"
          ),
          
          p(
            "資料更新時間：",
            textOutput(
              "market_data_update_time",
              inline = TRUE
            )
          ),
          
          hr(),
          
          # ----------------------------------------------------
          # 2. 市場觀察日期
          # ----------------------------------------------------
          
          h3("市場觀察日期"),
          
          p(
            class = "control-description",
            "選擇特定交易日後，以下 0050 市場概況與成分股漲跌資訊會依該日期重新計算。"
          ),
          
          dateInput(
            
            inputId = "market_observation_date",
            
            label = "請選擇市場觀察日期：",
            
            value = data_max_date,
            
            min = data_min_date,
            
            max = data_max_date,
            
            format = "yyyy-mm-dd",
            
            language = "zh-TW",
            
            weekstart = 1,
            
            datesdisabled = setdiff(
              seq(
                data_min_date,
                data_max_date,
                by = "day"
              ),
              sort(
                unique(
                  initial_data$date
                )
              )
            ),
            
            daysofweekdisabled = c(0, 6)
            
          ),
          
          hr(),
          
          # ----------------------------------------------------
          # 3. 0050 與市場概況
          # ----------------------------------------------------
          
          h3("0050 與市場概況"),
          
          fluidRow(
            
            column(
              width = 3,
              
              h4(
                tags$span(
                  "0050 最新調整後價格",
                  title = "指定市場觀察日期的 0050 調整後收盤價。調整後價格適合用於長期價格與報酬比較。"
                )
              ),
              
              h3(
                textOutput(
                  "market_0050_price",
                  inline = TRUE
                )
              )
              
            ),
            
            column(
              width = 3,
              
              h4(
                tags$span(
                  "0050 最新日報酬率",
                  title = "0050 當日調整後價格相較前一交易日的報酬率。"
                )
              ),
              
              h3(
                textOutput(
                  "market_0050_daily_return",
                  inline = TRUE
                )
              )
              
            ),
            
            column(
              width = 3,
              
              h4(
                tags$span(
                  "0050 YTD 報酬率",
                  title = "YTD（Year-to-Date）表示由當年度第一個有效交易日至目前市場觀察日期的累積報酬率。"
                )
              ),
              
              h3(
                textOutput(
                  "market_0050_ytd",
                  inline = TRUE
                )
              )
              
            ),
            
            column(
              width = 3,
              
              h4(
                tags$span(
                  "成分股漲跌",
                  title = "統計 50 檔 0050 成分股於指定市場觀察日期相較前一交易日的上漲、下跌與持平家數。無有效資料者不列入統計。"
                )
              ),
              
              h3(
                textOutput(
                  "market_breadth",
                  inline = TRUE
                )
              )
              
            )
            
          ),
          
          p(
            "成分股漲跌的呈現順序為：上漲／下跌／持平。"
          ),
          
          hr(),
          
          # ----------------------------------------------------
          # 4. 0050 歷史價格時間序列
          # ----------------------------------------------------
          
          h3("0050 調整後收盤價歷史走勢"),
          
          p(
            "以下圖形預設顯示目前可取得的完整資料期間；使用者也可以自行調整圖表時間範圍。"
          ),
          
          dateRangeInput(
            
            inputId = "market_chart_date_range",
            
            label = "圖表時間範圍：",
            
            start = data_min_date,
            
            end = data_max_date,
            
            min = data_min_date,
            
            max = data_max_date,
            
            format = "yyyy-mm-dd",
            
            separator = " 至 "
            
          ),
          
          plotOutput(
            "market_0050_price_chart",
            height = "550px"
          ),
          
          hr(),
          
          # ----------------------------------------------------
          # 5. 0050 近一年每日報酬率
          # ----------------------------------------------------
          
          h3("0050 近一年每日報酬率"),
          
          p(
            "此圖呈現市場觀察日期往前一年的 0050 每日報酬率。Y 軸為單日報酬率；大於 0% 代表當日上漲，小於 0% 代表當日下跌，距離 0% 越遠表示當日價格變動幅度越大。"
          ),
          
          plotOutput(
            "market_0050_daily_return_timeseries",
            height = "500px"
          ),
          
          hr(),
          
          # ----------------------------------------------------
          # 6. 近 30 個交易日成分股漲跌幅排名
          # ----------------------------------------------------
          
          h3("近 30 個交易日漲跌幅：漲幅前五名與跌幅前五名"),
          
          p(
            "以市場觀察日期為基準，計算 50 檔 0050 成分股近 30 個交易日的累積漲跌幅，並呈現漲幅最高的 5 檔與跌幅最大的 5 檔。"
          ),
          
          p(
            class = "control-description",
            "橫軸為近 30 個交易日累積漲跌幅。向右越遠代表漲幅越高，向左越遠代表跌幅越大；0% 表示期間價格大致沒有變化。"
          ),
          
          plotOutput(
            "market_30d_return_ranking",
            height = "1000px"
          )
          
        )
        
      ),
      
      # ========================================================
      # 分頁 2：51 檔即時監控
      # ========================================================
      
      tabPanel(
        
        "51檔即時監控",
        
        div(
          class = "section-box",
          
          h2("51 檔股票最新行情監控"),
          
          p(
            class = "page-description",
            "以下整理 0050 與目前 50 檔成分股的最新可取得行情，"
            ,"方便快速查看各股票的最新價格、單日漲跌、成交量與成交量相對於近 20 個交易日平均值的變化。"
          ),
          
          p(
            class = "control-description",
            "資料系統每 60 秒檢查一次是否有新版本。若資料來源尚未完成當日市場資料更新，"
            ,"本頁將維持前一個有效交易日資料；因此本頁的「最新」是指資料來源目前可取得的最新交易日，"
            ,"並非盤中即時報價。"
          ),
          
          hr(),
          
          h3("最新行情"),
          
          tableOutput(
            "realtime_monitor_table"
          )
          
        )
        
      ),   
      
      # ========================================================
      # 分頁 2：個股分析
      # ========================================================

         
          tabPanel(
            
            "個股分析",
            
            div(
              class = "section-box",
              
              h2("個股分析"),
              
              p(
                class = "page-description",
                "選擇股票與分析期間後，可從價格、報酬分布、績效與風險等角度進行完整分析。所有分析共用同一組股票與分析期間。一次最多比較 5 檔股票。"
              ),
              
              # ====================================================
              # 1. 分析設定
              # ====================================================
              
              h3("分析設定"),
              
              dateRangeInput(
                
                inputId = "date_range",
                
                label = "分析期間：",
                
                start = research_start_date,
                
                end = research_end_date,
                
                min = data_min_date,
                
                max = data_max_date,
                
                format = "yyyy-mm-dd",
                
                separator = " 至 "
                
              ),
              
              selectizeInput(
                
                inputId = "selected_stock",
                
                label = "請選擇股票（最多 5 檔）：",
                
                choices = c(
                  "0050 元大台灣50" = "0050",
                  
                  setNames(
                    stock_info$symbol,
                    paste(
                      stock_info$symbol,
                      stock_info$name
                    )
                  )
                ),
                
                selected = "0050",
                
                multiple = TRUE,
                
                options = list(
                  maxItems = 5,
                  placeholder = "請選擇股票",
                  plugins = list(
                    "remove_button"
                  )
                )
                
              ),
              
              tags$div(
                
                title = "依每日調整後價格相較前一交易日的變化著色：上漲紅色、下跌綠色、持平灰色。",
                
                checkboxInput(
                  inputId = "color_by_change",
                  label = "啟用漲跌著色",
                  value = FALSE
                )
                
              ),
              
              p(
                class = "control-description",
                "價格與報酬分析皆會依上方選擇的股票及目前設定的分析期間重新計算。"
              ),
              
              hr(),
              
              # ====================================================
              # 2. 實際資料期間
              # ====================================================
              
              h3("所選股票實際資料期間"),
              
              p(
                "以下列出所選股票目前實際具有的資料範圍，協助確認分析期間是否有完整資料。"
              ),
              
              tableOutput(
                "selected_stock_ranges"
              ),
              
              uiOutput(
                "stock_warning"
              ),
              
              hr(),
              
              # ====================================================
              # 3. 價格與累積報酬
              # ====================================================
              
              h3("價格與累積報酬"),
              
              h4("調整後收盤價"),
              
              p(
                "調整後價格適合用於長期價格與投資報酬比較，可降低股票分割、除權息等公司行動對價格序列造成的不連續影響。"
              ),
              
              plotOutput(
                "price_plot",
                height = "500px"
              ),
              
              br(),
              
              h4("分析期間累積報酬率"),
              
              uiOutput(
                "cumulative_return_display"
              ),
              
              p(
                class = "control-description",
                "累積報酬率表示從分析期間第一個有效交易日到目前分析期間結束日的整體價格報酬。"
              ),
              
              br(),
              
              
              
              # ====================================================
              # 4. 日對數報酬描述統計
              # ====================================================
              
              h3("日對數報酬描述統計"),
              
              p(
                "日對數報酬率以相鄰兩個交易日的調整後價格計算，公式為 ln(Pt / Pt-1)。相較單純價格變化，對數報酬適合用於時間序列與統計分析。"
              ),
              
              tableOutput(
                "return_summary"
              ),
              
              hr(),
              
              # ====================================================
              # 5. 日對數報酬分布
              # ====================================================
              
              h3("日對數報酬分布"),
              
              p(
                "直方圖搭配密度曲線，用於觀察日報酬的集中位置、分布寬度、偏態與尾端情形。"
              ),
              
              plotOutput(
                "return_distribution",
                height = "650px"
              ),
              
              hr(),
              
              # ====================================================
              # 6. 日對數報酬箱型圖
              # ====================================================
              
              h3("日對數報酬箱型圖"),
              
              p(
                "箱型圖可比較不同股票日報酬的中位數、四分位距與極端值。箱體越高通常代表中間 50% 報酬的分布範圍越大。"
              ),
              
              plotOutput(
                "return_boxplot",
                height = "500px"
              ),
              
              hr(),
              
              # ====================================================
              # 7. 日對數報酬時間序列
              # ====================================================
              
              h3("日對數報酬時間序列"),
              
              p(
                "時間序列圖可觀察各交易日的報酬變化，以及高波動或極端報酬集中出現的期間。"
              ),
              
              plotOutput(
                "return_timeseries",
                height = "650px"
              ),
              
              hr(),
              
              # ====================================================
              # 8. 期間績效
              # ====================================================
              
              h3("期間績效摘要"),
              
              p(
                "以下指標用於描述選定分析期間內的整體投資表現。累積報酬率描述整段期間的總報酬；年化報酬率則將不同長度的投資期間換算為年度複合報酬，方便比較。"
              ),
              
              tableOutput(
                "period_performance"
              ),
              
              hr(),
              
              # ====================================================
              # 9. 期間價格摘要
              # ====================================================
              
              h3("期間價格摘要"),
              
              p(
                "整理分析期間內的起始價格、結束價格、期間最高價、最低價、平均價格、價格標準差與有效交易日數。"
              ),
              
              tableOutput(
                "price_summary"
              ),
              
              hr(),
              
              # ====================================================
              # 10. 風險指標
              # ====================================================
              
              h3("風險指標摘要"),
              
              p(
                "年化波動度衡量報酬波動程度，數值越高表示價格報酬變化通常越大；Sharpe Ratio 則以每單位風險所取得的報酬衡量投資效率，本 Dashboard 假設無風險利率為 0%。"
              ),
              
              tableOutput(
                "risk_summary"
              ),
              
              hr(),
              
              # ====================================================
              # 11. 最大回撤
              # ====================================================
              
              h3("回撤時間序列"),
              
              p(
                "回撤（Drawdown）衡量目前價格相較於過去累積高點的跌幅。Y = 0% 代表目前位於當時的高點；Y 越低代表從歷史高點回落越深。最大回撤越負，代表期間曾經歷較大的高點至低點跌幅。"
              ),
              
              plotOutput(
                "drawdown_chart",
                height = "650px"
              ),
              
              hr(),
              
              # ====================================================
              # 12. 報酬－風險比較
              # ====================================================
              
              h3("報酬－風險比較"),
              
              p(
                "橫軸代表年化報酬率，縱軸代表年化波動度。往右表示年化報酬較高；往上表示報酬波動較大。右下區域代表較高報酬與較低波動的相對位置，右上則代表較高報酬但同時具有較高波動；左下與左上則分別代表較低報酬搭配不同程度的波動。此圖用於觀察報酬與風險的相對關係，不代表單一指標即可判定投資優劣。"
              ),
              
              plotOutput(
                "return_risk_chart",
                height = "550px"
              )
              
            )
            
          ),
    
    
    # ========================================================
    # 分頁 3：產業比較
    # ========================================================
    
    tabPanel(
      
      "產業比較",
      
      div(
        class = "section-box",
        
        h2("產業比較"),
        
        p(
          class = "page-description",
          "比較 0050 成分股不同產業的報酬、風險與累積表現。"
        ),
        
        h4("目前規劃功能"),
        
        p(
          "產業組成、產業累積報酬、產業報酬－風險比較，以及產業內股票比較將在此頁整合。"
        ),
        
        hr(),
        
        p(
          "目前股票產業分類已建立於 stock_info，後續直接共用同一套分類。"
        )
        
      )
      
    ),
    
    
    
    
    
    # ========================================================
    # 分頁 5：相關性分析
    # ========================================================
    
    tabPanel(
      
      "相關性分析",
      
      div(
        class = "section-box",
        
        h2("相關性分析"),
        
        p(
          class = "page-description",
          "此頁後續將提供個股與其他股票的相關係數比較，以及相關係數矩陣與 Heatmap。"
        ),
        
        h4("目前規劃"),
        
        p(
          "第一階段先提供單一股票與其他股票的相關係數；第二階段再加入 51 × 51 Heatmap。"
        )
        
      )
      
    ),
    
    
    # ========================================================
    # 分頁 7：投資組合
    # ========================================================
    
    tabPanel(
      
      "投資組合",
      
      div(
        class = "section-box",
        
        h2("投資組合模擬"),
        
        p(
          class = "page-description",
          "可為不同股票設定個別交易條件，模擬投入資金、交易成本、持有期間與投資組合績效。"
        ),
        
        # ----------------------------------------------------
        # 投資組合設定
        # ----------------------------------------------------
        
        h3("投資組合設定"),
        
        p(
          "此區與個股分析的選股分開。可為不同股票設定個別買入日期、賣出日期、投入金額或持有股數。"
        ),
        
        selectizeInput(
          
          inputId = "portfolio_stock",
          
          label = "投資組合股票（最多 5 檔）：",
          
          choices = c(
            "0050 元大台灣50" = "0050",
            
            setNames(
              stock_info$symbol,
              paste(
                stock_info$symbol,
                stock_info$name
              )
            )
          ),
          
          selected = "0050",
          
          multiple = TRUE,
          
          options = list(
            maxItems = 5,
            placeholder = "請選擇投資組合股票",
            plugins = list(
              "remove_button"
            )
          )
          
        ),
        
        br(),
        
        selectInput(
          
          inputId = "portfolio_trade_mode",
          
          label = "交易設定方式：",
          
          choices = c(
            "以投入金額設定",
            "以持有股數設定"
          ),
          
          selected = "以投入金額設定"
          
        ),
        
        p(
          class = "control-description",
          
          strong("投入金額模式說明："),
          
          "投入金額為該股票的配置上限。",
          
          "系統會依買入價格、交易單位與手續費，自動計算最多可購買的股數。",
          
          "因此，實際投入資金可能低於設定的投入金額；",
          
          "Dashboard 中的「實際投入資金」是實際成交金額加上買入手續費。"
          
        ),
        
        uiOutput(
          "portfolio_trade_inputs"
        ),
        
        hr(),
        
        # ----------------------------------------------------
        # 交易成本
        # ----------------------------------------------------
        
        h3("交易成本設定"),
        
        p(
          "以下費率可依實際使用的券商或研究假設自行調整。"
        ),
        
        fluidRow(
          
          column(
            width = 3,
            
            numericInput(
              inputId = "brokerage_rate",
              label = "手續費率（%）：",
              value = 0.1425,
              min = 0,
              step = 0.01
            )
            
          ),
          
          column(
            width = 3,
            
            numericInput(
              inputId = "minimum_fee",
              label = "最低手續費（元）：",
              value = 20,
              min = 0,
              step = 1
            )
            
          ),
          
          column(
            width = 3,
            
            numericInput(
              inputId = "stock_tax_rate",
              label = "股票證交稅（%）：",
              value = 0.30,
              min = 0,
              step = 0.01
            )
            
          ),
          
          column(
            width = 3,
            
            numericInput(
              inputId = "etf_tax_rate",
              label = "ETF 證交稅（%）：",
              value = 0.10,
              min = 0,
              step = 0.01
            )
            
          )
          
        ),
        
        p(
          "手續費於買進與賣出時計算；證券交易稅僅於賣出時計算。"
        ),
        
        hr(),
        
        # ----------------------------------------------------
        # 交易明細
        # ----------------------------------------------------
        
        h3("投資組合交易明細"),
        
        p(
          "買入與賣出日期僅可選擇實際交易日；持有中的股票則以分析期間最後一個有效交易日的價格進行估值。"
        ),
        
        tableOutput(
          "portfolio_trade_table"
        ),
        
        hr(),
        
        # ----------------------------------------------------
        # 每日資產價值
        # ----------------------------------------------------
        
        h3("投資組合資產價值"),
        
        p(
          "此圖呈現投資組合每日淨資產價值。尚未買入的資金視為現金；已買入股票依當日價格計算持倉價值，估值時考慮假設賣出所產生之手續費與證券交易稅；已賣出的股票則以實際賣出後所得金額計入現金。"
        ),
        
        dateRangeInput(
          
          inputId = "portfolio_value_date_range",
          
          label = "圖表時間範圍：",
          
          start = data_min_date,
          
          end = data_max_date,
          
          min = data_min_date,
          
          max = data_max_date,
          
          format = "yyyy-mm-dd",
          
          separator = " 至 "
          
        ),
        
        plotOutput(
          "portfolio_value_chart",
          height = "600px"
        ),
        
        hr(),
        
        # ----------------------------------------------------
        # 投資組合績效摘要
        # ----------------------------------------------------
        
        h3("投資組合績效摘要"),
        
        tableOutput(
          "portfolio_summary"
        ),
        
        hr(),
        
        # ----------------------------------------------------
        # 績效與風險指標
        # ----------------------------------------------------
        
        h3("投資組合績效與風險指標"),
        
        p(
          "以下指標以最早買入日至分析期間結束日計算；圖表時間範圍僅控制圖表顯示，不影響以下績效指標。Sharpe Ratio 假設無風險利率為 0%。"
        ),
        
        tableOutput(
          "portfolio_performance_metrics"
        ),
        
        hr(),
        
        # ----------------------------------------------------
        # Benchmark
        # ----------------------------------------------------
        
        h3("投資組合與 0050 績效比較"),
        
        p(
          "以下比較使用與投資組合相同的投資期間。投資組合報酬率以初始配置資金與期末資產價值計算；0050 則以同期調整後價格計算累積報酬率。超額報酬定義為投資組合總報酬率減去 0050 總報酬率。"
        ),
        
        tableOutput(
          "portfolio_benchmark_summary"
        ),
        
        plotOutput(
          "portfolio_benchmark_chart",
          height = "500px"
        )
        
      )
      
    ),
    
    
    # ========================================================
    # 分頁 6：資料明細
    # ========================================================
    
    tabPanel(
      
      "資料明細",
      
      div(
        class = "section-box",
        
        h2("資料明細"),
        
        p(
          class = "page-description",
          "查看目前 Dashboard 使用的股票清單、產業分類與資料範圍。"
        ),
        
        tableOutput(
          "stock_table"
        )
        
      )
      
    )   # tabPanel
    
        )     # tabsetPanel
    
      )     # dashboard-tabs
    
    )     # fluidPage
    

# ============================================================
# 4. Server
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # GitHub 最新資料網址
  # ----------------------------------------------------------
  
  github_version_url <- paste0(
    "https://raw.githubusercontent.com/",
    "bug330022-sys/stock-portfolio-dashboard/",
    "main/data_version.txt"
  )
  
  github_rds_url <- paste0(
    "https://raw.githubusercontent.com/",
    "bug330022-sys/stock-portfolio-dashboard/",
    "main/stock_daily_v2.rds"
  )
  
  
  # ----------------------------------------------------------
  # 自動取得 GitHub 最新資料
  # ----------------------------------------------------------
  
  data <- reactivePoll(
    
    intervalMillis = 60000,
    
    session = session,
    
    
    # ========================================================
    # 檢查 GitHub 的資料版本
    # ========================================================
    
    checkFunc = function() {
      
      version_text <- tryCatch(
        
        readLines(
          github_version_url,
          warn = FALSE
        ),
        
        error = function(e) {
          NULL
        }
        
      )
      
      
      if (is.null(version_text)) {
        
        return(
          "github_unavailable"
        )
        
      }
      
      
      paste(
        version_text,
        collapse = "\n"
      )
      
    },
    
    
    # ========================================================
    # 需要更新時，下載最新 RDS
    # ========================================================
    
    valueFunc = function() {
      
      temp_file <- tempfile(
        fileext = ".rds"
      )
      
      
      result <- tryCatch(
        
        {
          
          download.file(
            github_rds_url,
            temp_file,
            mode = "wb",
            quiet = TRUE
          )
          
          
          readRDS(
            temp_file
          )
          
        },
        
        
        # ------------------------------------------------------
        # GitHub 暫時無法連線
        # → 使用本機備份資料
        # ------------------------------------------------------
        
        error = function(e) {
          
          readRDS(
            "stock_daily_v2.rds"
          )
          
        },
        
        
        # ------------------------------------------------------
        # 刪除暫存檔
        # ------------------------------------------------------
        
        finally = {
          
          if (
            file.exists(temp_file)
          ) {
            
            unlink(
              temp_file
            )
            
          }
          
        }
        
      )
      
      
      result
      
    }
    
  )
  
 
  


# ============================================================
# 依分析期間篩選資料
# ============================================================

filtered_data <- reactive({
  
  current_data <- data()
  
  req(
    input$date_range
  )
  
  start_date <- as.Date(
    input$date_range[1]
  )
  
  end_date <- as.Date(
    input$date_range[2]
  )
  
  req(
    !is.na(start_date),
    !is.na(end_date)
  )
  
  current_data[
    current_data$date >= start_date &
      current_data$date <= end_date,
  ]
  
})
  
  # ============================================================
  # 個股分析：股票固定代表色
  # ============================================================
  
  all_symbols <- unique(
    c(
      "0050",
      stock_info$symbol
    )
  )
  
  # ------------------------------------------------------------
  # 建立足夠 51 檔股票使用的固定代表色
  # 顏色偏鮮豔，但降低飽和度與亮度，避免過度刺眼
  # ------------------------------------------------------------
  
  stock_colors <- grDevices::hsv(
    
    h = seq(
      0,
      1,
      length.out = length(all_symbols) + 1
    )[-(length(all_symbols) + 1)],
    
    s = 0.68,
    
    v = 0.82
    
  )
  
  # ------------------------------------------------------------
  # 股票代號 → 固定顏色
  # 同一檔股票在所有圖表永遠使用相同顏色
  # ------------------------------------------------------------
  
  stock_color_map <- setNames(
    
    stock_colors,
    
    all_symbols
    
  )
  
  
  
  # ============================================================
  # 個股分析共用資料
  # ============================================================
  
  selected_analysis_data <- reactive({
    
    current_data <- filtered_data()
    
    req(
      input$selected_stock
    )
    
    req(
      length(input$selected_stock) > 0
    )
    
    current_data <- current_data[
      current_data$symbol %in% input$selected_stock &
        is.finite(
          as.numeric(
            current_data$adjusted
          )
        ),
    ]
    
    req(
      nrow(current_data) > 0
    )
    
    current_data <- current_data[
      order(
        current_data$symbol,
        current_data$date
      ),
    ]
    
    current_data <- current_data %>%
      dplyr::group_by(
        symbol
      ) %>%
      dplyr::mutate(
        
        simple_return =
          adjusted /
          dplyr::lag(adjusted) -
          1,
        
        log_return =
          log(
            adjusted /
              dplyr::lag(adjusted)
          ),
        
        running_max =
          cummax(adjusted),
        
        drawdown =
          adjusted /
          running_max -
          1
        
      ) %>%
      dplyr::ungroup()
    
    current_data
    
  })

  # ============================================================
  # 所選股票實際資料期間
  # ============================================================
  
  output$selected_stock_ranges <- renderTable({
    
    x <- data() %>%
      dplyr::filter(
        symbol %in% input$selected_stock,
        is.finite(adjusted)
      ) %>%
      dplyr::group_by(
        symbol
      ) %>%
      dplyr::summarise(
        
        起始日期 = min(date),
        
        結束日期 = max(date),
        
        有效交易日數 = n(),
        
        .groups = "drop"
        
      )
    
    req(
      nrow(x) > 0
    )
    
    x$name <- stock_info$name[
      match(
        x$symbol,
        stock_info$symbol
      )
    ]
    
    x$name[
      is.na(x$name)
    ] <- "未設定"
    
    x <- x[
      c(
        "symbol",
        "name",
        "起始日期",
        "結束日期",
        "有效交易日數"
      )
    ]
    
    names(x) <- c(
      "股票代號",
      "股票名稱",
      "起始日期",
      "結束日期",
      "有效交易日數"
    )
    
    x
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE)
  
  
  # ============================================================
  # 價格圖
  # ============================================================
  
  output$price_plot <- renderPlot({
    
    x <- selected_analysis_data()
    
    req(
      nrow(x) > 0
    )
    
    ggplot(
      x,
      aes(
        x = date,
        y = adjusted,
        color = symbol
      )
    ) +
      
      geom_line(
        linewidth = 0.9
      ) +
      
      scale_x_date(
        breaks = scales::breaks_pretty(
          n = 8
        )
      ) +
      
      labs(
        x = "日期",
        y = "調整後收盤價",
        color = "股票"
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title = element_text(
          hjust = 0.5
        ),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        legend.position = "bottom"
      )
    
  })
  
  
  # ============================================================
  # 分析期間累積報酬率
  # ============================================================
  
  output$cumulative_return_display <- renderUI({
    
    x <- selected_analysis_data()
    
    req(
      nrow(x) > 0
    )
    
    result <- x %>%
      dplyr::group_by(
        symbol
      ) %>%
      dplyr::summarise(
        
        first_price = first(adjusted),
        
        last_price = last(adjusted),
        
        cumulative_return =
          last_price /
          first_price -
          1,
        
        .groups = "drop"
        
      )
    
    result$name <- stock_info$name[
      match(
        result$symbol,
        stock_info$symbol
      )
    ]
    
    result$name[
      is.na(result$name)
    ] <- "未設定"
    
    tagList(
      
      lapply(
        
        seq_len(
          nrow(result)
        ),
        
        function(i) {
          
          div(
            
            style = "margin-bottom:10px;",
            
            strong(
              paste(
                result$symbol[i],
                result$name[i]
              )
            ),
            
            br(),
            
            strong(
              sprintf(
                "%+.2f%%",
                result$cumulative_return[i] * 100
              )
            )
            
          )
          
        }
        
      )
      
    )
    
  })
  
  
  # ============================================================
  # 日對數報酬描述統計
  # ============================================================
  
  output$return_summary <- renderTable({
    
    x <- selected_analysis_data() %>%
      dplyr::filter(
        is.finite(log_return)
      )
    
    req(
      nrow(x) > 0
    )
    
    result <- x %>%
      dplyr::group_by(
        symbol
      ) %>%
      dplyr::summarise(
        
        有效交易日數 = n(),
        
        平均數 = mean(
          log_return,
          na.rm = TRUE
        ),
        
        標準差 = sd(
          log_return,
          na.rm = TRUE
        ),
        
        中位數 = median(
          log_return,
          na.rm = TRUE
        ),
        
        最小值 = min(
          log_return,
          na.rm = TRUE
        ),
        
        最大值 = max(
          log_return,
          na.rm = TRUE
        ),
        
        偏態 = moments::skewness(
          log_return,
          na.rm = TRUE
        ),
        
        峰度 = moments::kurtosis(
          log_return,
          na.rm = TRUE
        ),
        
        .groups = "drop"
        
      )
    
    result$name <- stock_info$name[
      match(
        result$symbol,
        stock_info$symbol
      )
    ]
    
    result$name[
      is.na(result$name)
    ] <- "未設定"
    
    result <- result[
      c(
        "symbol",
        "name",
        "有效交易日數",
        "平均數",
        "標準差",
        "中位數",
        "最小值",
        "最大值",
        "偏態",
        "峰度"
      )
    ]
    
    names(result)[1:2] <- c(
      "股票代號",
      "股票名稱"
    )
    
    result
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE)
  
  
  # ============================================================
  # 日對數報酬分布
  # ============================================================
  
  output$return_distribution <- renderPlot({
    
    x <- selected_analysis_data() %>%
      dplyr::filter(
        is.finite(log_return)
      )
    
    req(
      nrow(x) > 0
    )
    
    ggplot(
      x,
      aes(
        x = log_return
      )
    ) +
      
      # --------------------------------------------------------
    # 直方圖：淡色
    # --------------------------------------------------------
    
    geom_histogram(
      aes(
        fill = symbol,
        y = after_stat(density)
      ),
      bins = 40,
      alpha = 0.35,
      position = "identity"
    ) +
      
      # --------------------------------------------------------
    # 密度線：深色、不填下面
    # --------------------------------------------------------
    
    geom_density(
      aes(
        color = symbol
      ),
      linewidth = 1.0,
      alpha = 0.95,
      fill = NA
    ) +
      
      facet_wrap(
        ~ symbol,
        scales = "free_y"
      ) +
      
      scale_fill_manual(
        values = stock_color_map,
        drop = FALSE
      ) +
      
      scale_color_manual(
        values = stock_color_map,
        drop = FALSE
      ) +
      
      labs(
        x = "日對數報酬率",
        y = "密度",
        fill = "股票",
        color = "股票"
      ) +
      
      theme_minimal() +
      
      theme(
        
        plot.title = element_text(
          hjust = 0.5
        ),
        
        legend.position = "bottom"
        
      )
    
  })
  
  # ============================================================
  # 日對數報酬箱型圖
  # ============================================================
  
  output$return_boxplot <- renderPlot({
    
    x <- selected_analysis_data() %>%
      dplyr::filter(
        is.finite(log_return)
      )
    
    req(
      nrow(x) > 0
    )
    
    ggplot(
      x,
      aes(
        x = symbol,
        y = log_return,
        fill = symbol
      )
    ) +
      
      geom_boxplot(
        width = 0.65,
        alpha = 0.80
      ) +
      
      scale_fill_manual(
        values = stock_color_map,
        drop = FALSE
      ) +
      
      labs(
        x = "股票",
        y = "日對數報酬率"
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title = element_text(
          hjust = 0.5
        ),
        legend.position = "none"
      )
    
  })
  
  
  # ============================================================
  # 日對數報酬時間序列
  # ============================================================
  
  output$return_timeseries <- renderPlot({
    
    x <- selected_analysis_data() %>%
      dplyr::filter(
        is.finite(log_return)
      )
    
    req(
      nrow(x) > 0
    )
    
    ggplot(
      x,
      aes(
        x = date,
        y = log_return,
        color = symbol
      )
    ) +
      
      geom_line(
        linewidth = 0.55,
        alpha = 0.90
      ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        linewidth = 0.6
      ) +
      
      facet_wrap(
        ~ symbol,
        ncol = 1,
        scales = "free_y"
      ) +
      
      scale_color_manual(
        values = stock_color_map,
        drop = FALSE
      ) +
      
      scale_y_continuous(
        labels = scales::percent_format(
          accuracy = 0.1
        )
      ) +
      
      labs(
        x = "日期",
        y = "日對數報酬率",
        color = "股票"
      ) +
      
      theme_minimal() +
      
      theme(
        
        plot.title = element_text(
          hjust = 0.5
        ),
        
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        
        legend.position = "bottom"
        
      )
    
  })
  
  # ============================================================
  # 期間績效
  # ============================================================
  
  output$period_performance <- renderTable({
    
    x <- selected_analysis_data()
    
    req(
      nrow(x) > 0
    )
    
    result <- x %>%
      dplyr::group_by(
        symbol
      ) %>%
      dplyr::summarise(
        
        start_date = min(date),
        
        end_date = max(date),
        
        start_price = first(adjusted),
        
        end_price = last(adjusted),
        
        cumulative_return =
          end_price /
          start_price -
          1,
        
        trading_days = n(),
        
        annual_return =
          ifelse(
            n() > 1,
            (
              end_price /
                start_price
            ) ^ (
              252 /
                (n() - 1)
            ) - 1,
            NA_real_
          ),
        
        .groups = "drop"
        
      )
    
    result$name <- stock_info$name[
      match(
        result$symbol,
        stock_info$symbol
      )
    ]
    
    result$name[
      is.na(result$name)
    ] <- "未設定"
    
    result$cumulative_return <- paste0(
      sprintf(
        "%+.2f",
        result$cumulative_return * 100
      ),
      "%"
    )
    
    result$annual_return <- ifelse(
      is.na(result$annual_return),
      NA,
      paste0(
        sprintf(
          "%+.2f",
          result$annual_return * 100
        ),
        "%"
      )
    )
    
    result <- result[
      c(
        "symbol",
        "name",
        "start_date",
        "end_date",
        "trading_days",
        "cumulative_return",
        "annual_return"
      )
    ]
    
    names(result) <- c(
      "股票代號",
      "股票名稱",
      "起始日期",
      "結束日期",
      "有效交易日數",
      "累積報酬率",
      "年化報酬率"
    )
    
    result
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE)
  
  
  # ============================================================
  # 期間價格摘要
  # ============================================================
  
  output$price_summary <- renderTable({
    
    x <- selected_analysis_data()
    
    req(
      nrow(x) > 0
    )
    
    result <- x %>%
      dplyr::group_by(
        symbol
      ) %>%
      dplyr::summarise(
        
        起始價格 = first(adjusted),
        
        結束價格 = last(adjusted),
        
        期間最低價 = min(
          adjusted,
          na.rm = TRUE
        ),
        
        期間最高價 = max(
          adjusted,
          na.rm = TRUE
        ),
        
        平均價格 = mean(
          adjusted,
          na.rm = TRUE
        ),
        
        價格標準差 = sd(
          adjusted,
          na.rm = TRUE
        ),
        
        有效交易日數 = n(),
        
        .groups = "drop"
        
      )
    
    result$name <- stock_info$name[
      match(
        result$symbol,
        stock_info$symbol
      )
    ]
    
    result$name[
      is.na(result$name)
    ] <- "未設定"
    
    result[
      c(
        "symbol",
        "name",
        "起始價格",
        "結束價格",
        "期間最低價",
        "期間最高價",
        "平均價格",
        "價格標準差",
        "有效交易日數"
      )
    ] |> 
      setNames(
        c(
          "股票代號",
          "股票名稱",
          "起始價格",
          "結束價格",
          "期間最低價",
          "期間最高價",
          "平均價格",
          "價格標準差",
          "有效交易日數"
        )
      )
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE)
  
  
  # ============================================================
  # 風險指標
  # ============================================================
  
  output$risk_summary <- renderTable({
    
    x <- selected_analysis_data()
    
    req(
      nrow(x) > 1
    )
    
    result <- x %>%
      dplyr::filter(
        is.finite(log_return)
      ) %>%
      dplyr::group_by(
        symbol
      ) %>%
      dplyr::summarise(
        
        annual_volatility =
          sd(
            log_return,
            na.rm = TRUE
          ) *
          sqrt(252),
        
        sharpe_ratio =
          mean(
            log_return,
            na.rm = TRUE
          ) /
          sd(
            log_return,
            na.rm = TRUE
          ) *
          sqrt(252),
        
        max_drawdown =
          min(
            drawdown,
            na.rm = TRUE
          ),
        
        .groups = "drop"
        
      )
    
    result$name <- stock_info$name[
      match(
        result$symbol,
        stock_info$symbol
      )
    ]
    
    result$name[
      is.na(result$name)
    ] <- "未設定"
    
    result$annual_volatility <- paste0(
      sprintf(
        "%.2f",
        result$annual_volatility * 100
      ),
      "%"
    )
    
    result$max_drawdown <- paste0(
      sprintf(
        "%.2f",
        result$max_drawdown * 100
      ),
      "%"
    )
    
    result <- result[
      c(
        "symbol",
        "name",
        "annual_volatility",
        "sharpe_ratio",
        "max_drawdown"
      )
    ]
    
    names(result) <- c(
      "股票代號",
      "股票名稱",
      "年化波動度",
      "Sharpe Ratio",
      "最大回撤"
    )
    
    result
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE)
  
  
  # ============================================================
  # 回撤時間序列
  # ============================================================
  
  output$drawdown_chart <- renderPlot({
    
    x <- selected_analysis_data()
    
    req(
      nrow(x) > 0
    )
    
    ggplot(
      x,
      aes(
        x = date,
        y = drawdown,
        color = symbol
      )
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        linewidth = 0.6
      ) +
      
      geom_line(
        linewidth = 0.75,
        alpha = 0.90
      ) +
      
      facet_wrap(
        ~ symbol,
        ncol = 1,
        scales = "free_y"
      ) +
      
      scale_color_manual(
        values = stock_color_map,
        drop = FALSE
      ) +
      
      scale_y_continuous(
        labels = scales::percent_format(
          accuracy = 1
        )
      ) +
      
      labs(
        x = "日期",
        y = "回撤",
        color = "股票"
      ) +
      
      theme_minimal() +
      
      theme(
        
        plot.title = element_text(
          hjust = 0.5
        ),
        
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        
        legend.position = "bottom"
        
      )
    
  })
  
  
  # ============================================================
  # 報酬－風險比較
  # ============================================================
  
  output$return_risk_chart <- renderPlot({
    
    x <- selected_analysis_data() %>%
      dplyr::filter(
        is.finite(log_return)
      ) %>%
      dplyr::group_by(
        symbol
      ) %>%
      dplyr::summarise(
        
        annual_return =
          exp(
            mean(
              log_return,
              na.rm = TRUE
            ) * 252
          ) - 1,
        
        annual_volatility =
          sd(
            log_return,
            na.rm = TRUE
          ) *
          sqrt(252),
        
        .groups = "drop"
        
      )
    
    req(
      nrow(x) > 0
    )
    
    ggplot(
      x,
      aes(
        x = annual_return * 100,
        y = annual_volatility * 100,
        color = symbol
      )
    ) +
      
      geom_point(
        size = 4
      ) +
      
      geom_text(
        aes(
          label = symbol
        ),
        vjust = -0.8,
        show.legend = FALSE
      ) +
      
      scale_color_manual(
        values = stock_color_map,
        drop = FALSE
      ) +
      
      labs(
        x = "年化報酬率（%）",
        y = "年化波動度（%）",
        color = "股票"
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title = element_text(
          hjust = 0.5
        ),
        legend.position = "bottom"
      )
    
  })
  # ============================================================
  # 分析期間控制
  # ============================================================
  
  trading_dates <- reactive({
    
    current_data <- data()
    
    req(
      nrow(current_data) > 0
    )
    
    sort(
      unique(
        current_data$date
      )
    )
    
  })
  
  
  # --------------------------------------------------------
  # 目前分析模式
  # --------------------------------------------------------
  
  analysis_mode <- reactiveVal(
    "research"
  )
  
  
  # --------------------------------------------------------
  # 研究期間
  # --------------------------------------------------------
  
  observeEvent(
    
    input$period_research,
    
    {
      
      dates <- trading_dates()
      
      req(
        length(dates) > 0
      )
      
      start_date <- research_start_date
      
      end_date <- research_end_date
      
      if (
        start_date < min(dates)
      ) {
        
        start_date <- min(dates)
        
      }
      
      if (
        end_date > max(dates)
      ) {
        
        end_date <- max(dates)
        
      }
      
      analysis_mode(
        "research"
      )
      
      updateDateRangeInput(
        
        session,
        
        "date_range",
        
        start = start_date,
        
        end = end_date,
        
        min = min(dates),
        
        max = max(dates)
        
      )
      
    }
    
  )
  
  
  # --------------------------------------------------------
  # 全部資料
  # --------------------------------------------------------
  
  observeEvent(
    
    input$period_all,
    
    {
      
      dates <- trading_dates()
      
      req(
        length(dates) > 0
      )
      
      analysis_mode(
        "all"
      )
      
      updateDateRangeInput(
        
        session,
        
        "date_range",
        
        start = min(dates),
        
        end = max(dates),
        
        min = min(dates),
        
        max = max(dates)
        
      )
      
    }
    
  )
  
  
  # --------------------------------------------------------
  # 1 週 = 5 個交易日
  # --------------------------------------------------------
  
  observeEvent(
    
    input$period_1w,
    
    {
      
      dates <- trading_dates()
      
      req(
        length(dates) >= 5
      )
      
      analysis_mode(
        "1w"
      )
      
      updateDateRangeInput(
        
        session,
        
        "date_range",
        
        start = dates[
          length(dates) - 4
        ],
        
        end = max(dates),
        
        min = min(dates),
        
        max = max(dates)
        
      )
      
    }
    
  )
  
  
  # --------------------------------------------------------
  # 1 個月 = 21 個交易日
  # --------------------------------------------------------
  
  observeEvent(
    
    input$period_1m,
    
    {
      
      dates <- trading_dates()
      
      req(
        length(dates) >= 21
      )
      
      analysis_mode(
        "1m"
      )
      
      updateDateRangeInput(
        
        session,
        
        "date_range",
        
        start = dates[
          length(dates) - 20
        ],
        
        end = max(dates),
        
        min = min(dates),
        
        max = max(dates)
        
      )
      
    }
    
  )
  
  
  # --------------------------------------------------------
  # 1 季 = 63 個交易日
  # --------------------------------------------------------
  
  observeEvent(
    
    input$period_1q,
    
    {
      
      dates <- trading_dates()
      
      req(
        length(dates) >= 63
      )
      
      analysis_mode(
        "1q"
      )
      
      updateDateRangeInput(
        
        session,
        
        "date_range",
        
        start = dates[
          length(dates) - 62
        ],
        
        end = max(dates),
        
        min = min(dates),
        
        max = max(dates)
        
      )
      
    }
    
  )
  
  
  # --------------------------------------------------------
  # 6 個月 = 126 個交易日
  # --------------------------------------------------------
  
  observeEvent(
    
    input$period_6m,
    
    {
      
      dates <- trading_dates()
      
      req(
        length(dates) >= 126
      )
      
      analysis_mode(
        "6m"
      )
      
      updateDateRangeInput(
        
        session,
        
        "date_range",
        
        start = dates[
          length(dates) - 125
        ],
        
        end = max(dates),
        
        min = min(dates),
        
        max = max(dates)
        
      )
      
    }
    
  )
  
  
  # --------------------------------------------------------
  # 1 年 = 252 個交易日
  # --------------------------------------------------------
  
  observeEvent(
    
    input$period_1y,
    
    {
      
      dates <- trading_dates()
      
      req(
        length(dates) >= 252
      )
      
      analysis_mode(
        "1y"
      )
      
      updateDateRangeInput(
        
        session,
        
        "date_range",
        
        start = dates[
          length(dates) - 251
        ],
        
        end = max(dates),
        
        min = min(dates),
        
        max = max(dates)
        
      )
      
    }
    
  )
  
  
  # --------------------------------------------------------
  # YTD
  # --------------------------------------------------------
  
  observeEvent(
    
    input$period_ytd,
    
    {
      
      dates <- trading_dates()
      
      req(
        length(dates) > 0
      )
      
      latest_date <- max(
        dates
      )
      
      current_year <- as.integer(
        format(
          latest_date,
          "%Y"
        )
      )
      
      year_start <- as.Date(
        paste0(
          current_year,
          "-01-01"
        )
      )
      
      ytd_dates <- dates[
        dates >= year_start
      ]
      
      req(
        length(ytd_dates) > 0
      )
      
      analysis_mode(
        "ytd"
      )
      
      updateDateRangeInput(
        
        session,
        
        "date_range",
        
        start = min(ytd_dates),
        
        end = latest_date,
        
        min = min(dates),
        
        max = max(dates)
        
      )
      
    }
    
  )
  
  
  # --------------------------------------------------------
  # 自訂
  # --------------------------------------------------------
  
  observeEvent(
    
    input$period_custom,
    
    {
      
      analysis_mode(
        "custom"
      )
      
    }
    
  )
  
  # ========================================================
  # 分析期間日期範圍同步
  # ========================================================
  
  observeEvent(
    
    data(),
    
    {
      
      dates <- trading_dates()
      
      req(
        length(dates) > 0
      )
      
      current_start <- isolate(
        input$date_range[1]
      )
      
      current_end <- isolate(
        input$date_range[2]
      )
      
      if (
        is.null(current_start) ||
        length(current_start) == 0 ||
        is.na(current_start)
      ) {
        
        current_start <- research_start_date
        
      }
      
      if (
        is.null(current_end) ||
        length(current_end) == 0 ||
        is.na(current_end)
      ) {
        
        current_end <- research_end_date
        
      }
      
      current_start <- as.Date(
        current_start
      )
      
      current_end <- as.Date(
        current_end
      )
      
      if (
        current_start < min(dates)
      ) {
        
        current_start <- min(dates)
        
      }
      
      if (
        current_end > max(dates)
      ) {
        
        current_end <- max(dates)
        
      }
      
      if (
        current_start > current_end
      ) {
        
        current_start <- min(dates)
        current_end <- max(dates)
        
      }
      
      updateDateRangeInput(
        
        session,
        
        "date_range",
        
        start = current_start,
        
        end = current_end,
        
        min = min(dates),
        
        max = max(dates)
        
      )
      
    },
    
    ignoreInit = FALSE
    
  )
 
  
  
  # ========================================================
  # 顯示目前分析期間
  # ========================================================
  
  output$analysis_period_text <- renderText({
    
    req(
      input$date_range
    )
    
    start_date <- as.Date(
      input$date_range[1]
    )
    
    end_date <- as.Date(
      input$date_range[2]
    )
    
    req(
      !is.na(start_date),
      !is.na(end_date)
    )
    
    paste0(
      format(
        start_date,
        "%Y-%m-%d"
      ),
      " 至 ",
      format(
        end_date,
        "%Y-%m-%d"
      )
    )
    
  })
  

  
  # ========================================================
  # 個股分析：依分析期間更新可選股票
  # ========================================================
  
  analysis_available_symbols <- reactive({
    
    current_data <- data()
    
    req(
      nrow(current_data) > 0
    )
    
    all_symbols <- c(
      "0050",
      stock_info$symbol
    )
    
    mode <- analysis_mode()
    
    
    # ------------------------------------------------------
    # 全部資料
    # ------------------------------------------------------
    
    if (
      identical(
        mode,
        "all"
      )
    ) {
      
      return(
        all_symbols
      )
      
    }
    
    
    # ------------------------------------------------------
    # 其餘模式需要分析期間
    # ------------------------------------------------------
    
    req(
      input$date_range
    )
    
    selected_start <- as.Date(
      input$date_range[1]
    )
    
    selected_end <- as.Date(
      input$date_range[2]
    )
    
    req(
      !is.na(selected_start),
      !is.na(selected_end)
    )
    
    
    # ------------------------------------------------------
    # 計算各股票實際資料起訖日
    # ------------------------------------------------------
    
    coverage <- current_data %>%
      dplyr::filter(
        symbol %in% all_symbols,
        is.finite(adjusted)
      ) %>%
      dplyr::group_by(
        symbol
      ) %>%
      dplyr::summarise(
        
        stock_start = min(
          date,
          na.rm = TRUE
        ),
        
        stock_end = max(
          date,
          na.rm = TRUE
        ),
        
        .groups = "drop"
        
      )
    
    
    # ------------------------------------------------------
    # 只有完整涵蓋分析期間的股票才能選
    # ------------------------------------------------------
    
    valid_symbols <- coverage$symbol[
      
      coverage$stock_start <= selected_start &
        coverage$stock_end >= selected_end
      
    ]
    
    
    valid_symbols
    
  })
  
  
  # ========================================================
  # 顯示符合分析期間的股票數量
  # ========================================================
  
  output$analysis_stock_availability <- renderText({
    
    symbols <- analysis_available_symbols()
    
    length(
      symbols
    )
    
  })
  
  
  # ========================================================
  # 依分析期間更新個股選單
  # ========================================================
  
  observeEvent(
    
    list(
      input$date_range,
      analysis_mode(),
      data()
    ),
    
    {
      
      valid_symbols <- analysis_available_symbols()
      
      # ------------------------------------------------------
      # 取得目前已選股票
      # ------------------------------------------------------
      
      current_selected <- input$selected_stock
      
      if (
        is.null(current_selected) ||
        length(current_selected) == 0
      ) {
        
        current_selected <- character(0)
        
      }
      
      current_selected <- as.character(
        current_selected
      )
      
      # ------------------------------------------------------
      # 只保留仍符合分析期間的股票
      # ------------------------------------------------------
      
      valid_selected <- intersect(
        current_selected,
        valid_symbols
      )
      
      # ------------------------------------------------------
      # 如果全部被排除，優先選擇 0050
      # ------------------------------------------------------
      
      if (
        length(valid_selected) == 0 &&
        "0050" %in% valid_symbols
      ) {
        
        valid_selected <- "0050"
        
      }
      
      # ------------------------------------------------------
      # 建立可選股票名稱
      # ------------------------------------------------------
      
      valid_stock_info <- stock_info %>%
        dplyr::filter(
          symbol %in% valid_symbols
        )
      
      choices <- character(0)
      
      # ------------------------------------------------------
      # 加入 0050
      # ------------------------------------------------------
      
      if (
        "0050" %in% valid_symbols
      ) {
        
        choices <- c(
          "0050 元大台灣50" = "0050"
        )
        
      }
      
      # ------------------------------------------------------
      # 加入其他股票
      # ------------------------------------------------------
      
      if (
        nrow(valid_stock_info) > 0
      ) {
        
        stock_choices <- setNames(
          valid_stock_info$symbol,
          paste(
            valid_stock_info$symbol,
            valid_stock_info$name
          )
        )
        
        choices <- c(
          choices,
          stock_choices
        )
        
      }
      
      # ------------------------------------------------------
      # 更新 Selectize
      # ------------------------------------------------------
      
      updateSelectizeInput(
        session,
        "selected_stock",
        choices = choices,
        selected = valid_selected,
        server = TRUE
      )
      
    },
    
    ignoreInit = FALSE
    
  )
  
  # ----------------------------------------------------------
  # 0050 調整後價格走勢圖
  # ----------------------------------------------------------
  # ----------------------------------------------------------
  # 所選股票的實際資料期間
  # ----------------------------------------------------------
  
  # ----------------------------------------------------------
  # 顯示所選股票實際資料期間
  # ----------------------------------------------------------
  
  output$selected_stock_ranges <- renderTable({
    
    current_data <- data()
    
    req(input$selected_stock)
    
    result <- lapply(
      input$selected_stock,
      function(symbol) {
        
        x <- current_data[
          current_data$symbol == symbol,
        ]
        
        stock_name <- ifelse(
          symbol == "0050",
          "元大台灣50",
          stock_info$name[
            match(symbol, stock_info$symbol)
          ]
        )
        
        data.frame(
          股票代號 = symbol,
          股票名稱 = stock_name,
          最早資料日 = format(
            min(x$date),
            "%Y-%m-%d"
          ),
          最新資料日 = format(
            max(x$date),
            "%Y-%m-%d"
          ),
          stringsAsFactors = FALSE
        )
        
      }
    )
    
    do.call(rbind, result)
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE)
  
  # ----------------------------------------------------------
  # 檢查分析期間是否超出個股實際資料期間
  # ----------------------------------------------------------
  # ----------------------------------------------------------
  # 檢查所選股票是否涵蓋完整分析期間
  # ----------------------------------------------------------
  
  output$stock_warning <- renderUI({
    
    current_data <- data()
    
    req(input$selected_stock)
    req(input$date_range)
    
    selected_min <- input$date_range[1]
    selected_max <- input$date_range[2]
    
    messages <- c()
    
    for (symbol in input$selected_stock) {
      
      x <- current_data[
        current_data$symbol == symbol,
      ]
      
      req(nrow(x) > 0)
      
      stock_min <- min(x$date)
      stock_max <- max(x$date)
      
      stock_name <- ifelse(
        symbol == "0050",
        "元大台灣50",
        stock_info$name[
          match(symbol, stock_info$symbol)
        ]
      )
      
      if (selected_min < stock_min) {
        
        messages <- c(
          messages,
          paste0(
            "⚠️ ",
            symbol,
            " ",
            stock_name,
            " 的資料從 ",
            format(stock_min, "%Y-%m-%d"),
            " 才開始，分析期間前段沒有資料。"
          )
        )
        
      }
      
      if (selected_max > stock_max) {
        
        messages <- c(
          messages,
          paste0(
            "⚠️ ",
            symbol,
            " ",
            stock_name,
            " 的資料截至 ",
            format(stock_max, "%Y-%m-%d"),
            "，分析期間後段沒有資料。"
          )
        )
        
      }
      
    }
    
    if (length(messages) == 0) {
      return(NULL)
    }
    
    div(
      style = "color: #b45309; margin-bottom: 15px;",
      
      lapply(
        messages,
        function(msg) {
          p(msg)
        }
      )
    )
    
  })
  
  # ----------------------------------------------------------
  # 依分析期間篩選資料
  # ----------------------------------------------------------
  
  filtered_data <- reactive({
    
    current_data <- data()
    
    req(
      input$date_range
    )
    
    start_date <- as.Date(input$date_range[1])
    end_date <- as.Date(input$date_range[2])
    
    req(
      !is.na(start_date),
      !is.na(end_date)
    )
    
    current_data[
      current_data$date >= start_date &
        current_data$date <= end_date,
    ]
  })
  # ----------------------------------------------------------
  # 累積報酬率
  # ----------------------------------------------------------
  
  output$cumulative_return_display <- renderUI({
    
    x <- filtered_data()
    
    req(input$selected_stock)
    
    result <- lapply(
      input$selected_stock,
      function(symbol) {
        
        stock_data <- x[
          x$symbol == symbol,
        ]
        
        req(nrow(stock_data) > 0)
        
        stock_name <- ifelse(
          symbol == "0050",
          "元大台灣50",
          stock_info$name[
            match(
              symbol,
              stock_info$symbol
            )
          ]
        )
        
        # 第一筆與最後一筆調整後價格
        start_price <- stock_data$adjusted[1]
        end_price <- stock_data$adjusted[
          nrow(stock_data)
        ]
        
        # 累積報酬率
        cumulative_return <-
          end_price / start_price - 1
        
        # 正報酬紅色
        # 負報酬綠色
        # 0 為灰色
        text_color <- if (
          cumulative_return > 0
        ) {
          "red"
        } else if (
          cumulative_return < 0
        ) {
          "green"
        } else {
          "gray"
        }
        
        div(
          style = paste0(
            "display:inline-block;",
            "margin-right:30px;",
            "margin-bottom:10px;"
          ),
          
          strong(
            paste(
              symbol,
              stock_name
            )
          ),
          
          br(),
          
          span(
            style = paste0(
              "font-size:20px;",
              "font-weight:bold;",
              "color:",
              text_color,
              ";"
            ),
            
            sprintf(
              "%+.2f%%",
              cumulative_return * 100
            )
          )
        )
        
      }
    )
    
    do.call(
      tagList,
      result
    )
    
  })
  
  
  
  
  
  # ----------------------------------------------------------
  # 多股票調整後價格比較圖
  # ----------------------------------------------------------
  
  output$price_plot <- renderPlot({
    
    x <- filtered_data()
    
    req(input$selected_stock)
    
    symbols <- input$selected_stock
    
    x <- x[
      x$symbol %in% symbols,
    ]
    
    req(nrow(x) > 0)
    
    # --------------------------------------------------------
    # 確保日期排序
    # --------------------------------------------------------
    
    x <- x[
      order(x$symbol, x$date),
    ]
    
    # --------------------------------------------------------
    # 價格範圍
    # --------------------------------------------------------
    
    y_min <- min(
      x$adjusted,
      na.rm = TRUE
    )
    
    y_max <- max(
      x$adjusted,
      na.rm = TRUE
    )
    
    if (y_min == y_max) {
      
      y_min <- y_min * 0.95
      y_max <- y_max * 1.05
      
    }
    
    # --------------------------------------------------------
    # 建立空白圖
    # --------------------------------------------------------
    
    plot(
      NA,
      NA,
      
      xlim = range(x$date),
      ylim = c(y_min, y_max),
      
      xlab = "日期",
      ylab = "調整後價格",
      
      main = if (
        input$color_by_change
      ) {
        "個股調整後價格走勢（漲跌著色）"
      } else {
        "個股調整後價格比較"
      }
    )
 
  
    
    # --------------------------------------------------------
    # 畫每一檔股票
    # --------------------------------------------------------
    
    for (i in seq_along(symbols)) {
      
      symbol <- symbols[i]
      
      stock_data <- x[
        x$symbol == symbol,
      ]
      
      stock_data <- stock_data[
        order(stock_data$date),
      ]
      
      req(nrow(stock_data) > 0)
      
      
      # ======================================================
      # 一般模式
      # ======================================================
      
      if (!input$color_by_change) {
        
        lines(
          stock_data$date,
          stock_data$adjusted,
          col = i,
          lwd = 2
        )
        
      }
      
      
      # ======================================================
      # 漲跌著色模式
      # ======================================================
      
      if (input$color_by_change) {
        
        # 至少需要兩個交易日才能畫漲跌區間
        if (nrow(stock_data) >= 2) {
          
          for (j in 2:nrow(stock_data)) {
            
            change <- (
              stock_data$adjusted[j] -
                stock_data$adjusted[j - 1]
            )
            
            segment_color <- if (
              change > 0
            ) {
              "red"
            } else if (
              change < 0
            ) {
              "green"
            } else {
              "gray"
            }
            
            lines(
              stock_data$date[
                (j - 1):j
              ],
              
              stock_data$adjusted[
                (j - 1):j
              ],
              
              col = segment_color,
              
              lwd = 2,
              
              lty = i
            )
            
          }
          
        }
        
      }
      
    }
    
    
    # --------------------------------------------------------
    # 股票名稱
    # --------------------------------------------------------
    
    labels <- sapply(
      symbols,
      function(symbol) {
        
        stock_name <- ifelse(
          symbol == "0050",
          "元大台灣50",
          stock_info$name[
            match(
              symbol,
              stock_info$symbol
            )
          ]
        )
        
        paste(
          symbol,
          stock_name
        )
        
      }
    )
    
    
    # --------------------------------------------------------
    # 圖例
    # --------------------------------------------------------
    
    if (!input$color_by_change) {
      
      legend(
        "topleft",
        legend = labels,
        col = seq_along(symbols),
        lwd = 2,
        bty = "n"
      )
      
    } else {
      
      legend(
        "topleft",
        legend = labels,
        col = "black",
        lty = seq_along(symbols),
        lwd = 2,
        bty = "n"
      )
      
      legend(
        "topright",
        legend = c(
          "上漲",
          "下跌",
          "持平"
        ),
        col = c(
          "red",
          "green",
          "gray"
        ),
        lwd = 3,
        bty = "n"
      )
      
    }
    
  })
  # ----------------------------------------------------------
  # 股票數量
  # ----------------------------------------------------------
  
  output$n_stock <- renderText({
    
    length(unique(data()$symbol))
    
  })
  
  output$n_data <- renderText({
    
    current_data <- data()
    
    req(
      nrow(current_data) > 0
    )
    
    nrow(current_data)
    
  })
  
  # ----------------------------------------------------------
  # 全站資料狀態
  # ----------------------------------------------------------
  
  output$global_n_stock <- renderText({
    
    current_data <- data()
    
    req(
      nrow(current_data) > 0
    )
    
    length(
      unique(current_data$symbol)
    )
    
  })
  
  
  output$global_n_data <- renderText({
    
    current_data <- data()
    
    req(
      nrow(current_data) > 0
    )
    
    nrow(current_data)
    
  })
  
  
  output$global_min_date <- renderText({
    
    current_data <- data()
    
    req(
      nrow(current_data) > 0
    )
    
    format(
      min(current_data$date, na.rm = TRUE),
      "%Y-%m-%d"
    )
    
  })
  
  
  output$global_max_date <- renderText({
    
    current_data <- data()
    
    req(
      nrow(current_data) > 0
    )
    
    format(
      max(current_data$date, na.rm = TRUE),
      "%Y-%m-%d"
    )
    
  })
  
  # ----------------------------------------------------------
  # 最早日期
  # ----------------------------------------------------------
  
  output$min_date <- renderText({
    
    format(min(data()$date), "%Y-%m-%d")
    
  })
  
  
  # ----------------------------------------------------------
  # 最新日期
  # ----------------------------------------------------------
  
  output$max_date <- renderText({
    
    format(max(data()$date), "%Y-%m-%d")
    
  })
  # ========================================================
  # 市場總覽
  # ========================================================
  
  # ========================================================
  # 51 檔最新行情監控
  # ========================================================
  
  realtime_monitor_data <- reactive({
    
    current_data <- data()
    
    req(
      nrow(current_data) > 0
    )
    
    # --------------------------------------------------------
    # 檢查必要欄位
    # --------------------------------------------------------
    
    req(
      "symbol" %in% names(current_data),
      "date" %in% names(current_data),
      "close" %in% names(current_data),
      "volume" %in% names(current_data)
    )
    
    # --------------------------------------------------------
    # 51 檔股票
    # 0050 + 50 檔成分股
    # --------------------------------------------------------
    
    monitor_symbols <- c(
      "0050",
      stock_info$symbol
    )
    
    df <- current_data %>%
      dplyr::filter(
        symbol %in% monitor_symbols,
        is.finite(close),
        is.finite(volume)
      ) %>%
      dplyr::arrange(
        symbol,
        date
      ) %>%
      dplyr::group_by(
        symbol
      ) %>%
      dplyr::mutate(
        
        # ------------------------------------------------------
        # 前一交易日收盤價
        # ------------------------------------------------------
        
        previous_close = dplyr::lag(
          close
        ),
        
        # ------------------------------------------------------
        # 單日漲跌幅
        # ------------------------------------------------------
        
        daily_change_pct = (
          close /
            previous_close -
            1
        ) * 100
        
      ) %>%
      dplyr::group_modify(
        ~ {
          
          x <- .x
          
          # ----------------------------------------------------
          # 前 20 個交易日平均成交量
          # 不包含今天
          # ----------------------------------------------------
          
          x$volume_20d_avg <- sapply(
            seq_len(nrow(x)),
            function(i) {
              
              if (i <= 20) {
                
                return(NA_real_)
                
              }
              
              previous_volumes <- x$volume[
                (i - 20):(i - 1)
              ]
              
              previous_volumes <- previous_volumes[
                is.finite(previous_volumes)
              ]
              
              if (
                length(previous_volumes) == 0
              ) {
                
                return(NA_real_)
                
              }
              
              mean(
                previous_volumes
              )
              
            }
          )
          
          # ----------------------------------------------------
          # 成交量相較近 20 日平均的變化
          # ----------------------------------------------------
          
          x$volume_20d_pct <- (
            x$volume /
              x$volume_20d_avg -
              1
          ) * 100
          
          x
          
        }
      ) %>%
      dplyr::ungroup()
    
    # --------------------------------------------------------
    # 每檔股票只保留最新資料
    # --------------------------------------------------------
    
    latest <- df %>%
      dplyr::group_by(
        symbol
      ) %>%
      dplyr::slice_tail(
        n = 1
      ) %>%
      dplyr::ungroup()
    
    # --------------------------------------------------------
    # 加入股票名稱
    # --------------------------------------------------------
    
    latest$stock_name <- sapply(
      latest$symbol,
      function(symbol) {
        
        if (
          symbol == "0050"
        ) {
          
          return("元大台灣50")
          
        }
        
        stock_info$name[
          match(
            symbol,
            stock_info$symbol
          )
        ]
        
      }
    )
    
    # --------------------------------------------------------
    # 整理欄位
    # --------------------------------------------------------
    
    latest <- latest %>%
      dplyr::select(
        symbol,
        stock_name,
        date,
        close,
        daily_change_pct,
        volume,
        volume_20d_pct
      ) %>%
      dplyr::arrange(
        symbol
      )
    
    latest
    
  })
  
  
  # ========================================================
  # 顯示 51 檔最新行情
  # ========================================================
  
  output$realtime_monitor_table <- renderTable({
    
    df <- realtime_monitor_data()
    
    req(
      nrow(df) > 0
    )
    
    # --------------------------------------------------------
    # 欄位名稱
    # --------------------------------------------------------
    
    names(df) <- c(
      "代號",
      "名稱",
      "日期",
      "收盤價",
      "漲跌幅_pct",
      "成交量",
      "近20日_pct"
    )
    
    # --------------------------------------------------------
    # 日期
    # --------------------------------------------------------
    
    df$日期 <- format(
      df$日期,
      "%Y-%m-%d"
    )
    
    # --------------------------------------------------------
    # 收盤價
    # --------------------------------------------------------
    
    df$收盤價 <- round(
      df$收盤價,
      2
    )
    
    # --------------------------------------------------------
    # 漲跌幅
    # --------------------------------------------------------
    
    df$漲跌幅_pct <- ifelse(
      is.finite(df$漲跌幅_pct),
      sprintf(
        "%+.2f%%",
        df$漲跌幅_pct
      ),
      "-"
    )
    
    # --------------------------------------------------------
    # 成交量
    # --------------------------------------------------------
    
    df$成交量 <- format(
      round(df$成交量),
      big.mark = ",",
      scientific = FALSE
    )
    
    # --------------------------------------------------------
    # 近 20 日成交量變化
    # --------------------------------------------------------
    
    df$近20日_pct <- ifelse(
      is.finite(df$近20日_pct),
      sprintf(
        "%+.2f%%",
        df$近20日_pct
      ),
      "-"
    )
    
    df
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE)
  
  
  # --------------------------------------------------------
  # 市場版本資訊／更新時間
  # --------------------------------------------------------
  
  market_version_url <- paste0(
    "https://raw.githubusercontent.com/",
    "bug330022-sys/stock-portfolio-dashboard/",
    "main/data_version.txt"
  )
  
  market_version_text <- reactivePoll(
    
    60000,
    
    session,
    
    checkFunc = function() {
      
      tryCatch(
        
        paste(
          readLines(
            market_version_url,
            warn = FALSE
          ),
          collapse = "\n"
        ),
        
        error = function(e) {
          ""
        }
        
      )
      
    },
    
    valueFunc = function() {
      
      tryCatch(
        
        paste(
          readLines(
            market_version_url,
            warn = FALSE
          ),
          collapse = "\n"
        ),
        
        error = function(e) {
          ""
        }
        
      )
      
    }
    
  )
  
  # ============================================================
  # 市場概況：資料更新時間
  # ============================================================
  
  output$market_data_update_time <- renderText({
    
    version_text <- market_version_text()
    
    req(
      !is.null(version_text),
      nzchar(version_text)
    )
    
    lines <- strsplit(
      version_text,
      "\n",
      fixed = TRUE
    )[[1]]
    
    lines <- trimws(lines)
    
    updated_line <- lines[
      grepl(
        "^updated_at=",
        lines
      )
    ]
    
    req(
      length(updated_line) > 0
    )
    
    updated_time <- sub(
      "^updated_at=",
      "",
      updated_line[1]
    )
    
    req(
      nzchar(updated_time)
    )
    
    updated_time
    
  })
  
  # --------------------------------------------------------
  # 市場觀察日期：自動跟隨最新資料
  # --------------------------------------------------------
  
  observeEvent(
    
    data(),
    
    {
      
      current_data <- data()
      
      req(
        nrow(current_data) > 0
      )
      
      current_trading_dates <- sort(
        unique(
          current_data$date
        )
      )
      
      current_min_date <- min(
        current_trading_dates
      )
      
      current_max_date <- max(
        current_trading_dates
      )
      
      selected_date <- isolate(
        input$market_observation_date
      )
      
      if (
        is.null(selected_date) ||
        length(selected_date) == 0
      ) {
        
        selected_date <- current_max_date
        
      } else {
        
        selected_date <- as.Date(
          selected_date
        )
        
        if (
          is.na(selected_date) ||
          !(selected_date %in% current_trading_dates)
        ) {
          
          selected_date <- current_max_date
          
        }
        
      }
      
  
      updateDateInput(
        
        session,
        
        "market_observation_date",
        
        value = selected_date,
        
        min = current_min_date,
        
        max = current_max_date
        
      )
      updateDateRangeInput(
        
        session,
        
        "market_chart_date_range",
        
        start = isolate(
          input$market_chart_date_range[1]
        ),
        
        end = isolate(
          input$market_chart_date_range[2]
        ),
        
        min = current_min_date,
        
        max = current_max_date
        
      )
      
    }
    
  )
  
  
  # --------------------------------------------------------
  # 市場觀察日資料
  # --------------------------------------------------------
  
  market_daily_data <- reactive({
    
    current_data <- data()
    
    req(
      nrow(current_data) > 0
    )
    
    current_data %>%
      dplyr::filter(
        symbol %in% c(
          "0050",
          stock_info$symbol
        )
      ) %>%
      dplyr::arrange(
        symbol,
        date
      ) %>%
      dplyr::group_by(
        symbol
      ) %>%
      dplyr::mutate(
        
        simple_return =
          adjusted / dplyr::lag(adjusted) - 1
        
      ) %>%
      dplyr::ungroup()
    
  })
  
  
  market_observation_data <- reactive({
    
    req(
      input$market_observation_date
    )
    
    obs_date <- as.Date(
      input$market_observation_date
    )
    
    df <- market_daily_data()
    
    df %>%
      dplyr::filter(
        date == obs_date
      )
    
  })
  
  
  # --------------------------------------------------------
  # 0050 最新調整後價格
  # --------------------------------------------------------
  
  output$market_0050_price <- renderText({
    
    df <- market_observation_data() %>%
      dplyr::filter(
        symbol == "0050"
      )
    
    req(
      nrow(df) > 0
    )
    
    sprintf(
      "%.2f",
      df$adjusted[1]
    )
    
  })
  
  
  # --------------------------------------------------------
  # 0050 最新日報酬率
  # --------------------------------------------------------
  
  output$market_0050_daily_return <- renderText({
    
    df <- market_observation_data() %>%
      dplyr::filter(
        symbol == "0050"
      )
    
    req(
      nrow(df) > 0
    )
    
    daily_return <- df$simple_return[1]
    
    if (
      is.na(daily_return) ||
      !is.finite(daily_return)
    ) {
      
      return("無資料")
      
    }
    
    sprintf(
      "%+.2f%%",
      daily_return * 100
    )
    
  })
  
  
  # --------------------------------------------------------
  # 0050 YTD
  # --------------------------------------------------------
  
  market_0050_ytd_data <- reactive({
    
    req(
      input$market_observation_date
    )
    
    obs_date <- as.Date(
      input$market_observation_date
    )
    
    df <- data() %>%
      dplyr::filter(
        symbol == "0050",
        date <= obs_date
      ) %>%
      dplyr::arrange(
        date
      )
    
    req(
      nrow(df) > 0
    )
    
    current_year <- as.integer(
      format(
        obs_date,
        "%Y"
      )
    )
    
    year_data <- df %>%
      dplyr::filter(
        as.integer(
          format(
            date,
            "%Y"
          )
        ) == current_year
      )
    
    req(
      nrow(year_data) > 0
    )
    
    start_price <- year_data$adjusted[1]
    
    end_price <- year_data$adjusted[
      nrow(year_data)
    ]
    
    end_price / start_price - 1
    
  })
  
  
  output$market_0050_ytd <- renderText({
    
    ytd <- market_0050_ytd_data()
    
    req(
      length(ytd) == 1
    )
    
    sprintf(
      "%+.2f%%",
      ytd * 100
    )
    
  })
  
  
  # --------------------------------------------------------
  # 50 檔成分股：上漲／下跌／持平
  # --------------------------------------------------------
  
  market_breadth_data <- reactive({
    
    df <- market_observation_data() %>%
      dplyr::filter(
        symbol %in% stock_info$symbol
      )
    
    req(
      nrow(df) > 0
    )
    
    up_count <- sum(
      df$simple_return > 0,
      na.rm = TRUE
    )
    
    down_count <- sum(
      df$simple_return < 0,
      na.rm = TRUE
    )
    
    flat_count <- sum(
      df$simple_return == 0,
      na.rm = TRUE
    )
    
    valid_count <- sum(
      is.finite(df$simple_return)
    )
    
    data.frame(
      up = up_count,
      down = down_count,
      flat = flat_count,
      valid = valid_count
    )
    
  })
  
  
  output$market_breadth <- renderText({
    
    df <- market_breadth_data()
    
    req(
      nrow(df) == 1
    )
    
    paste0(
      df$up,
      " / ",
      df$down,
      " / ",
      df$flat
    )
    
  })
  
  
  # --------------------------------------------------------
  # 0050 完整歷史價格
  # --------------------------------------------------------
  
  output$market_0050_price_chart <- renderPlot({
    
    req(
      input$market_chart_date_range
    )
    
    start_date <- as.Date(
      input$market_chart_date_range[1]
    )
    
    end_date <- as.Date(
      input$market_chart_date_range[2]
    )
    
    df <- data() %>%
      dplyr::filter(
        symbol == "0050",
        date >= start_date,
        date <= end_date
      ) %>%
      dplyr::arrange(
        date
      )
    
    req(
      nrow(df) > 1
    )
    
    plot(
      
      df$date,
      df$adjusted,
      
      type = "l",
      
      xlab = "日期",
      
      ylab = "調整後收盤價",
      
      main = "0050 調整後收盤價歷史走勢"
      
    )
    
  })
  
  
  # --------------------------------------------------------
  # 0050 近一年每日報酬率
  # --------------------------------------------------------
  
  market_0050_daily_return_timeseries_data <- reactive({
    
    req(
      input$market_observation_date
    )
    
    obs_date <- as.Date(
      input$market_observation_date
    )
    
    start_date <- obs_date - 365
    
    df <- market_daily_data() %>%
      dplyr::filter(
        symbol == "0050",
        date >= start_date,
        date <= obs_date
      ) %>%
      dplyr::arrange(
        date
      ) %>%
      dplyr::filter(
        is.finite(simple_return)
      )
    
    df
    
  })
  
  
  output$market_0050_daily_return_timeseries <- renderPlot({
    
    df <- market_0050_daily_return_timeseries_data()
    
    req(
      nrow(df) > 1
    )
    
    plot(
      
      df$date,
      
      df$simple_return * 100,
      
      type = "l",
      
      xlab = "日期",
      
      ylab = "每日報酬率（%）",
      
      main = "0050 近一年每日報酬率"
      
    )
    
    abline(
      h = 0,
      lty = 2
    )
    
  })
  
  # --------------------------------------------------------
  # 50 檔成分股：近 30 個交易日漲跌幅排名資料
  # --------------------------------------------------------
  
  market_30d_return_ranking_data <- reactive({
    
    req(
      input$market_observation_date
    )
    
    obs_date <- as.Date(
      input$market_observation_date
    )
    
    current_data <- data() %>%
      dplyr::filter(
        symbol %in% stock_info$symbol,
        date <= obs_date,
        is.finite(adjusted)
      ) %>%
      dplyr::arrange(
        symbol,
        date
      )
    
    req(
      nrow(current_data) > 0
    )
    
    result <- current_data %>%
      dplyr::group_by(
        symbol
      ) %>%
      dplyr::slice_tail(
        n = 31
      ) %>%
      dplyr::summarise(
        
        start_date = first(date),
        
        end_date = last(date),
        
        start_price = first(adjusted),
        
        end_price = last(adjusted),
        
        n = n(),
        
        return_30d =
          end_price /
          start_price -
          1,
        
        .groups = "drop"
        
      ) %>%
      dplyr::filter(
        n >= 31,
        is.finite(return_30d)
      )
    
    result$name <- stock_info$name[
      match(
        result$symbol,
        stock_info$symbol
      )
    ]
    
    result$name[
      is.na(result$name)
    ] <- "未設定"
    
    result
    
  })
  
  
  # --------------------------------------------------------
  # 50 檔成分股：近 30 個交易日漲跌幅排名
  # --------------------------------------------------------
  
  output$market_30d_return_ranking <- renderPlot({
    
    df <- market_30d_return_ranking_data()
    
    req(
      nrow(df) > 0
    )
    
    # ------------------------------------------------------
    # 漲幅前五名
    # ------------------------------------------------------
    
    top_up <- df %>%
      dplyr::arrange(
        dplyr::desc(return_30d)
      ) %>%
      dplyr::slice_head(
        n = 5
      )
    
    # ------------------------------------------------------
    # 跌幅前五名
    # ------------------------------------------------------
    
    top_down <- df %>%
      dplyr::arrange(
        return_30d
      ) %>%
      dplyr::slice_head(
        n = 5
      )
    
    # ------------------------------------------------------
    # 合併
    # ------------------------------------------------------
    
    plot_data <- dplyr::bind_rows(
      top_down,
      top_up
    ) %>%
      dplyr::distinct(
        symbol,
        .keep_all = TRUE
      ) %>%
      dplyr::arrange(
        return_30d
      )
    
    req(
      nrow(plot_data) > 0
    )
    
    plot_data$label <- paste(
      plot_data$symbol,
      plot_data$name
    )
    
    # ------------------------------------------------------
    # 判斷漲跌
    # ------------------------------------------------------
    
    plot_data$move <- dplyr::case_when(
      
      plot_data$return_30d > 0 ~ "上漲",
      
      plot_data$return_30d < 0 ~ "下跌",
      
      TRUE ~ "持平"
      
    )
    
    # ------------------------------------------------------
    # 繪圖
    # ------------------------------------------------------
    
    ggplot(
      plot_data,
      aes(
        x = return_30d * 100,
        y = reorder(
          label,
          return_30d
        ),
        fill = move
      )
    ) +
      
      geom_col(
        width = 0.65
      ) +
      
      geom_vline(
        xintercept = 0,
        linetype = "dashed",
        linewidth = 0.7
      ) +
      
      geom_text(
        aes(
          label = paste0(
            ifelse(
              return_30d > 0,
              "+",
              ""
            ),
            sprintf(
              "%.2f%%",
              return_30d * 100
            )
          )
        ),
        hjust = ifelse(
          plot_data$return_30d >= 0,
          -0.15,
          1.15
        ),
        size = 4
      ) +
      
      scale_fill_manual(
        values = c(
          "上漲" = "#C97878",
          "下跌" = "#78A88C",
          "持平" = "#A8A8A8"
        )
      ) +
      
      scale_x_continuous(
        expand = expansion(
          mult = c(
            0.15,
            0.20
          )
        )
      ) +
      
      labs(
        x = "近 30 個交易日累積漲跌幅（%）",
        y = NULL,
        fill = NULL,
        title = paste0(
          "近 30 個交易日漲跌幅排名（截至 ",
          format(
            max(plot_data$end_date),
            "%Y-%m-%d"
          ),
          "）"
        )
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title = element_text(
          hjust = 0.5
        ),
        axis.text.y = element_text(
          size = 11
        ),
        legend.position = "bottom",
        panel.grid.major.y = element_blank()
      )
    
  })
  
  
  # ============================================================
  # 投資組合個別交易設定
  # ============================================================
  
  output$portfolio_trade_inputs <- renderUI({
    
    req(input$portfolio_stock)
    req(length(input$portfolio_stock) > 0)
    req(input$portfolio_trade_mode)
    
    current_data <- data()
    
    # ----------------------------------------------------------
    # 每一檔股票建立自己的交易設定
    # ----------------------------------------------------------
    
    tagList(
      
      lapply(
        
        input$portfolio_stock,
        
        function(symbol) {
          
          stock_name <- ifelse(
            symbol == "0050",
            "元大台灣50",
            stock_info$name[
              match(
                symbol,
                stock_info$symbol
              )
            ]
          )
          
          # ------------------------------------------------------
          # 股票實際資料範圍
          # ------------------------------------------------------
          
          stock_data <- current_data[
            current_data$symbol == symbol &
              is.finite(current_data$adjusted),
          ]
          
          req(
            nrow(stock_data) > 0
          )
          
          stock_data <- stock_data[
            order(stock_data$date),
          ]
          
          # ------------------------------------------------------
          # 股票真正存在的交易日
          # ------------------------------------------------------
          
          trading_dates <- sort(
            unique(
              stock_data$date
            )
          )
          
          req(
            length(trading_dates) > 0
          )
          
          # ------------------------------------------------------
          # 建立完整日曆
          # ------------------------------------------------------
          
          calendar_dates <- seq(
            from = min(trading_dates),
            to = max(trading_dates),
            by = "day"
          )
          
          # ------------------------------------------------------
          # 不在股票資料中的日期全部禁止選擇
          #
          # 包含：
          # 星期六
          # 星期日
          # 台股休市日
          # 該股票沒有資料的日期
          # ------------------------------------------------------
          
          disabled_dates <- setdiff(
            calendar_dates,
            trading_dates
          )
          
          # ------------------------------------------------------
          # 建立單一股票設定區
          # ------------------------------------------------------
          
          tagList(
            
            h4(
              paste(
                symbol,
                stock_name
              )
            ),
            
            fluidRow(
              
              column(
                width = 4,
                
                dateInput(
                  
                  inputId = paste0(
                    "portfolio_buy_date_",
                    symbol
                  ),
                  
                  label = "買入日期：",
                  
                  value = min(
                    trading_dates
                  ),
                  
                  min = min(
                    trading_dates
                  ),
                  
                  max = max(
                    trading_dates
                  ),
                  
                  format = "yyyy-mm-dd",
                  
                  language = "zh-TW",
                  
                  weekstart = 1,
                  
                  datesdisabled = disabled_dates,
                  
                  daysofweekdisabled = c(
                    0,
                    6
                  )
                  
                )
                
              ),
              
              column(
                width = 4,
                
                dateInput(
                  
                  inputId = paste0(
                    "portfolio_sell_date_",
                    symbol
                  ),
                  
                  label = "賣出日期：",
                  
                  value = max(
                    trading_dates
                  ),
                  
                  min = min(
                    trading_dates
                  ),
                  
                  max = max(
                    trading_dates
                  ),
                  
                  format = "yyyy-mm-dd",
                  
                  language = "zh-TW",
                  
                  weekstart = 1,
                  
                  datesdisabled = disabled_dates,
                  
                  daysofweekdisabled = c(
                    0,
                    6
                  )
                  
                )
                
              ),
              
              column(
                width = 4,
                
                checkboxInput(
                  
                  inputId = paste0(
                    "portfolio_hold_",
                    symbol
                  ),
                  
                  label = "持有中",
                  
                  value = TRUE
                  
                )
                
              )
              
            ),
            
            fluidRow(
              
              column(
                width = 4,
                
                selectInput(
                  
                  inputId = paste0(
                    "portfolio_unit_",
                    symbol
                  ),
                  
                  label = "交易單位：",
                  
                  choices = c(
                    "股" = 1,
                    "張" = 1000
                  ),
                  
                  selected = 1
                  
                )
                
              ),
              
              column(
                width = 8,
                
                if (
                  input$portfolio_trade_mode ==
                  "以投入金額設定"
                ) {
                  
                  numericInput(
                    
                    inputId = paste0(
                      "portfolio_amount_",
                      symbol
                    ),
                    
                    label = "投入金額（元）：",
                    
                    value = 100000,
                    
                    min = 1,
                    
                    step = 1000
                    
                  )
                  
                } else {
                  
                  numericInput(
                    
                    inputId = paste0(
                      "portfolio_quantity_",
                      symbol
                    ),
                    
                    label = "購買數量：",
                    
                    value = 1,
                    
                    min = 1,
                    
                    step = 1
                    
                  )
                  
                }
                
              )
              
            ),
            
            hr()
            
          )
          
        }
        
      )
      
    )
    
  })
  
  
  
  # ============================================================
  # 投資組合交易資料
  # ============================================================
  
  portfolio_trades <- reactive({
    
    req(input$portfolio_stock)
    req(length(input$portfolio_stock) > 0)
    
    req(
      input$portfolio_trade_mode
    )
    
    req(
      input$portfolio_trade_mode %in%
        c(
          "以投入金額設定",
          "以持有股數設定"
        )
    )
    
    current_data <- data()
    
    # ----------------------------------------------------------
    # 交易價格
    #
    # 實際交易模擬優先使用 close
    # 若資料不存在 close，才使用 adjusted
    # ----------------------------------------------------------
    
    price_column <- if (
      "close" %in% names(current_data)
    ) {
      "close"
    } else {
      "adjusted"
    }
    
    # ----------------------------------------------------------
    # 費用參數
    # ----------------------------------------------------------
    
    brokerage_rate <- as.numeric(
      input$brokerage_rate
    ) / 100
    
    minimum_fee <- as.numeric(
      input$minimum_fee
    )
    
    stock_tax_rate <- as.numeric(
      input$stock_tax_rate
    ) / 100
    
    etf_tax_rate <- as.numeric(
      input$etf_tax_rate
    ) / 100
    
    # ----------------------------------------------------------
    # 手續費函數
    # ----------------------------------------------------------
    
    calculate_fee <- function(
    trade_value
    ) {
      
      if (
        !is.finite(trade_value) ||
        trade_value <= 0
      ) {
        
        return(0)
        
      }
      
      max(
        trade_value * brokerage_rate,
        minimum_fee
      )
      
    }
    
    # ----------------------------------------------------------
    # 最大可購買股數
    #
    # 以投入金額模式使用
    # ----------------------------------------------------------
    
    calculate_max_shares <- function(
    budget,
    price,
    unit_size
    ) {
      
      if (
        !is.finite(budget) ||
        !is.finite(price) ||
        budget <= 0 ||
        price <= 0
      ) {
        
        return(0)
        
      }
      
      # --------------------------------------------------------
      # 先估計最大股數
      # --------------------------------------------------------
      
      candidate_1 <- if (
        budget > minimum_fee
      ) {
        
        floor(
          (budget - minimum_fee) /
            price /
            unit_size
        ) * unit_size
        
      } else {
        
        0
        
      }
      
      candidate_2 <- floor(
        budget /
          (price * (1 + brokerage_rate)) /
          unit_size
      ) * unit_size
      
      shares <- max(
        candidate_1,
        candidate_2,
        0
      )
      
      # --------------------------------------------------------
      # 最後確認實際成本不超過預算
      # --------------------------------------------------------
      
      while (
        shares > 0
      ) {
        
        gross_value <- shares * price
        
        fee <- calculate_fee(
          gross_value
        )
        
        total_cost <- gross_value + fee
        
        if (
          total_cost <= budget
        ) {
          
          break
          
        }
        
        shares <- shares - unit_size
        
      }
      
      shares
      
    }
    
    # ----------------------------------------------------------
    # 每檔股票計算交易結果
    # ----------------------------------------------------------
    
    result <- lapply(
      
      input$portfolio_stock,
      
      function(symbol) {
        
        stock_name <- ifelse(
          symbol == "0050",
          "元大台灣50",
          stock_info$name[
            match(
              symbol,
              stock_info$symbol
            )
          ]
        )
        
        # ------------------------------------------------------
        # 取得股票資料
        # ------------------------------------------------------
        
        stock_data <- current_data[
          current_data$symbol == symbol &
            is.finite(
              as.numeric(
                current_data[[price_column]]
              )
            ),
        ]
        
        stock_data <- stock_data[
          order(stock_data$date),
        ]
        
        if (
          nrow(stock_data) == 0
        ) {
          
          return(
            data.frame(
              股票代號 = symbol,
              股票名稱 = stock_name,
              狀態 = "無可用價格資料",
              stringsAsFactors = FALSE
            )
          )
          
        }
        
        stock_data$trade_price <- as.numeric(
          stock_data[[price_column]]
        )
        

        
        # ------------------------------------------------------
        # 買入日期
        # ------------------------------------------------------
        # ------------------------------------------------------
        # 買入日期
        # ------------------------------------------------------
        
        buy_input_id <- paste0(
          "portfolio_buy_date_",
          symbol
        )
        
        buy_request_raw <- input[[buy_input_id]]
        
        # ------------------------------------------------------
        # 檢查買入日期輸入是否已經建立
        # ------------------------------------------------------
        
        if (
          is.null(buy_request_raw) ||
          length(buy_request_raw) == 0
        ) {
          
          return(
            data.frame(
              股票代號 = symbol,
              股票名稱 = stock_name,
              狀態 = "尚未設定買入日期",
              stringsAsFactors = FALSE
            )
          )
          
        }
        
        # ------------------------------------------------------
        # 轉換為 Date
        # ------------------------------------------------------
        
        buy_request <- as.Date(
          buy_request_raw
        )
        
        # ------------------------------------------------------
        # 檢查是否為有效日期
        # ------------------------------------------------------
        
        if (
          length(buy_request) != 1
        ) {
          
          return(
            data.frame(
              股票代號 = symbol,
              股票名稱 = stock_name,
              狀態 = "買入日期格式錯誤",
              stringsAsFactors = FALSE
            )
          )
          
        }
        
        if (
          is.na(buy_request)
        ) {
          
          return(
            data.frame(
              股票代號 = symbol,
              股票名稱 = stock_name,
              狀態 = "尚未選擇有效買入日期",
              stringsAsFactors = FALSE
            )
          )
          
        }
        
        # ------------------------------------------------------
        # 使用者選擇的日期必須為實際交易日
        # ------------------------------------------------------
        
        if (
          !(buy_request %in% stock_data$date)
        ) {
          
          return(
            data.frame(
              股票代號 = symbol,
              股票名稱 = stock_name,
              狀態 = "買入日期不是有效交易日",
              stringsAsFactors = FALSE
            )
          )
          
        }
        
        buy_date <- buy_request
        
        buy_price <- stock_data$trade_price[
          stock_data$date == buy_date
        ][1]
        
        # ------------------------------------------------------
        # 交易單位
        # ------------------------------------------------------
        unit_size <- as.numeric(
          input[[
            paste0(
              "portfolio_unit_",
              symbol
            )
          ]]
        )
        
        req(
          is.finite(unit_size)
        )
        
        req(
          unit_size %in% c(
            1,
            1000
          )
        )
        
        unit_name <- if (
          unit_size == 1000
        ) {
          "張"
        } else {
          "股"
        }
        
        # ------------------------------------------------------
        # 計算購買股數
        # ------------------------------------------------------
        
        if (
          input$portfolio_trade_mode ==
          "以投入金額設定"
        ) {
          
          budget <- as.numeric(
            input[[
              paste0(
                "portfolio_amount_",
                symbol
              )
            ]]
          )
          
          req(
            is.finite(budget)
          )
          
          req(
            budget > 0
          )
          
          shares <- calculate_max_shares(
            budget = budget,
            price = buy_price,
            unit_size = unit_size
          )
          
        } else {
          
          quantity <- as.numeric(
            input[[
              paste0(
                "portfolio_quantity_",
                symbol
              )
            ]]
          )
          
          req(
            is.finite(quantity)
          )
          
          req(
            quantity > 0
          )
          
          shares <- quantity * unit_size
          
          budget <- NA
          
        }
        
        # ------------------------------------------------------
        # 沒有買到股票
        # ------------------------------------------------------
        
        if (
          shares <= 0
        ) {
          
          return(
            data.frame(
              股票代號 = symbol,
              股票名稱 = stock_name,
              狀態 = "投入金額不足以購買 1 個設定單位",
              stringsAsFactors = FALSE
            )
          )
          
        }
        
        # ------------------------------------------------------
        # 買入成本
        # ------------------------------------------------------
        
        buy_gross <- shares *
          buy_price
        
        buy_fee <- calculate_fee(
          buy_gross
        )
        
        buy_total <- buy_gross +
          buy_fee
        # ------------------------------------------------------
        # 配置資金與未使用現金
        # ------------------------------------------------------
        
        allocated_capital <- if (
          input$portfolio_trade_mode ==
          "以投入金額設定"
        ) {
          
          budget
          
        } else {
          
          buy_total
          
        }
        
        unused_cash <- if (
          input$portfolio_trade_mode ==
          "以投入金額設定"
        ) {
          
          max(
            budget - buy_total,
            0
          )
          
        } else {
          
          0
          
        }
        # ------------------------------------------------------
        # 判斷是否持有中
        # ------------------------------------------------------
        
        holding <- isTRUE(
          input[[
            paste0(
              "portfolio_hold_",
              symbol
            )
          ]]
        )
        
        # ------------------------------------------------------
        # 賣出 / 目前估值
        # ------------------------------------------------------
        
        if (
          holding
        ) {
          # ----------------------------------------------------
          # 持有中 → 使用該股票最新有效交易日估值
          # 不受全域研究期間影響
          # ----------------------------------------------------
          valuation_date <- max(
            stock_data$date
          )
          
          valuation_price <- stock_data$trade_price[
            stock_data$date == valuation_date
          ][1]
          
          gross_value <- shares *
            valuation_price
          
          sell_fee <- calculate_fee(
            gross_value
          )
          
          # 0050 = ETF
          tax_rate <- if (
            symbol == "0050"
          ) {
            etf_tax_rate
          } else {
            stock_tax_rate
          }
          
          sell_tax <- gross_value *
            tax_rate
          
          net_value <- gross_value -
            sell_fee -
            sell_tax
          
          profit <- net_value -
            buy_total
          
          return_rate <- profit /
            buy_total
          
          result_status <- "持有中"
          
          final_date <- valuation_date
          
          final_price <- valuation_price
          
        } else {
          
          # ----------------------------------------------------
          # 已賣出
          # ----------------------------------------------------
          # ------------------------------------------------------
          # 賣出日期
          # ------------------------------------------------------
          
          sell_input_id <- paste0(
            "portfolio_sell_date_",
            symbol
          )
          
          sell_request_raw <- input[[sell_input_id]]
          
          # ------------------------------------------------------
          # 檢查賣出日期輸入是否已經建立
          # ------------------------------------------------------
          
          if (
            is.null(sell_request_raw) ||
            length(sell_request_raw) == 0
          ) {
            
            return(
              data.frame(
                股票代號 = symbol,
                股票名稱 = stock_name,
                狀態 = "尚未設定賣出日期",
                stringsAsFactors = FALSE
              )
            )
            
          }
          
          # ------------------------------------------------------
          # 轉換為 Date
          # ------------------------------------------------------
          
          sell_request <- as.Date(
            sell_request_raw
          )
          
          # ------------------------------------------------------
          # 檢查是否為單一有效日期
          # ------------------------------------------------------
          
          if (
            length(sell_request) != 1
          ) {
            
            return(
              data.frame(
                股票代號 = symbol,
                股票名稱 = stock_name,
                狀態 = "賣出日期格式錯誤",
                stringsAsFactors = FALSE
              )
            )
            
          }
          
          if (
            is.na(sell_request)
          ) {
            
            return(
              data.frame(
                股票代號 = symbol,
                股票名稱 = stock_name,
                狀態 = "尚未選擇有效賣出日期",
                stringsAsFactors = FALSE
              )
            )
            
          }
          
          # ------------------------------------------------------
          # 使用者選擇的日期必須為實際交易日
          # ------------------------------------------------------
          
          if (
            !(sell_request %in% stock_data$date)
          ) {
            
            return(
              data.frame(
                股票代號 = symbol,
                股票名稱 = stock_name,
                狀態 = "賣出日期不是有效交易日",
                stringsAsFactors = FALSE
              )
            )
            
          }
          
          sell_date <- sell_request
          
          # ----------------------------------------------------
          # 賣出日必須晚於買入日
          # ----------------------------------------------------
          
          if (
            sell_date <= buy_date
          ) {
            
            return(
              data.frame(
                股票代號 = symbol,
                股票名稱 = stock_name,
                狀態 = "賣出日期必須晚於買入日期",
                stringsAsFactors = FALSE
              )
            )
            
          }
          
          sell_price <- stock_data$trade_price[
            stock_data$date == sell_date
          ][1]
          
          gross_value <- shares *
            sell_price
          
          sell_fee <- calculate_fee(
            gross_value
          )
          
          # ----------------------------------------------------
          # 0050 = ETF
          # ----------------------------------------------------
          
          tax_rate <- if (
            symbol == "0050"
          ) {
            etf_tax_rate
          } else {
            stock_tax_rate
          }
          
          sell_tax <- gross_value *
            tax_rate
          
          net_value <- gross_value -
            sell_fee -
            sell_tax
          
          profit <- net_value -
            buy_total
          
          return_rate <- profit /
            buy_total
          
          result_status <- "已賣出"
          
          final_date <- sell_date
          
          final_price <- sell_price
          
        }
        
        # ------------------------------------------------------
        # 建立結果
        # ------------------------------------------------------
        
        data.frame(
          
          股票代號 = symbol,
          
          股票名稱 = stock_name,
          
          狀態 = result_status,
          
          買入日期 = buy_date,
          
          買入價格 = buy_price,
          
          交易單位 = unit_name,
          
          股數 = shares,
          
          買入總金額 = buy_gross,
          
          買入手續費 = buy_fee,
          
          實際投入資金 = buy_total,
          
          配置資金 = allocated_capital,
          
          未使用現金 = unused_cash,
          
          結算日期 = final_date,
          
          結算價格 = final_price,
          
          結算總金額 = gross_value,
          
          賣出手續費 = sell_fee,
          
          證交稅 = sell_tax,
          
          結算後金額 = net_value,
          
          損益 = profit,
          
          報酬率 = return_rate,
          
          stringsAsFactors = FALSE
          
        )
        
      }
      
    )
    
    valid_results <- result[
      sapply(
        result,
        function(x) {
          "損益" %in% names(x)
        }
      )
    ]
    
    if (
      length(valid_results) == 0
    ) {
      
      return(
        data.frame(
          股票代號 = character(),
          股票名稱 = character(),
          狀態 = character(),
          stringsAsFactors = FALSE
        )
      )
      
    }
    
    do.call(
      rbind,
      valid_results
    )
    
  })
  
  # ============================================================
  # 投資組合交易明細
  # ============================================================
  
  output$portfolio_trade_table <- renderTable({
    
    x <- portfolio_trades()
    
    req(
      nrow(x) > 0
    )
    
    display_data <- x
    
    # ----------------------------------------------------------
    # 日期格式
    # ----------------------------------------------------------
    
    display_data$買入日期 <- format(
      display_data$買入日期,
      "%Y-%m-%d"
    )
    
    display_data$結算日期 <- format(
      display_data$結算日期,
      "%Y-%m-%d"
    )
    
    # ----------------------------------------------------------
    # 數字格式
    # ----------------------------------------------------------
    
    display_data$買入價格 <- round(
      display_data$買入價格,
      2
    )
    
    display_data$結算價格 <- round(
      display_data$結算價格,
      2
    )
    
    display_data$股數 <- format(
      display_data$股數,
      big.mark = ",",
      scientific = FALSE
    )
    
    display_data$配置資金 <- round(
      display_data$配置資金,
      2
    )
    
    display_data$實際投入資金 <- round(
      display_data$實際投入資金,
      2
    )
    
    display_data$未使用現金 <- round(
      display_data$未使用現金,
      2
    )
    
    display_data$結算後金額 <- round(
      display_data$結算後金額,
      2
    )
    
    display_data$買入手續費 <- round(
      display_data$買入手續費,
      2
    )
    
    display_data$賣出手續費 <- round(
      display_data$賣出手續費,
      2
    )
    
    display_data$證交稅 <- round(
      display_data$證交稅,
      2
    )
    
    display_data$損益 <- round(
      display_data$損益,
      2
    )
    
    display_data$報酬率 <- paste0(
      sprintf(
        "%+.2f",
        display_data$報酬率 * 100
      ),
      "%"
    )
    
    # ----------------------------------------------------------
    # 只顯示 Dashboard 真正需要的欄位
    # ----------------------------------------------------------
    display_data[
      ,
      c(
        "股票代號",
        "股票名稱",
        "狀態",
        "買入日期",
        "買入價格",
        "交易單位",
        "股數",
        "配置資金",
        "實際投入資金",
        "未使用現金",
        "買入手續費",
        "結算日期",
        "結算價格",
        "結算後金額",
        "賣出手續費",
        "證交稅",
        "損益",
        "報酬率"
      )
    ]
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE)
  
  
  # ============================================================
  # 投資組合績效摘要
  # ============================================================
  
  output$portfolio_summary <- renderTable({
    
    x <- portfolio_trades()
    
    req(
      nrow(x) > 0
    )
    
    # ----------------------------------------------------------
    # 實際投入資金
    # ----------------------------------------------------------
    
    total_invested <- sum(
      x$實際投入資金,
      na.rm = TRUE
    )
    
    # ----------------------------------------------------------
    # 初始配置資金
    #
    # 金額模式：
    # 使用者設定的總配置資金
    #
    # 數量模式：
    # 使用實際投入資金
    # ----------------------------------------------------------
    
    initial_capital <- if (
      input$portfolio_trade_mode ==
      "以投入金額設定"
    ) {
      
      sum(
        x$配置資金,
        na.rm = TRUE
      )
      
    } else {
      
      total_invested
      
    }
    
    # ----------------------------------------------------------
    # 未使用現金
    # ----------------------------------------------------------
    
    total_unused_cash <- sum(
      x$未使用現金,
      na.rm = TRUE
    )
    
    # ----------------------------------------------------------
    # 最終 / 目前淨值
    # ----------------------------------------------------------
    
    total_value <- sum(
      x$結算後金額,
      na.rm = TRUE
    ) +
      total_unused_cash
    
    # ----------------------------------------------------------
    # 總損益
    #
    # 以初始配置資金為基準
    # ----------------------------------------------------------
    
    total_profit <- total_value -
      initial_capital
    
    # ----------------------------------------------------------
    # 總報酬率
    #
    # 投資組合報酬率 =
    # 總損益 / 初始配置資金
    # ----------------------------------------------------------
    
    total_return <- if (
      initial_capital > 0
    ) {
      
      total_profit /
        initial_capital
      
    } else {
      
      NA
      
    }
    
    # ----------------------------------------------------------
    # 交易成本
    # ----------------------------------------------------------
    
    total_buy_fee <- sum(
      x$買入手續費,
      na.rm = TRUE
    )
    
    total_sell_fee <- sum(
      x$賣出手續費,
      na.rm = TRUE
    )
    
    total_tax <- sum(
      x$證交稅,
      na.rm = TRUE
    )
    
    # ----------------------------------------------------------
    # 建立摘要
    # ----------------------------------------------------------
    
    data.frame(
      
      投資標的數 = nrow(x),
      
      配置資金 = round(
        initial_capital,
        2
      ),
      
      實際投入資金 = round(
        total_invested,
        2
      ),
      
      未使用現金 = round(
        total_unused_cash,
        2
      ),
      
      目前最終淨值 = round(
        total_value,
        2
      ),
      
      總損益 = paste0(
        ifelse(
          total_profit >= 0,
          "+",
          ""
        ),
        format(
          round(
            total_profit,
            2
          ),
          big.mark = ",",
          scientific = FALSE
        ),
        " 元"
      ),
      
      總報酬率 = paste0(
        ifelse(
          total_return >= 0,
          "+",
          ""
        ),
        round(
          total_return * 100,
          2
        ),
        "%"
      ),
      
      買入手續費 = round(
        total_buy_fee,
        2
      ),
      
      賣出手續費 = round(
        total_sell_fee,
        2
      ),
      
      證交稅 = round(
        total_tax,
        2
      ),
      
      stringsAsFactors = FALSE
      
    )
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE)
  
  
  observeEvent(
    portfolio_trades(),
    {
      trades <- portfolio_trades()
      
      req(
        nrow(trades) > 0
      )
      
      req(
        "買入日期" %in% names(trades)
      )
      
      # --------------------------------------------------------
      # 所有有效買入日期
      # --------------------------------------------------------
      
      buy_dates <- as.Date(
        trades$買入日期
      )
      
      buy_dates <- buy_dates[
        !is.na(buy_dates)
      ]
      
      req(
        length(buy_dates) > 0
      )
      
      # --------------------------------------------------------
      # 圖表可用日期範圍
      #
      # 最早買入日
      # 到目前資料的最新交易日
      # --------------------------------------------------------
      
      chart_start <- min(
        buy_dates
      )
      
      chart_end <- max(
        data()$date,
        na.rm = TRUE
      )
      
      req(
        !is.na(chart_start),
        !is.na(chart_end)
      )
      
      req(
        chart_start <= chart_end
      )
      
      # --------------------------------------------------------
      # 更新投資組合價值圖表日期選擇器
      # --------------------------------------------------------
      
      updateDateRangeInput(
        session,
        "portfolio_value_date_range",
        start = chart_start,
        end = chart_end,
        min = chart_start,
        max = chart_end
      )
    },
    ignoreInit = FALSE
  )
  
  # ============================================================
  # 投資組合每日資產價值
  # ============================================================
  
  portfolio_daily_value <- reactive({
    
    # ----------------------------------------------------------
    # 交易結果
    # ----------------------------------------------------------
    
    
    trades <- portfolio_trades()
    
    req(
      nrow(trades) > 0
    )
    
    # ----------------------------------------------------------
    # 只保留成功建立交易資料的股票
    # ----------------------------------------------------------
    
    req(
      "股數" %in% names(trades)
    )
    
    # ----------------------------------------------------------
    # 取得行情資料
    # ----------------------------------------------------------
    
    current_data <- data()
    
    price_column <- if (
      "close" %in% names(current_data)
    ) {
      
      "close"
      
    } else {
      
      "adjusted"
      
    }
    
    # ----------------------------------------------------------
    # 費用參數
    # ----------------------------------------------------------
    
    brokerage_rate <- as.numeric(
      input$brokerage_rate
    ) / 100
    
    minimum_fee <- as.numeric(
      input$minimum_fee
    )
    
    stock_tax_rate <- as.numeric(
      input$stock_tax_rate
    ) / 100
    
    etf_tax_rate <- as.numeric(
      input$etf_tax_rate
    ) / 100
    
    # ----------------------------------------------------------
    # 手續費函數
    # ----------------------------------------------------------
    
    calculate_fee <- function(
    trade_value
    ) {
      
      if (
        !is.finite(trade_value) ||
        trade_value <= 0
      ) {
        
        return(0)
        
      }
      
      max(
        trade_value * brokerage_rate,
        minimum_fee
      )
      
    }
    
    # ----------------------------------------------------------
    # 投資組合完整績效期間
    #
    # 由最早買入日開始，
    # 一直到目前分析期間的結束日。
    #
    # 圖表 Range 不影響這裡的完整資料計算。
    # ----------------------------------------------------------
    
    buy_dates <- as.Date(
      trades$買入日期
    )
    
    buy_dates <- buy_dates[
      !is.na(buy_dates)
    ]
    
    req(
      length(buy_dates) > 0
    )
    
    portfolio_start_date <- min(
      buy_dates
    )
    
    portfolio_end_date <- max(
      current_data$date,
      na.rm = TRUE
    )
    
    req(
      !is.na(portfolio_end_date)
    )
    
    req(
      portfolio_start_date <= portfolio_end_date
    )
    
    # ----------------------------------------------------------
    # 取得完整投資期間的交易日
    # ----------------------------------------------------------
    
    dates <- sort(
      unique(
        current_data$date[
          current_data$date >= portfolio_start_date &
            current_data$date <= portfolio_end_date
        ]
      )
    )
    
    req(
      length(dates) > 0
    )
    
    req(
      length(dates) > 0
    )
    
    # ----------------------------------------------------------
    # 初始配置資金
    #
    # 金額模式：
    # 使用者設定的配置資金
    #
    # 數量模式：
    # 實際投入資金
    # ----------------------------------------------------------
    
    initial_cash <- if (
      input$portfolio_trade_mode ==
      "以投入金額設定"
    ) {
      
      sum(
        trades$配置資金,
        na.rm = TRUE
      )
      
    } else {
      
      sum(
        trades$實際投入資金,
        na.rm = TRUE
      )
      
    }
    
    # ----------------------------------------------------------
    # 每一個交易日計算投資組合價值
    # ----------------------------------------------------------
    
    result <- lapply(
      dates,
      function(current_date) {
        
        # ========================================================
        # 當日現金
        #
        # 初始配置資金
        # - 已經買入的股票成本
        # + 已經完成賣出的股票結算金額
        # ========================================================
        
        cash <- initial_cash
        
        # ========================================================
        # 當日持股淨清算價值
        # ========================================================
        
        holdings_value <- 0
        
        # ========================================================
        # 逐檔股票處理
        # ========================================================
        
        for (
          i in seq_len(nrow(trades))
        ) {
          
          symbol <- trades$股票代號[i]
          
          buy_date <- as.Date(
            trades$買入日期[i]
          )
          
          shares <- as.numeric(
            trades$股數[i]
          )
          
          buy_cost <- as.numeric(
            trades$實際投入資金[i]
          )
          
          status <- trades$狀態[i]
          
          # ------------------------------------------------------
          # 尚未買入
          # ------------------------------------------------------
          
          if (
            current_date < buy_date
          ) {
            
            next
            
          }
          
          # ------------------------------------------------------
          # 已經買入
          # → 扣除實際買入成本
          # ------------------------------------------------------
          
          cash <- cash - buy_cost
          
          # ------------------------------------------------------
          # 取得截至當日最近一筆有效價格
          # ------------------------------------------------------
          
          stock_data <- current_data[
            current_data$symbol == symbol &
              current_data$date <= current_date &
              is.finite(
                current_data[[price_column]]
              ),
          ]
          
          stock_data <- stock_data[
            order(stock_data$date),
          ]
          
          if (
            nrow(stock_data) == 0
          ) {
            
            next
            
          }
          
          current_price <- tail(
            stock_data[[price_column]],
            1
          )
          
          current_price <- as.numeric(
            current_price
          )
          
          # ------------------------------------------------------
          # 假設目前清算持股
          # ------------------------------------------------------
          
          gross_value <- shares *
            current_price
          
          sell_fee <- calculate_fee(
            gross_value
          )
          
          tax_rate <- if (
            symbol == "0050"
          ) {
            
            etf_tax_rate
            
          } else {
            
            stock_tax_rate
            
          }
          
          sell_tax <- gross_value *
            tax_rate
          
          net_market_value <- gross_value -
            sell_fee -
            sell_tax
          
          # ======================================================
          # 已賣出
          # ======================================================
          
          if (
            status == "已賣出"
          ) {
            
            sell_date <- as.Date(
              trades$結算日期[i]
            )
            
            # ----------------------------------------------------
            # 賣出前
            # → 仍然持有股票
            # ----------------------------------------------------
            
            if (
              current_date < sell_date
            ) {
              
              holdings_value <- holdings_value +
                net_market_value
              
            }
            
            # ----------------------------------------------------
            # 賣出日及之後
            # → 不再計入持股
            # → 加入實際賣出所得
            #
            # 注意：
            # 因為每一天重新計算，
            # 所以這裡只要加一次「該筆交易的結算金額」，
            # 不會累積到下一天。
            # ----------------------------------------------------
            
            if (
              current_date >= sell_date
            ) {
              
              cash <- cash +
                as.numeric(
                  trades$結算後金額[i]
                )
              
            }
            
          } else {
            
            # ====================================================
            # 持有中
            # ====================================================
            
            holdings_value <- holdings_value +
              net_market_value
            
          }
          
        }
        
        # ========================================================
        # 投資組合總資產價值
        # ========================================================
        
        total_value <- cash +
          holdings_value
        
        data.frame(
          
          date = current_date,
          
          cash = cash,
          
          holdings_value = holdings_value,
          
          portfolio_value = total_value,
          
          stringsAsFactors = FALSE
          
        )
        
      }
    )
    
    do.call(
      rbind,
      result
    )
    
  })
  
  # ============================================================
  # 投資組合績效與風險指標
  # ============================================================
  
  portfolio_performance_metrics <- reactive({
    
    # ----------------------------------------------------------
    # 完整投資組合每日資產價值
    # ----------------------------------------------------------
    
    x <- portfolio_daily_value()
    
    req(
      nrow(x) > 0
    )
    
    # ----------------------------------------------------------
    # 交易資料
    # ----------------------------------------------------------
    
    trades <- portfolio_trades()
    
    req(
      nrow(trades) > 0
    )
    
    # ----------------------------------------------------------
    # 初始配置資金
    # ----------------------------------------------------------
    
    initial_capital <- if (
      input$portfolio_trade_mode ==
      "以投入金額設定"
    ) {
      
      sum(
        trades$配置資金,
        na.rm = TRUE
      )
      
    } else {
      
      sum(
        trades$實際投入資金,
        na.rm = TRUE
      )
      
    }
    
    req(
      is.finite(initial_capital)
    )
    
    req(
      initial_capital > 0
    )
    
    # ----------------------------------------------------------
    # 日期
    # ----------------------------------------------------------
    
    start_date <- min(
      x$date
    )
    
    end_date <- max(
      x$date
    )
    
    holding_days <- as.numeric(
      end_date - start_date
    )
    
    calendar_years <- holding_days /
      365.25
    
    # ----------------------------------------------------------
    # 最終投資組合價值
    # ----------------------------------------------------------
    
    final_value <- tail(
      x$portfolio_value,
      1
    )
    
    # ----------------------------------------------------------
    # 年化報酬率
    #
    # 以初始配置資金到最終淨值計算
    # ----------------------------------------------------------
    
    annual_return <- if (
      calendar_years > 0 &&
      final_value > 0
    ) {
      
      (
        final_value /
          initial_capital
      ) ^ (
        1 /
          calendar_years
      ) - 1
      
    } else {
      
      NA
      
    }
    
    # ----------------------------------------------------------
    # 每日投資組合報酬率
    #
    # 第一天：
    # 初始配置資金 → 第一天期末資產
    #
    # 後續：
    # 當日資產 / 前一日資產 - 1
    # ----------------------------------------------------------
    
    daily_returns <- c(
      
      x$portfolio_value[1] /
        initial_capital - 1,
      
      x$portfolio_value[-1] /
        x$portfolio_value[
          -nrow(x)
        ] - 1
      
    )
    
    daily_returns <- daily_returns[
      is.finite(daily_returns)
    ]
    
    # ----------------------------------------------------------
    # 年化波動度
    # ----------------------------------------------------------
    
    annual_volatility <- if (
      length(daily_returns) >= 2
    ) {
      
      sd(
        daily_returns
      ) *
        sqrt(252)
      
    } else {
      
      NA
      
    }
    
    # ----------------------------------------------------------
    # Sharpe Ratio
    #
    # 無風險利率假設為 0%
    # ----------------------------------------------------------
    
    sharpe_ratio <- if (
      length(daily_returns) >= 2 &&
      is.finite(
        sd(daily_returns)
      ) &&
      sd(daily_returns) > 0
    ) {
      
      mean(
        daily_returns
      ) /
        sd(
          daily_returns
        ) *
        sqrt(252)
      
    } else {
      
      NA
      
    }
    
    # ----------------------------------------------------------
    # 最大回撤
    #
    # 將初始配置資金也視為第一個資產基準點
    # ----------------------------------------------------------
    
    wealth <- c(
      initial_capital,
      x$portfolio_value
    )
    
    running_peak <- cummax(
      wealth
    )
    
    drawdown <- (
      wealth /
        running_peak
    ) - 1
    
    # 移除最前面的初始資金基準點
    drawdown <- drawdown[
      -1
    ]
    
    if (
      length(drawdown) > 0
    ) {
      
      max_drawdown <- min(
        drawdown
      )
      
      max_drawdown_index <- which.min(
        drawdown
      )
      
      max_drawdown_date <- x$date[
        max_drawdown_index
      ]
      
    } else {
      
      max_drawdown <- NA
      
      max_drawdown_date <- NA
      
    }
    
    # ----------------------------------------------------------
    # 建立績效表
    # ----------------------------------------------------------
    
    data.frame(
      
      投資期間 = paste0(
        format(
          start_date,
          "%Y-%m-%d"
        ),
        " 至 ",
        format(
          end_date,
          "%Y-%m-%d"
        )
      ),
      
      有效交易日數 = nrow(
        x
      ),
      
      年化報酬率 = ifelse(
        is.na(annual_return),
        NA,
        paste0(
          sprintf(
            "%+.2f",
            annual_return * 100
          ),
          "%"
        )
      ),
      
      年化波動度 = ifelse(
        is.na(annual_volatility),
        NA,
        paste0(
          round(
            annual_volatility * 100,
            2
          ),
          "%"
        )
      ),
      
      最大回撤 = ifelse(
        is.na(max_drawdown),
        NA,
        paste0(
          round(
            max_drawdown * 100,
            2
          ),
          "%"
        )
      ),
      
      最大回撤日期 = ifelse(
        is.na(max_drawdown_date),
        NA,
        format(
          max_drawdown_date,
          "%Y-%m-%d"
        )
      ),
      
      Sharpe_Ratio = ifelse(
        is.na(sharpe_ratio),
        NA,
        round(
          sharpe_ratio,
          3
        )
      ),
      
      stringsAsFactors = FALSE
      
    )
    
  })
  
  # ============================================================
  # 顯示投資組合績效與風險指標
  # ============================================================
  
  output$portfolio_performance_metrics <- renderTable({
    
    portfolio_performance_metrics()
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE)
  
  # ============================================================
  # 投資組合資產價值時間序列
  # ============================================================
  
  output$portfolio_value_chart <- renderPlot({
    
    x <- portfolio_daily_value()
    
    req(
      nrow(x) > 0
    )
    
    # ----------------------------------------------------------
    # 圖表只顯示使用者指定的時間範圍
    # ----------------------------------------------------------
    
    req(
      input$portfolio_value_date_range
    )
    
    x <- x[
      x$date >= input$portfolio_value_date_range[1] &
        x$date <= input$portfolio_value_date_range[2],
    ]
    
    req(
      nrow(x) > 0
    )
    
    # ----------------------------------------------------------
    # 自動決定 X 軸日期格式
    # ----------------------------------------------------------
    
    date_span <- as.numeric(
      max(x$date) - min(x$date)
    )
    
    if (
      date_span <= 90
    ) {
      
      x_date_labels <- "%m-%d"
      
    } else {
      
      x_date_labels <- "%Y-%m"
      
    }
    
    # ----------------------------------------------------------
    # 投資組合資產價值圖
    # ----------------------------------------------------------
    
    ggplot(
      x,
      aes(
        x = date,
        y = portfolio_value
      )
    ) +
      
      geom_line(
        linewidth = 1,
        alpha = 0.9
      ) +
      
      scale_x_date(
        breaks = scales::breaks_pretty(
          n = 7
        ),
        labels = function(x) {
          format(
            x,
            x_date_labels
          )
        }
      ) +
      
      scale_y_continuous(
        labels = scales::comma
      ) +
      
      labs(
        x = "日期",
        y = "投資組合資產價值（元）",
        title = "投資組合每日資產價值"
      ) +
      
      theme_minimal() +
      
      theme(
        
        plot.title = element_text(
          hjust = 0.5
        ),
        
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
        
      )
    
  })
  
  # ============================================================
  # 投資組合與 0050 累積報酬比較
  # ============================================================
  
  output$portfolio_benchmark_chart <- renderPlot({
    
    # ----------------------------------------------------------
    # 投資組合每日資產價值
    # ----------------------------------------------------------
    
    portfolio_x <- portfolio_daily_value()
    
    req(
      nrow(portfolio_x) > 0
    )
    
    # ----------------------------------------------------------
    # Benchmark 圖只顯示使用者指定的圖表時間範圍
    # ----------------------------------------------------------
    
    req(
      input$portfolio_value_date_range
    )
    
    portfolio_x <- portfolio_x[
      portfolio_x$date >= input$portfolio_value_date_range[1] &
        portfolio_x$date <= input$portfolio_value_date_range[2],
    ]
    
    req(
      nrow(portfolio_x) > 0
    )
    
    # ----------------------------------------------------------
    # 交易資料
    # ----------------------------------------------------------
    
    trades <- portfolio_trades()
    
    req(
      nrow(trades) > 0
    )
    
    # ----------------------------------------------------------
    # 取得目前資料
    # ----------------------------------------------------------
    
    current_data <- data()
    
    # ----------------------------------------------------------
    # 初始配置資金
    # ----------------------------------------------------------
    
    initial_capital <- if (
      input$portfolio_trade_mode ==
      "以投入金額設定"
    ) {
      
      sum(
        trades$配置資金,
        na.rm = TRUE
      )
      
    } else {
      
      sum(
        trades$實際投入資金,
        na.rm = TRUE
      )
      
    }
    
    req(
      is.finite(initial_capital)
    )
    
    req(
      initial_capital > 0
    )
    
    # ----------------------------------------------------------
    # 取得 0050 調整後價格
    # ----------------------------------------------------------
    
    benchmark_data <- current_data[
      current_data$symbol == "0050" &
        current_data$date %in% portfolio_x$date &
        is.finite(current_data$adjusted),
      c(
        "date",
        "adjusted"
      )
    ]
    
    req(
      nrow(benchmark_data) > 0
    )
    
    benchmark_data <- benchmark_data[
      order(
        benchmark_data$date
      ),
    ]
    
    # ----------------------------------------------------------
    # 確保日期不重複
    # ----------------------------------------------------------
    
    benchmark_data <- benchmark_data[
      !duplicated(
        benchmark_data$date
      ),
    ]
    
    # ----------------------------------------------------------
    # 投資組合與 0050 對齊
    # ----------------------------------------------------------
    
    plot_data <- merge(
      portfolio_x,
      benchmark_data,
      by = "date",
      all = FALSE
    )
    
    plot_data <- plot_data[
      order(
        plot_data$date
      ),
    ]
    
    req(
      nrow(plot_data) > 0
    )
    
    # ----------------------------------------------------------
    # 投資組合累積報酬率
    #
    # 以初始配置資金作為分母
    # ----------------------------------------------------------
    
    plot_data$portfolio_return <- (
      plot_data$portfolio_value /
        initial_capital
    ) - 1
    
    # ----------------------------------------------------------
    # 0050 累積報酬率
    #
    # 以圖表開始日的 0050 調整後價格作為基準
    # ----------------------------------------------------------
    
    benchmark_start_price <- plot_data$adjusted[1]
    
    req(
      is.finite(
        benchmark_start_price
      )
    )
    
    plot_data$benchmark_return <- (
      plot_data$adjusted /
        benchmark_start_price
    ) - 1
    
    # ----------------------------------------------------------
    # 自動決定 X 軸日期格式
    # ----------------------------------------------------------
    
    date_span <- as.numeric(
      max(plot_data$date) -
        min(plot_data$date)
    )
    
    if (
      date_span <= 90
    ) {
      
      x_date_labels <- "%m-%d"
      
    } else {
      
      x_date_labels <- "%Y-%m"
      
    }
    
    # ----------------------------------------------------------
    # 繪製累積報酬比較圖
    # ----------------------------------------------------------
    
    ggplot(
      plot_data,
      aes(
        x = date
      )
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      geom_line(
        aes(
          y = portfolio_return,
          color = "投資組合"
        ),
        linewidth = 1.1
      ) +
      
      geom_line(
        aes(
          y = benchmark_return,
          color = "0050"
        ),
        linewidth = 1.1,
        linetype = "dashed"
      ) +
      
      scale_color_brewer(
        palette = "Dark2"
      ) +
      
      scale_y_continuous(
        labels = scales::percent_format(
          accuracy = 1
        )
      ) +
      
      scale_x_date(
        breaks = scales::breaks_pretty(
          n = 7
        ),
        labels = function(x) {
          format(
            x,
            x_date_labels
          )
        }
      ) +
      
      labs(
        x = "日期",
        y = "累積報酬率",
        title = "投資組合與 0050 累積報酬比較",
        color = "比較標的"
      ) +
      
      theme_minimal() +
      
      theme(
        
        plot.title = element_text(
          hjust = 0.5
        ),
        
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        
        legend.position = "bottom"
        
      )
    
  })
  
  
  # ============================================================
  # 投資組合與 0050 績效摘要
  # ============================================================
  
  output$portfolio_benchmark_summary <- renderTable({
    
    # ----------------------------------------------------------
    # 投資組合每日資產價值
    # ----------------------------------------------------------
    
    portfolio_x <- portfolio_daily_value()
    
    req(
      nrow(portfolio_x) > 0
    )
    
    # ----------------------------------------------------------
    # 交易資料
    # ----------------------------------------------------------
    
    trades <- portfolio_trades()
    
    req(
      nrow(trades) > 0
    )
    
    # ----------------------------------------------------------
    # 初始配置資金
    # ----------------------------------------------------------
    
    initial_capital <- if (
      input$portfolio_trade_mode ==
      "以投入金額設定"
    ) {
      
      sum(
        trades$配置資金,
        na.rm = TRUE
      )
      
    } else {
      
      sum(
        trades$實際投入資金,
        na.rm = TRUE
      )
      
    }
    
    req(
      is.finite(initial_capital)
    )
    
    req(
      initial_capital > 0
    )
    
    # ----------------------------------------------------------
    # 投資組合期末價值
    # ----------------------------------------------------------
    
    final_portfolio_value <- tail(
      portfolio_x$portfolio_value,
      1
    )
    
    req(
      is.finite(final_portfolio_value)
    )
    
    # ----------------------------------------------------------
    # 投資組合總報酬率
    # ----------------------------------------------------------
    
    portfolio_return <- (
      final_portfolio_value /
        initial_capital
    ) - 1
    
    # ----------------------------------------------------------
    # 0050 資料
    # ----------------------------------------------------------
    
    current_data <- data()
    
    benchmark_data <- current_data[
      current_data$symbol == "0050" &
        current_data$date >= min(portfolio_x$date) &
        current_data$date <= max(portfolio_x$date) &
        is.finite(current_data$adjusted),
      c(
        "date",
        "adjusted"
      )
    ]
    
    req(
      nrow(benchmark_data) > 0
    )
    
    benchmark_data <- benchmark_data[
      order(
        benchmark_data$date
      ),
    ]
    
    benchmark_data <- benchmark_data[
      !duplicated(
        benchmark_data$date
      ),
    ]
    
    # ----------------------------------------------------------
    # 0050 起始與結束價格
    # ----------------------------------------------------------
    
    benchmark_start_price <- benchmark_data$adjusted[1]
    
    benchmark_end_price <- benchmark_data$adjusted[
      nrow(benchmark_data)
    ]
    
    req(
      is.finite(benchmark_start_price)
    )
    
    req(
      is.finite(benchmark_end_price)
    )
    
    # ----------------------------------------------------------
    # 0050 總報酬率
    # ----------------------------------------------------------
    
    benchmark_return <- (
      benchmark_end_price /
        benchmark_start_price
    ) - 1
    
    # ----------------------------------------------------------
    # 超額報酬
    #
    # 投資組合總報酬率 - 0050總報酬率
    # ----------------------------------------------------------
    
    excess_return <- portfolio_return -
      benchmark_return
    
    # ----------------------------------------------------------
    # 投資期間
    # ----------------------------------------------------------
    
    start_date <- min(
      portfolio_x$date
    )
    
    end_date <- max(
      portfolio_x$date
    )
    
    # ----------------------------------------------------------
    # 建立摘要表
    # ----------------------------------------------------------
    
    data.frame(
      
      投資標的 = c(
        "投資組合",
        "0050"
      ),
      
      投資期間 = paste0(
        format(
          start_date,
          "%Y-%m-%d"
        ),
        " 至 ",
        format(
          end_date,
          "%Y-%m-%d"
        )
      ),
      
      起始值 = c(
        initial_capital,
        benchmark_start_price
      ),
      
      結束值 = c(
        final_portfolio_value,
        benchmark_end_price
      ),
      
      總報酬率 = c(
        paste0(
          sprintf(
            "%+.2f",
            portfolio_return * 100
          ),
          "%"
        ),
        paste0(
          sprintf(
            "%+.2f",
            benchmark_return * 100
          ),
          "%"
        )
      ),
      
      超額報酬 = c(
        paste0(
          sprintf(
            "%+.2f",
            excess_return * 100
          ),
          " 個百分點"
        ),
        "-"
      ),
      
      stringsAsFactors = FALSE
      
    )
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE)
  
  # ----------------------------------------------------------
  # 實際交易日數--------------------------------------------------------------
  # ----------------------------------------------------------
  
  output$trading_days <- renderText({
    
    length(unique(filtered_data()$date))
    
  })
  
  
  # ----------------------------------------------------------
  # 實際資料日期範圍
  # ----------------------------------------------------------
  
  output$actual_date_range <- renderText({
    
    x <- filtered_data()
    
    req(nrow(x) > 0)
    
    paste(
      format(min(x$date), "%Y-%m-%d"),
      "至",
      format(max(x$date), "%Y-%m-%d")
    )
    
  })
  # ----------------------------------------------------------
  # 股票清單
  # ----------------------------------------------------------
  
  output$stock_table <- renderTable({
    
    current_data <- data()
    
    actual_symbols <- unique(current_data$symbol)
    
    stock_list <- data.frame(
      symbol = actual_symbols,
      stringsAsFactors = FALSE
    )
    
    stock_list$name <- stock_info$name[
      match(stock_list$symbol, stock_info$symbol)
    ]
    
    stock_list$industry <- stock_info$industry[
      match(stock_list$symbol, stock_info$symbol)
    ]
    
    stock_list$name[
      stock_list$symbol == "0050"
    ] <- "元大台灣50"
    
    stock_list$industry[
      stock_list$symbol == "0050"
    ] <- "Benchmark"
    
    stock_list$name[
      is.na(stock_list$name)
    ] <- "未設定"
    
    stock_list$industry[
      is.na(stock_list$industry)
    ] <- "其他"
    
    stock_list <- stock_list[
      order(
        stock_list$symbol != "0050",
        stock_list$symbol
      ),
    ]
    
    stock_list
    
  },
      striped = TRUE,
      bordered = TRUE,
      hover = TRUE)

}
  
shinyApp(
  ui = ui,
  server = server
)

