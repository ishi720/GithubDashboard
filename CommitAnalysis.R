# =============================================================================
# GitHub ユーザー コミット数・言語分析スクリプト
# =============================================================================

# 共通設定ファイルの読み込み
source("config.R")

# 必要なパッケージのインストール（初回のみ）
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cran.r-project.org")
      library(pkg, character.only = TRUE)
    }
  }
}

install_if_missing(c("gh", "ggplot2", "dplyr", "scales", "RColorBrewer", "lubridate", "tidyr"))

# =============================================================================
# 設定
# =============================================================================

# 分析期間（過去何年分を取得するか）
YEARS_TO_ANALYZE <- 2

# =============================================================================
# ユーザーの全リポジトリからコミット情報を取得
# =============================================================================

get_user_commits <- function(username, years = 2) {
  cat("ユーザーのリポジトリを取得中:", username, "\n")
  
  # 取得開始日
  since_date <- Sys.Date() - (years * 365)
  
  tryCatch({
    # ユーザーの公開リポジトリを取得
    repos <- gh::gh("GET /users/{username}/repos",
                    username = username,
                    per_page = 100,
                    .limit = Inf)
    
    cat("見つかったリポジトリ数:", length(repos), "\n\n")
    
    all_commits <- data.frame()
    
    for (repo in repos) {
      # フォークしたリポジトリはスキップ
      if (isTRUE(repo$fork)) {
        next
      }
      
      cat("  -", repo$name)
      
      # リポジトリの主要言語を取得
      repo_language <- ifelse(is.null(repo$language), "Unknown", repo$language)
      
      # コミット履歴を取得
      commits <- tryCatch({
        gh::gh("GET /repos/{owner}/{repo}/commits",
               owner = username,
               repo = repo$name,
               author = username,
               since = format(since_date, "%Y-%m-%dT00:00:00Z"),
               per_page = 100,
               .limit = Inf)
      }, error = function(e) list())
      
      if (length(commits) > 0) {
        cat(" (", length(commits), "commits)\n")
        
        for (commit in commits) {
          # UTCで取得してから日本時間に変換
          commit_date_utc <- as.POSIXct(commit$commit$author$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
          commit_date <- with_tz(commit_date_utc, tzone = "Asia/Tokyo")
          
          # 曜日を数字で取得（0=日曜, 1=月曜, ..., 6=土曜）し、英語名に変換
          day_num <- as.integer(format(commit_date, "%w"))
          day_names <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
          day_of_week <- day_names[day_num + 1]
          
          commit_data <- data.frame(
            repo = repo$name,
            language = repo_language,
            date = as.Date(commit_date),
            year = format(commit_date, "%Y"),
            month = format(commit_date, "%Y-%m"),
            day_of_week = day_of_week,
            hour = as.integer(format(commit_date, "%H")),
            stringsAsFactors = FALSE
          )
          
          all_commits <- rbind(all_commits, commit_data)
        }
      } else {
        cat(" (0 commits)\n")
      }
      
      Sys.sleep(0.1)  # API制限対策
    }
    
    return(all_commits)
    
  }, error = function(e) {
    cat("エラー:", e$message, "\n")
    return(NULL)
  })
}

# =============================================================================
# データ集計関数
# =============================================================================

# 言語別コミット数
summarize_by_language <- function(commits_df) {
  commits_df %>%
    group_by(language) %>%
    summarise(commits = n(), .groups = "drop") %>%
    arrange(desc(commits)) %>%
    mutate(
      percentage = commits / sum(commits) * 100,
      language = factor(language, levels = language)
    )
}

# 月別コミット数
summarize_by_month <- function(commits_df) {
  commits_df %>%
    group_by(month) %>%
    summarise(commits = n(), .groups = "drop") %>%
    arrange(month) %>%
    mutate(month = factor(month, levels = month))
}

# 月別・言語別コミット数
summarize_by_month_language <- function(commits_df, top_n = 5) {
  # 上位N言語を取得
  top_languages <- commits_df %>%
    count(language, sort = TRUE) %>%
    head(top_n) %>%
    pull(language)
  
  commits_df %>%
    mutate(language = ifelse(language %in% top_languages, language, "Other")) %>%
    group_by(month, language) %>%
    summarise(commits = n(), .groups = "drop") %>%
    arrange(month)
}

# 曜日別コミット数
summarize_by_day_of_week <- function(commits_df) {
  # 曜日の順序を定義
  day_order <- c("月曜日", "火曜日", "水曜日", "木曜日", "金曜日", "土曜日", "日曜日")
  day_order_en <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  
  commits_df %>%
    group_by(day_of_week) %>%
    summarise(commits = n(), .groups = "drop") %>%
    mutate(
      day_of_week = factor(day_of_week, levels = day_order_en),
      percentage = commits / sum(commits) * 100
    ) %>%
    arrange(day_of_week)
}

# 時間帯別コミット数
summarize_by_hour <- function(commits_df) {
  commits_df %>%
    group_by(hour) %>%
    summarise(commits = n(), .groups = "drop") %>%
    complete(hour = 0:23, fill = list(commits = 0)) %>%
    mutate(percentage = commits / sum(commits) * 100)
}

# 曜日×時間帯のヒートマップ用データ
summarize_by_day_hour <- function(commits_df) {
  day_order_en <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  
  # 先に曜日をファクター化
  commits_df <- commits_df %>%
    mutate(day_of_week = factor(day_of_week, levels = day_order_en))
  
  # 集計
  result <- commits_df %>%
    group_by(day_of_week, hour) %>%
    summarise(commits = n(), .groups = "drop")
  
  # 全ての曜日×時間の組み合わせを作成
  all_combinations <- expand.grid(
    day_of_week = factor(day_order_en, levels = day_order_en),
    hour = 0:23
  )
  
  # マージして欠損値を0で埋める
  result <- all_combinations %>%
    left_join(result, by = c("day_of_week", "hour")) %>%
    mutate(commits = ifelse(is.na(commits), 0, commits))
  
  return(result)
}

# =============================================================================
# カラーパレット生成関数
# =============================================================================

generate_colors <- function(n) {
  if (n <= 12) {
    return(RColorBrewer::brewer.pal(max(3, n), "Set3"))
  } else {
    base_colors <- RColorBrewer::brewer.pal(12, "Set3")
    colorRampPalette(base_colors)(n)
  }
}

# =============================================================================
# グラフ作成関数
# =============================================================================

# 言語別コミット数 横棒グラフ
create_language_bar <- function(df, title) {
  df$language <- factor(df$language, levels = rev(df$language))
  colors <- generate_colors(nrow(df))
  
  ggplot(df, aes(x = language, y = commits, fill = language)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(commits, " (", round(percentage, 1), "%)")),
              hjust = -0.05, size = 3) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
    scale_fill_manual(values = colors) +
    labs(title = title, x = "", y = "コミット数") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
}

# 月別コミット数 折れ線グラフ
create_monthly_line <- function(df, title) {
  ggplot(df, aes(x = month, y = commits, group = 1)) +
    geom_line(color = "#4A90D9", size = 1.2) +
    geom_point(color = "#4A90D9", size = 3) +
    geom_text(aes(label = commits), vjust = -1, size = 3) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +
    labs(title = title, x = "月", y = "コミット数") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# 月別コミット数 棒グラフ
create_monthly_bar <- function(df, title) {
  ggplot(df, aes(x = month, y = commits)) +
    geom_bar(stat = "identity", fill = "#4A90D9", alpha = 0.8) +
    geom_text(aes(label = commits), vjust = -0.5, size = 3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = title, x = "月", y = "コミット数") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# 月別・言語別 積み上げ棒グラフ
create_monthly_language_stack <- function(df, title) {
  languages <- unique(df$language)
  colors <- generate_colors(length(languages))
  
  ggplot(df, aes(x = month, y = commits, fill = language)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colors) +
    labs(title = title, x = "月", y = "コミット数", fill = "言語") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# 曜日別コミット数 棒グラフ
create_day_of_week_bar <- function(df, title) {
  # 曜日ラベルを日本語に
  day_labels <- c("Monday" = "月", "Tuesday" = "火", "Wednesday" = "水", 
                  "Thursday" = "木", "Friday" = "金", "Saturday" = "土", "Sunday" = "日")
  
  ggplot(df, aes(x = day_of_week, y = commits, fill = day_of_week)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = commits), vjust = -0.5, size = 4) +
    scale_x_discrete(labels = day_labels) +
    scale_fill_brewer(palette = "Set2") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = title, x = "曜日", y = "コミット数") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
}

# 時間帯別コミット数 棒グラフ
create_hourly_bar <- function(df, title) {
  ggplot(df, aes(x = factor(hour), y = commits)) +
    geom_bar(stat = "identity", fill = "#4A90D9", alpha = 0.8) +
    geom_text(aes(label = ifelse(commits > 0, commits, "")), vjust = -0.5, size = 2.5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = title, x = "時間帯", y = "コミット数") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
}

# 曜日×時間帯 ヒートマップ
create_day_hour_heatmap <- function(df, title) {
  day_labels <- c("Monday" = "月", "Tuesday" = "火", "Wednesday" = "水", 
                  "Thursday" = "木", "Friday" = "金", "Saturday" = "土", "Sunday" = "日")
  
  ggplot(df, aes(x = factor(hour), y = day_of_week, fill = commits)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = ifelse(commits > 0, commits, "")), 
              color = "black", size = 2.5) +
    scale_y_discrete(labels = day_labels, limits = rev(levels(df$day_of_week))) +
    scale_fill_gradient(low = "#f7fbff", high = "#08519c",
                        name = "コミット数") +
    labs(title = title, x = "時間帯", y = "曜日") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      panel.grid = element_blank()
    )
}

# =============================================================================
# メイン処理
# =============================================================================

main <- function() {
  user <- TARGET_USER
  
  # 空の場合はインタラクティブに入力
  if (is.null(user) || user == "") {
    user <- readline(prompt = "GitHubユーザー名を入力: ")
    if (user == "") {
      stop("ユーザー名が指定されていません")
    }
  }
  
  # データ取得
  commits_df <- get_user_commits(user, YEARS_TO_ANALYZE)
  
  if (is.null(commits_df) || nrow(commits_df) == 0) {
    stop("コミットデータを取得できませんでした")
  }
  
  # 集計
  cat("\n===== 集計中 =====\n")
  
  lang_summary <- summarize_by_language(commits_df)
  month_summary <- summarize_by_month(commits_df)
  month_lang_summary <- summarize_by_month_language(commits_df, top_n = 5)
  day_summary <- summarize_by_day_of_week(commits_df)
  hour_summary <- summarize_by_hour(commits_df)
  day_hour_summary <- summarize_by_day_hour(commits_df)
  
  # 結果を表示
  cat("\n===== 言語別コミット数 =====\n")
  print(lang_summary)
  
  cat("\n===== 月別コミット数 =====\n")
  print(month_summary)
  
  cat("\n===== 曜日別コミット数 =====\n")
  print(day_summary)
  
  cat("\n===== 時間帯別コミット数 =====\n")
  print(hour_summary)
  
  cat("\n===== 総コミット数:", nrow(commits_df), "=====\n")
  
  # グラフを作成して保存
  cat("\nグラフを作成中...\n")
  
  # 出力ディレクトリの作成
  output_dir <- OUTPUT_DIR
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  cat("出力先:", output_dir, "\n")
  
  # 言語別コミット数 横棒グラフ
  p1 <- create_language_bar(lang_summary, paste0(user, " の言語別コミット数"))
  bar_height <- max(6, nrow(lang_summary) * 0.5)
  ggsave(file.path(output_dir, "commits_by_language_bar.png"), p1, 
         width = 10, height = bar_height, dpi = 150)
  
  # 月別コミット数 折れ線グラフ
  p2 <- create_monthly_line(month_summary, paste0(user, " の月別コミット数推移"))
  ggsave(file.path(output_dir, "commits_by_month_line.png"), p2, 
         width = 12, height = 6, dpi = 150)
  
  # 月別コミット数 棒グラフ
  p3 <- create_monthly_bar(month_summary, paste0(user, " の月別コミット数"))
  ggsave(file.path(output_dir, "commits_by_month_bar.png"), p3, 
         width = 12, height = 6, dpi = 150)
  
  # 月別・言語別 積み上げ棒グラフ
  p4 <- create_monthly_language_stack(month_lang_summary, 
                                      paste0(user, " の月別・言語別コミット数"))
  ggsave(file.path(output_dir, "commits_by_month_language.png"), p4, 
         width = 12, height = 6, dpi = 150)
  
  # 曜日別コミット数
  p5 <- create_day_of_week_bar(day_summary, paste0(user, " の曜日別コミット数"))
  ggsave(file.path(output_dir, "commits_by_day_of_week.png"), p5,
         width = 8, height = 6, dpi = 150)
  
  # 時間帯別コミット数
  p6 <- create_hourly_bar(hour_summary, paste0(user, " の時間帯別コミット数"))
  ggsave(file.path(output_dir, "commits_by_hour.png"), p6,
         width = 12, height = 6, dpi = 150)
  
  # 曜日×時間帯 ヒートマップ
  p7 <- create_day_hour_heatmap(day_hour_summary, paste0(user, " のコミット時間帯ヒートマップ"))
  ggsave(file.path(output_dir, "commits_heatmap.png"), p7,
         width = 14, height = 6, dpi = 150)
  
  cat("\n保存完了:\n")
  cat("  -", file.path(output_dir, "commits_by_language_bar.png"), "\n")
  cat("  -", file.path(output_dir, "commits_by_month_line.png"), "\n")
  cat("  -", file.path(output_dir, "commits_by_month_bar.png"), "\n")
  cat("  -", file.path(output_dir, "commits_by_month_language.png"), "\n")
  cat("  -", file.path(output_dir, "commits_by_day_of_week.png"), "\n")
  cat("  -", file.path(output_dir, "commits_by_hour.png"), "\n")
  cat("  -", file.path(output_dir, "commits_heatmap.png"), "\n")
  
  # グラフを表示
  print(p1)
  
  return(list(
    data = commits_df,
    language_summary = lang_summary,
    month_summary = month_summary,
    month_language_summary = month_lang_summary,
    day_summary = day_summary,
    hour_summary = hour_summary,
    day_hour_summary = day_hour_summary,
    plots = list(
      language_bar = p1,
      monthly_line = p2,
      monthly_bar = p3,
      monthly_language = p4,
      day_of_week = p5,
      hourly = p6,
      heatmap = p7
    )
  ))
}

# 実行
result <- main()