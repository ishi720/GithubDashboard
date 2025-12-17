# =============================================================================
# GitHub ユーザー コミット数・言語分析スクリプト
# =============================================================================

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

# 分析対象のGitHubユーザー名を指定
TARGET_USER <- "ishi720"

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
          commit_date <- as.POSIXct(commit$commit$author$date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
          
          commit_data <- data.frame(
            repo = repo$name,
            language = repo_language,
            date = as.Date(commit_date),
            year = format(commit_date, "%Y"),
            month = format(commit_date, "%Y-%m"),
            day_of_week = weekdays(commit_date),
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

# 言語別コミット数 円グラフ
create_language_pie <- function(df, title) {
  df_top <- head(df, 7)
  
  if (nrow(df) > 7) {
    others <- data.frame(
      language = "Others",
      commits = sum(df$commits[8:nrow(df)]),
      percentage = sum(df$percentage[8:nrow(df)])
    )
    df_top <- rbind(df_top, others)
  }
  
  df_top <- df_top %>%
    arrange(desc(commits)) %>%
    mutate(ypos = cumsum(percentage) - 0.5 * percentage)
  
  colors <- generate_colors(nrow(df_top))
  
  ggplot(df_top, aes(x = "", y = percentage, fill = language)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = ypos, label = ifelse(percentage > 3, 
                                           paste0(round(percentage, 1), "%"), "")),
              color = "black", size = 3.5) +
    scale_fill_manual(values = colors) +
    labs(title = title, fill = "言語") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
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
  
  # 結果を表示
  cat("\n===== 言語別コミット数 =====\n")
  print(lang_summary)
  
  cat("\n===== 月別コミット数 =====\n")
  print(month_summary)
  
  cat("\n===== 総コミット数:", nrow(commits_df), "=====\n")
  
  # グラフを作成して保存
  cat("\nグラフを作成中...\n")
  
  # 出力ディレクトリの設定と作成
  output_dir <- file.path(getwd(), "Documents")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  cat("出力先:", output_dir, "\n")
  
  # 言語別コミット数 横棒グラフ
  p1 <- create_language_bar(lang_summary, paste0(user, " の言語別コミット数"))
  bar_height <- max(6, nrow(lang_summary) * 0.5)
  ggsave(file.path(output_dir, "commits_by_language_bar.png"), p1, 
         width = 10, height = bar_height, dpi = 150)
  
  # 言語別コミット数 円グラフ
  p2 <- create_language_pie(lang_summary, paste0(user, " の言語別コミット構成比"))
  ggsave(file.path(output_dir, "commits_by_language_pie.png"), p2, 
         width = 8, height = 8, dpi = 150)
  
  # 月別コミット数 折れ線グラフ
  p3 <- create_monthly_line(month_summary, paste0(user, " の月別コミット数推移"))
  ggsave(file.path(output_dir, "commits_by_month_line.png"), p3, 
         width = 12, height = 6, dpi = 150)
  
  # 月別コミット数 棒グラフ
  p4 <- create_monthly_bar(month_summary, paste0(user, " の月別コミット数"))
  ggsave(file.path(output_dir, "commits_by_month_bar.png"), p4, 
         width = 12, height = 6, dpi = 150)
  
  # 月別・言語別 積み上げ棒グラフ
  p5 <- create_monthly_language_stack(month_lang_summary, 
                                      paste0(user, " の月別・言語別コミット数"))
  ggsave(file.path(output_dir, "commits_by_month_language.png"), p5, 
         width = 12, height = 6, dpi = 150)
  
  cat("\n保存完了:\n")
  cat("  -", file.path(output_dir, "commits_by_language_bar.png"), "\n")
  cat("  -", file.path(output_dir, "commits_by_language_pie.png"), "\n")
  cat("  -", file.path(output_dir, "commits_by_month_line.png"), "\n")
  cat("  -", file.path(output_dir, "commits_by_month_bar.png"), "\n")
  cat("  -", file.path(output_dir, "commits_by_month_language.png"), "\n")
  
  # グラフを表示
  print(p1)
  
  return(list(
    data = commits_df,
    language_summary = lang_summary,
    month_summary = month_summary,
    month_language_summary = month_lang_summary,
    plots = list(
      language_bar = p1,
      language_pie = p2,
      monthly_line = p3,
      monthly_bar = p4,
      monthly_language = p5
    )
  ))
}

# 実行
result <- main()