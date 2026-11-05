# =============================================================================
# GitHub ユーザー リポジトリ作成数分析スクリプト
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
# ユーザーのリポジトリ作成情報を取得
# =============================================================================

get_user_repos <- function(username) {
  cat("ユーザーのリポジトリを取得中:", username, "\n")
  
  tryCatch({
    # 認証済みユーザー情報を取得
    auth_user <- tryCatch({
      gh::gh("GET /user")$login
    }, error = function(e) NULL)
    
    # 認証済みユーザーと同じ場合は /user/repos を使用
    if (!is.null(auth_user) && tolower(auth_user) == tolower(username)) {
      cat("認証済みユーザーとして取得（プライベートリポジトリ含む）\n")
      repos <- gh::gh("GET /user/repos",
                      visibility = "all",
                      affiliation = "owner",
                      per_page = 100,
                      .limit = Inf)
    } else {
      cat("公開リポジトリのみ取得\n")
      repos <- gh::gh("GET /users/{username}/repos",
                      username = username,
                      per_page = 100,
                      .limit = Inf)
    }
    
    cat("見つかったリポジトリ数:", length(repos), "\n\n")
    
    # データフレームに変換
    repos_df <- data.frame()
    
    for (repo in repos) {
      # フォークしたリポジトリはスキップ（オプション）
      is_fork <- isTRUE(repo$fork)
      
      created_at <- as.POSIXct(repo$created_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      created_at_jst <- with_tz(created_at, tzone = "Asia/Tokyo")
      
      repo_data <- data.frame(
        name = repo$name,
        language = ifelse(is.null(repo$language), "Unknown", repo$language),
        is_fork = is_fork,
        is_private = isTRUE(repo$private),
        created_at = created_at_jst,
        created_year = format(created_at_jst, "%Y"),
        created_month = format(created_at_jst, "%Y-%m"),
        stringsAsFactors = FALSE
      )
      
      repos_df <- rbind(repos_df, repo_data)
    }
    
    return(repos_df)
    
  }, error = function(e) {
    cat("エラー:", e$message, "\n")
    return(NULL)
  })
}

# =============================================================================
# 集計関数
# =============================================================================

# 月別リポジトリ作成数（フォーク除外）
summarize_by_month <- function(repos_df, include_forks = FALSE) {
  df <- repos_df
  if (!include_forks) {
    df <- df %>% filter(!is_fork)
  }
  
  df %>%
    group_by(created_month) %>%
    summarise(repos = n(), .groups = "drop") %>%
    arrange(created_month) %>%
    mutate(created_month = factor(created_month, levels = created_month))
}

# 年別リポジトリ作成数
summarize_by_year <- function(repos_df, include_forks = FALSE) {
  df <- repos_df
  if (!include_forks) {
    df <- df %>% filter(!is_fork)
  }
  
  df %>%
    group_by(created_year) %>%
    summarise(repos = n(), .groups = "drop") %>%
    arrange(created_year) %>%
    mutate(created_year = factor(created_year, levels = created_year))
}

# 月別・言語別リポジトリ作成数
summarize_by_month_language <- function(repos_df, top_n = 5, include_forks = FALSE) {
  df <- repos_df
  if (!include_forks) {
    df <- df %>% filter(!is_fork)
  }
  
  # 上位N言語を取得
  top_languages <- df %>%
    filter(language != "Unknown") %>%
    count(language, sort = TRUE) %>%
    head(top_n) %>%
    pull(language)
  
  df %>%
    mutate(language = ifelse(language %in% top_languages, language, "Other")) %>%
    group_by(created_month, language) %>%
    summarise(repos = n(), .groups = "drop") %>%
    arrange(created_month)
}

# 累積リポジトリ数
summarize_cumulative <- function(repos_df, include_forks = FALSE) {
  df <- repos_df
  if (!include_forks) {
    df <- df %>% filter(!is_fork)
  }
  
  df %>%
    group_by(created_month) %>%
    summarise(repos = n(), .groups = "drop") %>%
    arrange(created_month) %>%
    mutate(
      cumulative = cumsum(repos),
      created_month = factor(created_month, levels = created_month)
    )
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

# 月別リポジトリ作成数 棒グラフ
create_monthly_bar <- function(df, title) {
  ggplot(df, aes(x = created_month, y = repos)) +
    geom_bar(stat = "identity", fill = "#4A90D9", alpha = 0.8) +
    geom_text(aes(label = repos), vjust = -0.5, size = 3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = title, x = "月", y = "リポジトリ数") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# 月別リポジトリ作成数 折れ線グラフ
create_monthly_line <- function(df, title) {
  ggplot(df, aes(x = created_month, y = repos, group = 1)) +
    geom_line(color = "#4A90D9", size = 1.2) +
    geom_point(color = "#4A90D9", size = 3) +
    geom_text(aes(label = repos), vjust = -1, size = 3) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +
    labs(title = title, x = "月", y = "リポジトリ数") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# 年別リポジトリ作成数 棒グラフ
create_yearly_bar <- function(df, title) {
  colors <- generate_colors(nrow(df))
  
  ggplot(df, aes(x = created_year, y = repos, fill = created_year)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = repos), vjust = -0.5, size = 4) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = title, x = "年", y = "リポジトリ数") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
}

# 月別・言語別 積み上げ棒グラフ
create_monthly_language_stack <- function(df, title) {
  languages <- unique(df$language)
  colors <- generate_colors(length(languages))
  
  ggplot(df, aes(x = created_month, y = repos, fill = language)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colors) +
    labs(title = title, x = "月", y = "リポジトリ数", fill = "言語") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# 累積リポジトリ数 折れ線グラフ
create_cumulative_line <- function(df, title) {
  ggplot(df, aes(x = created_month, y = cumulative, group = 1)) +
    geom_area(fill = "#4A90D9", alpha = 0.3) +
    geom_line(color = "#4A90D9", size = 1.2) +
    geom_point(color = "#4A90D9", size = 2) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(title = title, x = "月", y = "累積リポジトリ数") +
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
  
  if (is.null(user) || user == "") {
    user <- readline(prompt = "GitHubユーザー名を入力: ")
    if (user == "") {
      stop("ユーザー名が指定されていません")
    }
  }
  
  # データ取得
  repos_df <- get_user_repos(user)
  
  if (is.null(repos_df) || nrow(repos_df) == 0) {
    stop("リポジトリデータを取得できませんでした")
  }
  
  # 集計（フォークを除外）
  cat("\n===== 集計中（フォーク除外） =====\n")
  
  month_summary <- summarize_by_month(repos_df)
  year_summary <- summarize_by_year(repos_df)
  month_lang_summary <- summarize_by_month_language(repos_df, top_n = 5)
  cumulative_summary <- summarize_cumulative(repos_df)
  
  # 結果を表示
  cat("\n===== 月別リポジトリ作成数 =====\n")
  print(month_summary)
  
  cat("\n===== 年別リポジトリ作成数 =====\n")
  print(year_summary)
  
  original_repos <- repos_df %>% filter(!is_fork)
  cat("\n===== 総リポジトリ数:", nrow(original_repos), "（フォーク除外） =====\n")
  cat("===== フォーク数:", sum(repos_df$is_fork), " =====\n")
  
  # グラフを作成して保存
  cat("\nグラフを作成中...\n")
  
  output_dir <- OUTPUT_DIR
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  cat("出力先:", output_dir, "\n")
  
  # 月別リポジトリ作成数 棒グラフ
  p1 <- create_monthly_bar(month_summary, paste0(user, " の月別リポジトリ作成数"))
  ggsave(file.path(output_dir, "repos_by_month_bar.png"), p1,
         width = 12, height = 6, dpi = 150)
  
  # 月別リポジトリ作成数 折れ線グラフ
  p2 <- create_monthly_line(month_summary, paste0(user, " の月別リポジトリ作成数推移"))
  ggsave(file.path(output_dir, "repos_by_month_line.png"), p2,
         width = 12, height = 6, dpi = 150)
  
  # 年別リポジトリ作成数 棒グラフ
  p3 <- create_yearly_bar(year_summary, paste0(user, " の年別リポジトリ作成数"))
  ggsave(file.path(output_dir, "repos_by_year.png"), p3,
         width = 10, height = 6, dpi = 150)
  
  # 月別・言語別 積み上げ棒グラフ
  p4 <- create_monthly_language_stack(month_lang_summary,
                                      paste0(user, " の月別・言語別リポジトリ作成数"))
  ggsave(file.path(output_dir, "repos_by_month_language.png"), p4,
         width = 12, height = 6, dpi = 150)
  
  # 累積リポジトリ数
  p5 <- create_cumulative_line(cumulative_summary,
                               paste0(user, " の累積リポジトリ数"))
  ggsave(file.path(output_dir, "repos_cumulative.png"), p5,
         width = 12, height = 6, dpi = 150)
  
  cat("\n保存完了:\n")
  cat("  -", file.path(output_dir, "repos_by_month_bar.png"), "\n")
  cat("  -", file.path(output_dir, "repos_by_month_line.png"), "\n")
  cat("  -", file.path(output_dir, "repos_by_year.png"), "\n")
  cat("  -", file.path(output_dir, "repos_by_month_language.png"), "\n")
  cat("  -", file.path(output_dir, "repos_cumulative.png"), "\n")
  
  print(p3)
  
  return(list(
    data = repos_df,
    month_summary = month_summary,
    year_summary = year_summary,
    month_language_summary = month_lang_summary,
    cumulative_summary = cumulative_summary,
    plots = list(
      monthly_bar = p1,
      monthly_line = p2,
      yearly_bar = p3,
      monthly_language = p4,
      cumulative = p5
    )
  ))
}

# 実行
result <- main()

