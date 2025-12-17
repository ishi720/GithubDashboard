# =============================================================================
# GitHub ユーザー Issues分析スクリプト
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
# ユーザーが作成したIssuesを取得
# =============================================================================

get_user_issues <- function(username, years = 2) {
  cat("ユーザーが作成したIssuesを取得中:", username, "\n")
  
  since_date <- Sys.Date() - (years * 365)
  
  tryCatch({
    # Search APIでユーザーが作成したIssuesを取得
    # type:issue で Issue のみ（PRを除外）
    # is:public でプライベートリポジトリを除外
    query <- paste0("author:", username, " type:issue is:public created:>=", since_date)
    
    issues <- gh::gh("GET /search/issues",
                     q = query,
                     per_page = 100,
                     .limit = Inf)
    
    cat("見つかったIssues数:", issues$total_count, "\n")
    
    if (issues$total_count == 0) {
      return(NULL)
    }
    
    # データフレームに変換
    issues_df <- data.frame()
    
    for (issue in issues$items) {
      created_at <- as.POSIXct(issue$created_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      closed_at <- if (!is.null(issue$closed_at)) {
        as.POSIXct(issue$closed_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      } else {
        NA
      }
      
      # リポジトリ名を抽出（URLから）
      repo_url <- issue$repository_url
      repo_name <- sub(".*/repos/", "", repo_url)
      
      issue_data <- data.frame(
        number = issue$number,
        title = issue$title,
        repo = repo_name,
        state = issue$state,
        created_at = created_at,
        closed_at = closed_at,
        created_month = format(created_at, "%Y-%m"),
        closed_month = ifelse(is.na(closed_at), NA, format(closed_at, "%Y-%m")),
        stringsAsFactors = FALSE
      )
      
      issues_df <- rbind(issues_df, issue_data)
    }
    
    return(issues_df)
    
  }, error = function(e) {
    cat("エラー:", e$message, "\n")
    return(NULL)
  })
}

# =============================================================================
# ユーザーが解決（クローズ）したIssuesを取得
# =============================================================================

get_user_closed_issues <- function(username, years = 2) {
  cat("\nユーザーが解決したIssuesを取得中:", username, "\n")
  
  since_date <- Sys.Date() - (years * 365)
  
  tryCatch({
    # ユーザーがアサインされてクローズされたIssues
    # is:public でプライベートリポジトリを除外
    query <- paste0("assignee:", username, " type:issue is:public is:closed closed:>=", since_date)
    
    issues <- gh::gh("GET /search/issues",
                     q = query,
                     per_page = 100,
                     .limit = Inf)
    
    cat("見つかった解決Issues数:", issues$total_count, "\n")
    
    if (issues$total_count == 0) {
      return(NULL)
    }
    
    # データフレームに変換
    issues_df <- data.frame()
    
    for (issue in issues$items) {
      created_at <- as.POSIXct(issue$created_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      closed_at <- as.POSIXct(issue$closed_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      
      repo_url <- issue$repository_url
      repo_name <- sub(".*/repos/", "", repo_url)
      
      issue_data <- data.frame(
        number = issue$number,
        title = issue$title,
        repo = repo_name,
        state = issue$state,
        created_at = created_at,
        closed_at = closed_at,
        closed_month = format(closed_at, "%Y-%m"),
        stringsAsFactors = FALSE
      )
      
      issues_df <- rbind(issues_df, issue_data)
    }
    
    return(issues_df)
    
  }, error = function(e) {
    cat("エラー:", e$message, "\n")
    return(NULL)
  })
}

# =============================================================================
# 集計関数
# =============================================================================

# 月別の作成・クローズ数を集計
summarize_issues_by_month <- function(created_df, closed_df) {
  # 作成数
  created_summary <- created_df %>%
    group_by(month = created_month) %>%
    summarise(created = n(), .groups = "drop")
  
  # クローズ数（自分が作成したIssuesのうち）
  closed_own <- created_df %>%
    filter(!is.na(closed_month)) %>%
    group_by(month = closed_month) %>%
    summarise(closed_own = n(), .groups = "drop")
  
  # 解決数（アサインされたもの）
  if (!is.null(closed_df) && nrow(closed_df) > 0) {
    resolved_summary <- closed_df %>%
      group_by(month = closed_month) %>%
      summarise(resolved = n(), .groups = "drop")
  } else {
    resolved_summary <- data.frame(month = character(), resolved = integer())
  }
  
  # 全ての月を取得
  all_months <- sort(unique(c(created_summary$month, 
                              closed_own$month,
                              resolved_summary$month)))
  
  # マージ
  result <- data.frame(month = all_months) %>%
    left_join(created_summary, by = "month") %>%
    left_join(closed_own, by = "month") %>%
    left_join(resolved_summary, by = "month") %>%
    replace(is.na(.), 0) %>%
    mutate(month = factor(month, levels = month))
  
  return(result)
}

# =============================================================================
# グラフ作成関数
# =============================================================================

# サマリー棒グラフ
create_summary_bar <- function(created_count, closed_count, resolved_count, title) {
  df <- data.frame(
    category = factor(c("作成したIssues", "解決済み(自分の)", "解決(アサイン)"),
                      levels = c("作成したIssues", "解決済み(自分の)", "解決(アサイン)")),
    count = c(created_count, closed_count, resolved_count)
  )
  
  colors <- c("#4A90D9", "#7ED321", "#F5A623")
  
  ggplot(df, aes(x = category, y = count, fill = category)) +
    geom_bar(stat = "identity", width = 0.6) +
    geom_text(aes(label = count), vjust = -0.5, size = 5) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = title, x = "", y = "件数") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(size = 11)
    )
}

# 月別推移グラフ
create_monthly_issues_chart <- function(df, title) {
  df_long <- df %>%
    pivot_longer(cols = c(created, closed_own, resolved),
                 names_to = "type",
                 values_to = "count") %>%
    mutate(type = factor(type, 
                         levels = c("created", "closed_own", "resolved"),
                         labels = c("作成", "解決(自分の)", "解決(アサイン)")))
  
  colors <- c("作成" = "#4A90D9", "解決(自分の)" = "#7ED321", "解決(アサイン)" = "#F5A623")
  
  ggplot(df_long, aes(x = month, y = count, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = colors) +
    labs(title = title, x = "月", y = "件数", fill = "") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}

# 月別推移 折れ線グラフ
create_monthly_issues_line <- function(df, title) {
  df_long <- df %>%
    pivot_longer(cols = c(created, closed_own),
                 names_to = "type",
                 values_to = "count") %>%
    mutate(type = factor(type, 
                         levels = c("created", "closed_own"),
                         labels = c("作成", "解決")))
  
  colors <- c("作成" = "#4A90D9", "解決" = "#7ED321")
  
  ggplot(df_long, aes(x = month, y = count, color = type, group = type)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = colors) +
    labs(title = title, x = "月", y = "件数", color = "") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}

# リポジトリ別Issues数
create_repo_issues_bar <- function(df, title, top_n = 10) {
  repo_summary <- df %>%
    group_by(repo) %>%
    summarise(
      total = n(),
      open = sum(state == "open"),
      closed = sum(state == "closed"),
      .groups = "drop"
    ) %>%
    arrange(desc(total)) %>%
    head(top_n)
  
  repo_summary$repo <- factor(repo_summary$repo, levels = rev(repo_summary$repo))
  
  df_long <- repo_summary %>%
    pivot_longer(cols = c(open, closed),
                 names_to = "state",
                 values_to = "count") %>%
    mutate(state = factor(state, levels = c("open", "closed"),
                          labels = c("Open", "Closed")))
  
  colors <- c("Open" = "#F5A623", "Closed" = "#7ED321")
  
  ggplot(df_long, aes(x = repo, y = count, fill = state)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = colors) +
    labs(title = title, x = "", y = "件数", fill = "状態") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
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
  created_issues <- get_user_issues(user, YEARS_TO_ANALYZE)
  closed_issues <- get_user_closed_issues(user, YEARS_TO_ANALYZE)
  
  if (is.null(created_issues) || nrow(created_issues) == 0) {
    cat("作成したIssuesが見つかりませんでした\n")
    created_issues <- data.frame(
      created_month = character(),
      closed_month = character(),
      state = character(),
      repo = character()
    )
  }
  
  # 集計
  created_count <- nrow(created_issues)
  closed_own_count <- sum(!is.na(created_issues$closed_at))
  resolved_count <- if (!is.null(closed_issues)) nrow(closed_issues) else 0
  
  # 結果表示
  cat("\n===== Issues サマリー =====\n")
  cat("作成したIssues:", created_count, "件\n")
  cat("  - Open:", sum(created_issues$state == "open"), "件\n")
  cat("  - Closed:", closed_own_count, "件\n")
  cat("解決したIssues(アサイン):", resolved_count, "件\n")
  
  # 月別集計
  if (nrow(created_issues) > 0) {
    monthly_summary <- summarize_issues_by_month(created_issues, closed_issues)
    cat("\n===== 月別 Issues =====\n")
    print(monthly_summary)
  }
  
  # グラフ作成
  cat("\nグラフを作成中...\n")
  
  output_dir <- OUTPUT_DIR
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  cat("出力先:", output_dir, "\n")
  
  # サマリー棒グラフ
  p1 <- create_summary_bar(created_count, closed_own_count, resolved_count,
                           paste0(user, " の Issues サマリー"))
  ggsave(file.path(output_dir, "issues_summary.png"), p1,
         width = 8, height = 6, dpi = 150)
  
  if (nrow(created_issues) > 0) {
    # 月別推移 棒グラフ
    p2 <- create_monthly_issues_chart(monthly_summary,
                                      paste0(user, " の月別 Issues"))
    ggsave(file.path(output_dir, "issues_by_month_bar.png"), p2,
           width = 12, height = 6, dpi = 150)
    
    # 月別推移 折れ線グラフ
    p3 <- create_monthly_issues_line(monthly_summary,
                                     paste0(user, " の月別 Issues 推移"))
    ggsave(file.path(output_dir, "issues_by_month_line.png"), p3,
           width = 12, height = 6, dpi = 150)
    
    # リポジトリ別
    p4 <- create_repo_issues_bar(created_issues,
                                 paste0(user, " のリポジトリ別 Issues (Top 10)"))
    ggsave(file.path(output_dir, "issues_by_repo.png"), p4,
           width = 10, height = 6, dpi = 150)
  }
  
  cat("\n保存完了:\n")
  cat("  -", file.path(output_dir, "issues_summary.png"), "\n")
  cat("  -", file.path(output_dir, "issues_by_month_bar.png"), "\n")
  cat("  -", file.path(output_dir, "issues_by_month_line.png"), "\n")
  cat("  -", file.path(output_dir, "issues_by_repo.png"), "\n")
  
  print(p1)
  
  return(list(
    created_issues = created_issues,
    closed_issues = closed_issues,
    summary = data.frame(
      created = created_count,
      closed_own = closed_own_count,
      resolved = resolved_count
    )
  ))
}

# 実行
result <- main()