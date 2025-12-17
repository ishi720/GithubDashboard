# =============================================================================
# GitHub ユーザー 言語別コード量分析スクリプト
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

install_if_missing(c("gh", "ggplot2", "dplyr", "scales", "RColorBrewer"))

# =============================================================================
# ユーザーの全リポジトリを分析
# =============================================================================

analyze_user_repos <- function(username) {
  cat("ユーザーのリポジトリを取得中:", username, "\n")
  
  tryCatch({
    # ユーザーの公開リポジトリを取得
    repos <- gh::gh("GET /users/{username}/repos",
                    username = username,
                    per_page = 100,
                    .limit = Inf)
    
    cat("見つかったリポジトリ数:", length(repos), "\n\n")
    
    # 各リポジトリの言語を集計
    all_languages <- list()
    
    for (repo in repos) {
      # フォークしたリポジトリはスキップ
      if (isTRUE(repo$fork)) {
        next
      }
      
      cat("  -", repo$name, "\n")
      
      langs <- tryCatch({
        gh::gh("GET /repos/{owner}/{repo}/languages",
               owner = username,
               repo = repo$name)
      }, error = function(e) list())
      
      for (lang in names(langs)) {
        if (is.null(all_languages[[lang]])) {
          all_languages[[lang]] <- 0
        }
        all_languages[[lang]] <- all_languages[[lang]] + langs[[lang]]
      }
      
      Sys.sleep(0.1)  # API制限対策
    }
    
    # データフレームに変換
    df <- data.frame(
      language = names(all_languages),
      bytes = unlist(all_languages),
      stringsAsFactors = FALSE
    )
    
    df$percentage <- df$bytes / sum(df$bytes) * 100
    df <- df[order(-df$bytes), ]
    df$language <- factor(df$language, levels = df$language)
    
    return(df)
    
  }, error = function(e) {
    cat("エラー:", e$message, "\n")
    return(NULL)
  })
}

# =============================================================================
# カラーパレット生成関数
# =============================================================================

generate_colors <- function(n) {
  if (n <= 12) {
    return(RColorBrewer::brewer.pal(max(3, n), "Set3"))
  } else {
    # 12色を超える場合はSet3を拡張して生成
    base_colors <- RColorBrewer::brewer.pal(12, "Set3")
    colorRampPalette(base_colors)(n)
  }
}

# =============================================================================
# グラフ作成関数
# =============================================================================

# 横棒グラフ（全言語表示）
create_horizontal_bar <- function(df, title) {
  df$language <- factor(df$language, levels = rev(df$language))
  colors <- generate_colors(nrow(df))
  
  ggplot(df, aes(x = language, y = bytes, fill = language)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(scales::label_bytes()(bytes), " (", 
                                 round(percentage, 1), "%)")),
              hjust = -0.05, size = 3) +
    coord_flip() +
    scale_y_continuous(labels = scales::label_bytes(),
                       expand = expansion(mult = c(0, 0.3))) +
    scale_fill_manual(values = colors) +
    labs(title = title, x = "", y = "コード量") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
}

# 円グラフ
create_pie_chart <- function(df, title) {
  df_top <- head(df, 7)
  
  if (nrow(df) > 7) {
    others <- data.frame(
      language = "Others",
      bytes = sum(df$bytes[8:nrow(df)]),
      percentage = sum(df$percentage[8:nrow(df)])
    )
    df_top <- rbind(df_top, others)
  }
  
  df_top <- df_top %>%
    arrange(desc(bytes)) %>%
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
  df <- analyze_user_repos(user)
  
  if (is.null(df) || nrow(df) == 0) {
    stop("データを取得できませんでした")
  }
  
  # 結果を表示
  cat("\n===== 分析結果 =====\n")
  print(df)
  
  # グラフを作成して保存
  cat("\nグラフを作成中...\n")
  
  # 出力ディレクトリの設定と作成
  output_dir <- OUTPUT_DIR
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  cat("出力先:", output_dir, "\n")
  
  # 横棒グラフ（言語数に応じて高さを調整）
  p1 <- create_horizontal_bar(df, paste0(user, " の言語別コード量"))
  bar_height <- max(6, nrow(df) * 0.5)
  ggsave(file.path(output_dir, "language_bar.png"), p1, width = 10, height = bar_height, dpi = 150)
  
  # 円グラフ
  p2 <- create_pie_chart(df, paste0(user, " の言語構成比"))
  ggsave(file.path(output_dir, "language_pie.png"), p2, width = 8, height = 8, dpi = 150)
  
  cat("\n保存完了:\n")
  cat("  -", file.path(output_dir, "language_bar.png"), "\n")
  cat("  -", file.path(output_dir, "language_pie.png"), "\n")
  
  print(p1)
  
  return(list(data = df, bar = p1, pie = p2))
}

# 実行
result <- main()