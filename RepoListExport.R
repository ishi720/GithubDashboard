# =============================================================================
# GitHub ユーザー リポジトリ一覧JSON出力スクリプト
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

install_if_missing(c("gh", "jsonlite"))

# =============================================================================
# ユーザーのリポジトリ情報を取得
# =============================================================================

get_user_repos_detail <- function(username, include_private = FALSE) {
  cat("ユーザーのリポジトリを取得中:", username, "\n")
  cat("プライベートリポジトリ:", if (include_private) "含める" else "除外", "\n")
  
  tryCatch({
    # 認証済みユーザー情報を取得
    auth_user <- tryCatch({
      gh::gh("GET /user")$login
    }, error = function(e) NULL)
    
    # 認証済みユーザーと同じ場合は /user/repos を使用
    if (!is.null(auth_user) && tolower(auth_user) == tolower(username)) {
      if (include_private) {
        cat("認証済みユーザーとして取得（プライベートリポジトリ含む）\n")
        repos <- gh::gh("GET /user/repos",
                        visibility = "all",
                        affiliation = "owner",
                        per_page = 100,
                        .limit = Inf)
      } else {
        cat("認証済みユーザーとして取得（公開リポジトリのみ）\n")
        repos <- gh::gh("GET /user/repos",
                        visibility = "public",
                        affiliation = "owner",
                        per_page = 100,
                        .limit = Inf)
      }
    } else {
      cat("公開リポジトリのみ取得\n")
      repos <- gh::gh("GET /users/{username}/repos",
                      username = username,
                      per_page = 100,
                      .limit = Inf)
    }
    
    cat("見つかったリポジトリ数:", length(repos), "\n\n")
    
    # リポジトリ情報を収集
    repos_list <- list()
    
    for (repo in repos) {
      # フォークしたリポジトリはスキップ
      if (isTRUE(repo$fork)) {
        next
      }
      
      # プライベートリポジトリをスキップ（設定による）
      if (!include_private && isTRUE(repo$private)) {
        next
      }
      
      visibility <- if (isTRUE(repo$private)) "[Private]" else "[Public]"
      cat("  -", repo$name, visibility)
      
      # トピック（タグ）を取得
      topics <- tryCatch({
        result <- gh::gh("GET /repos/{owner}/{repo}/topics",
                         owner = username,
                         repo = repo$name,
                         .send_headers = c("Accept" = "application/vnd.github.mercy-preview+json"))
        unlist(result$names)
      }, error = function(e) character(0))
      
      if (length(topics) > 0) {
        cat(" (", length(topics), "topics)\n")
      } else {
        cat(" (no topics)\n")
      }
      
      # リポジトリ情報を構築
      repo_info <- list(
        name = repo$name,
        url = repo$html_url,
        tags = if (length(topics) > 0) topics else list(),
        description = if (!is.null(repo$description)) repo$description else "",
        created_at = repo$created_at,
        updated_at = repo$updated_at
      )
      
      repos_list[[length(repos_list) + 1]] <- repo_info
      
      Sys.sleep(0.1)  # API制限対策
    }
    
    return(repos_list)
    
  }, error = function(e) {
    cat("エラー:", e$message, "\n")
    return(NULL)
  })
}

# =============================================================================
# JSON出力関数
# =============================================================================

export_repos_to_json <- function(repos_list, output_path) {
  # JSONに変換（pretty print）
  json_output <- jsonlite::toJSON(repos_list, 
                                  pretty = TRUE, 
                                  auto_unbox = TRUE,
                                  force = TRUE)
  
  # ファイルに書き込み
  writeLines(json_output, output_path)
  
  cat("\nJSONファイルを出力しました:", output_path, "\n")
  
  return(invisible(json_output))
}

# =============================================================================
# メイン処理
# =============================================================================

main <- function() {
  user <- TARGET_USER
  include_private <- INCLUDE_PRIVATE_REPOS
  
  if (is.null(user) || user == "") {
    user <- readline(prompt = "GitHubユーザー名を入力: ")
    if (user == "") {
      stop("ユーザー名が指定されていません")
    }
  }
  
  # データ取得
  repos_list <- get_user_repos_detail(user, include_private)
  
  if (is.null(repos_list) || length(repos_list) == 0) {
    stop("リポジトリデータを取得できませんでした")
  }
  
  # 出力ディレクトリの作成
  output_dir <- OUTPUT_DIR
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # JSON出力
  output_path <- file.path(output_dir, "repos_list.json")
  json_output <- export_repos_to_json(repos_list, output_path)
  
  # サマリー表示
  cat("\n===== サマリー =====\n")
  cat("総リポジトリ数:", length(repos_list), "\n")
  cat("出力ファイル:", output_path, "\n")
  
  # 最初の数件をプレビュー表示
  cat("\n===== プレビュー（最初の3件） =====\n")
  preview_count <- min(3, length(repos_list))
  for (i in seq_len(preview_count)) {
    repo <- repos_list[[i]]
    cat("\n[", i, "]", repo$name, "\n")
    cat("    URL:", repo$url, "\n")
    cat("    Tags:", if (length(repo$tags) > 0) paste(repo$tags, collapse = ", ") else "(none)", "\n")
    cat("    Description:", if (nchar(repo$description) > 0) repo$description else "(none)", "\n")
    cat("    Created:", repo$created_at, "\n")
    cat("    Updated:", repo$updated_at, "\n")
  }
  
  return(list(
    repos = repos_list,
    json = json_output,
    output_path = output_path
  ))
}

# 実行
result <- main()