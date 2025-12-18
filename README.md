# GithubDashboard

GitHubユーザーのリポジトリを分析し、可視化するRスクリプトです。

# Windows環境セットアップ

- R: 4.5.2
- RStudio

# リポジトリセットアップ

1. リポジトリのクローン

```bash
git clone git@github.com:ishi720/GithubDashboard.git
cd GithubDashboard
```

2. 依存パッケージのインストール

```r
renv::restore()
```

# 実行

## 使用しているプログラミング言語（バイト数）

```r
source("LanguageAnalysis.R")
```

<img width="1500" height="1575" alt="language_bar" src="https://raw.githubusercontent.com/ishi720/GithubDashboard/refs/heads/main/Documents/language_bar.png" />

## 月・曜日ごとのコミット数

```r
source("CommitAnalysis.R")
```

<img width="1800" height="900" alt="commits_by_month_language" src="https://raw.githubusercontent.com/ishi720/GithubDashboard/refs/heads/main/Documents/commits_by_month_language.png" />

<img width="2100" height="900" alt="commits_heatmap" src="https://raw.githubusercontent.com/ishi720/GithubDashboard/refs/heads/main/Documents/commits_heatmap.png" />

## リポジトリの作成数

```r
source("Repoanalysis.R")
```

<img width="1800" height="900" alt="commits_by_month_language" src="https://raw.githubusercontent.com/ishi720/GithubDashboard/refs/heads/main/Documents/repos_by_year.png" />


## Issueの作成と解決

```r
source("IssuesAnalysis.R")
```

<img width="1800" height="900" alt="issues_by_month_line" src="https://raw.githubusercontent.com/ishi720/GithubDashboard/refs/heads/main/Documents/issues_by_month_line.png" />

