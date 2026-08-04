# GithubDashboard

GitHubユーザーのリポジトリを分析し、可視化するRスクリプトです。

# バッジ

[![Codacy Badge](https://app.codacy.com/project/badge/Grade/99f416fe7ac74d64a8bebfa7861c8da6)](https://app.codacy.com/gh/ishi720/GithubDashboard/dashboard?utm_source=gh&utm_medium=referral&utm_content=&utm_campaign=Badge_grade)

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

<img width="1500" alt="language_bar" src="https://raw.githubusercontent.com/ishi720/GithubDashboard/refs/heads/main/Documents/language_bar.png" />

## 月・曜日ごとのコミット数

```r
source("CommitAnalysis.R")
```

<img width="1800" alt="commits_heatmap" src="https://raw.githubusercontent.com/ishi720/GithubDashboard/refs/heads/main/Documents/commits_by_month_language.png" />

<img width="2100" alt="commits_heatmap" src="https://raw.githubusercontent.com/ishi720/GithubDashboard/refs/heads/main/Documents/commits_heatmap.png" />

## リポジトリの作成数

```r
source("Repoanalysis.R")
```

<img width="1800" alt="repos_by_year" src="https://raw.githubusercontent.com/ishi720/GithubDashboard/refs/heads/main/Documents/repos_by_year.png" />

## リポジトリ一覧


```r
source("RepoListExport.R")
```

`repos_list.json` を出力

## リポジトリのタグクラウド

```r
source("TagAnalysis.R")
```

<img width="1800" alt="topics_wordcloud" src="https://raw.githubusercontent.com/ishi720/GithubDashboard/refs/heads/main/Documents/topics_wordcloud.png" />



## Issueの作成と解決

```r
source("IssuesAnalysis.R")
```

<img width="1800" alt="issues_by_month_line" src="https://raw.githubusercontent.com/ishi720/GithubDashboard/refs/heads/main/Documents/issues_by_month_line.png" />

