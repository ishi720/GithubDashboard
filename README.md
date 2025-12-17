# R-GithubAnalysis

GitHubユーザーの公開リポジトリを分析し、使用言語別のコード量を可視化するRスクリプトです。

# Windows環境セットアップ

- R: 4.5.2
- RStudio

# リポジトリセットアップ

1. リポジトリのクローン

```bash
git clone git@github.com:ishi720/R-GithubAnalysis.git
cd R-GithubAnalysis
```

2. 依存パッケージのインストール

```r
renv::restore()
```

# 実行

## 使用しているプログラミング言語（バイト数）

```r
source("main.R")
```

<img width="1500" height="1575" alt="language_bar" src="https://github.com/user-attachments/assets/3eb91955-0367-4820-8799-689c72bdf33b" />

## 月ごとのコミット数

```r
source("CommitAnalysis.R")
```

<img width="1800" height="900" alt="commits_by_month_language" src="https://github.com/user-attachments/assets/7342eb3b-31fa-41d3-a2d1-f52b77983f04" />
