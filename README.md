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

```r
source("main.R")
```

