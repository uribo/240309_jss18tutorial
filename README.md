Rユーザのための機械学習チュートリアル
================

2024年3月9日に開催された[日本統計学会第18回春季集会](https://jss2024spring.ywstat.jp/)（成城大学）における企画セッション AM-B「Rユーザのための機械学習チュートリアル」で用いる資料を公開しています。

## 構成

### 第一部: Rの機械学習フレームワークの紹介〜tidymodelsを中心に〜

[slide](https://speakerdeck.com/s_uryu/machine-learning-with-r2024), [code](https://github.com/uribo/240309_jss18tutorial/tree/main/part1)

### 第二部: 地理空間データの機械学習への適用

[slide](https://speakerdeck.com/s_uryu/machine-learning-for-spatial-data), [code](https://github.com/uribo/240309_jss18tutorial/tree/main/part2)

## データセット

【国土数値情報】都道府県地価調査データ（2020年）

> [!NOTE]
>このデータは「国土交通省 国土数値情報（都道府県地価調査データ（2020年））」（国土交通省）（https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-L02-2020.html）（2024年3月6日取得）をもとに
瓜生真也が加工して作成したものです。

## プログラム実行環境

- R 4.3.2
- renv 1.0.5
- targets 1.5.1
- macOS Sonoma 14.3.1

### renvを使ったRパッケージのインストール

次の手順でワークショップで用いるRコードの実行に必要なパッケージをインストールします。

1. このリポジトリの内容をダウンロード
2. `240309_jss18tutorial.Rproj`を開く
3. 以下のコードを実行

```r
if (require(renv)) {
  # すべてのパッケージのインストールには
  # インターネット接続状況にもよりますが、5分程度かかります
  # Do you want to proceed? という出力が表示されたら
  # コンソールに Y と入力して Enter キーを押してください
  renv::restore()
}
```

### targetsによるRオブジェクトの復元

チュートリアルで用いるいくつかのRオブジェクトは、targetsパッケージを使って管理するようにしています。これにより、追加で行う処理に必要なRオブジェクトを簡単に再現できます。これらのオブジェクトを復元するには、次のコードを実行してください。

```r
# すべてのオブジェクトを復元するには、計算機の能力にもよりますが
# 数分〜数十分かかります。
targets::tar_make()
```

復元されたオブジェクトを利用するには`tar_load()`を使います。例えばチュートリアルで用いるデータセット（都道府県地価調査データ）を手元で復元するには次のようにします。

```r
targets::tar_load(lp_supply)
```

## その他

カラーパレット

```r
scales::show_col(c("#1F4690", "#3A5BA0", "#FFA500", "#FFE5B4"), labels = TRUE, ncol = 4)
```
