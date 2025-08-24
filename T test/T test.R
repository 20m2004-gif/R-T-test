
################################################################################
# 2種類の血圧データのT検定
################################################################################

################################################################################
# ①薬とプラセボの効果を比較する検定【対応のない2群の平均の検定】
################################################################################

# 作業ディレクトリの確認
getwd()

# CSV読み込み
data_pre <- read.csv("./data/blood.pre.csv")

# データの基本情報（平均や中央値なそ）を確認
summary(data_pre)

# group列を因子型に変換
# グループを正しく扱えるようにする
data_pre$group <- as.factor(data_pre$group)

# 因子型に変換された後のデータを確認
summary(data_pre)

#-------------------------------------------------------------------------------
# ここからデータの可視化
#-------------------------------------------------------------------------------

# Macのみ記載
par(family = "HirakakuProN-W3") # 文字化けするため

# 箱ひげ図を作成
boxplot(reduction ~ group, data = data_pre,
        main = "グループ別 血圧低下量の箱ひげ図",
        xlab = "群（Group）",
        ylab = "血圧の低下量",
        col = c("skyblue", "salmon"))
# 薬のグループとプラセボのグループの血圧低下量の分布を視覚的に比較

# 必要であれば以下のパッケージをインストールする
# showtext：日本語やカスタムフォントを綺麗に表示できるようにするツール
# sysfonts：システムフォントを扱うためのツール
# install.packages(c("showtext", "sysfonts"))

# それぞれのパッケージを読み込み
library(sysfonts)  # font_add(), font_add_google() はここ
library(showtext)  # showtext_auto() はここ
library(ggplot2)

# フォント登録（最初の1回だけでOK）
# ggplot2用の日本語フォントを登録
# Macのシステムフォント「ヒラギノ角ゴシック W3」を使用
font_add(family = "hiragino",
         regular = "/System/Library/Fonts/ヒラギノ角ゴシック W3.ttc")

# showtext を有効化
# これ以降に作成されるグラフに、登録したフォントが自動で適用される
showtext_auto()

# ここから先で作る ggplot にフォントが効く
# ggplot2パッケージを使った箱ひげ図の作成
# このコードは、R標準のboxplot()よりも美的でカスタマイズ性の高いグラフを作成できる
ggplot(data_pre, aes(x = group, y = reduction, fill = group)) +
  geom_boxplot() +
  labs(title = "グループ別 血圧低下量の箱ひげ図",
       x = "群（Group）",
       y = "血圧の低下量") +
  theme_minimal() +
  theme(text = element_text(family = "hiragino")) +
  scale_fill_manual(values = c("Placebo" = "lightblue",
                               "Drug" = "lightgreen"))

#-------------------------------------------------------------------------------
# F検定とT検定
#-------------------------------------------------------------------------------

# 【F検定】
# 検定の前提となる等分散の確認（F検定）
# reduction（血圧低下量）の分散がグループ間で等しいかどうかを検証
var_test <- var.test(reduction ~ group, data = data_pre)
print(var_test)

# 【T検定：Student's T-test】
# 等分散を仮定したT検定の実行
# F検定で分散が等しいと判断された場合に用いる
# 2つのグループの平均値に統計的に有意な差があるかを確認
t_result <- t.test(reduction ~ group, data = data_pre, 
                   var.equal = TRUE)
print(t_result)

# 【T検定：Welch's T-test】
# WelchのT検定の実行（等分散を仮定しない場合）
# F検定の結果に関わらず、ロバストな検定として実務でよく使われる
# この検定は、分散が等しいかどうかの事前判断が不要
t_result <- t.test(reduction ~ group, data = data_pre,
                   var.equal = FALSE)
print(t_result)

#-------------------------------------------------------------------------------
# 結果と結論
#-------------------------------------------------------------------------------
# 【結果】
# F検定のp値：p-value = 0.1835（有意水準：0.05）
# → p値が0.05以上なので、Student's T-test
# Student's T-testから
# p-value = 3.737e-05（<0.001）→ 有意差あり
# 群平均：3.567154（Drug群）, 5.312609（Placebo群）

# 【結論】
# Drug群はPlacebo群と比較して血圧低下量が有意に小さかった。
# よって、このデータでは薬の効果はプラセボよりも弱い可能性が示唆される。

#-------------------------------------------------------------------------------
# ※注意※
# このT検定の方法は理論的な学習としては問題ないが
# 検定を2回行うことは多重性の問題を引き起こすことになるので
# 実務的には初めからWelchの検定を行う場合を検討すべき

################################################################################
# ②朝と夕方の血圧変化を比較する検定【対応のある2群の平均の検定】
################################################################################

# 必要であれば以下のパッケージをインストールする
# readrをインストール
# install.packages("readr")

# 必要なパッケージの読み込み（readrとtidyr）
library(readr) # CSVファイル読み込み高速化
library(tidyr) # データの整形

# CSVの読み込み
data_pare <- read_csv("./data/blood.pare.csv")

# ロング形式に変換（ペアプロットのため）
data_long <- pivot_longer(data_pare, cols = c("morning", "evening"),
                          names_to = "time", values_to = "bp")

# time列の順序を明示的に指定
data_long$time <- factor(data_long$time, levels = c("morning", "evening"))

# ペアプロット作成（同一被験者の朝→夕方を線で結んで可視化）
ggplot(data_long, aes(x = time, y = bp, group = subject_id)) + 
  geom_line(color = "gray") + # 被験者ごとの変化を線で示す
  geom_point(aes(color = time), size = 3) + # 測定時点の点をプロット
  labs(title = "朝と夕方の血圧のペアプロット",
       x = "測定時間帯",
       y = "血圧（mmHg）") + 
  theme_minimal() + # ←消すと背景がグレーになる
  scale_color_manual(values = c("morning" = "skyblue",
                                "evening" = "salmon"))

#-------------------------------------------------------------------------------
# T検定
#-------------------------------------------------------------------------------
# 対応のあるT検定 (Paired t-test)
t_result_paired <- t.test(data_pare$morning, data_pare$evening, paired = TRUE)
print(t_result_paired)

#-------------------------------------------------------------------------------
# 結果と結論
#-------------------------------------------------------------------------------
# 【結果】
# p値：p-value = 8.823e-13（有意水準：0.05） → 有意差あり
# 朝と夕方の血圧の平均差：11.24214

# 【結論】
# 同一被験者において、夕方の血圧は朝よりも有意に高かった。
# 平均すると約11 mmHgの上昇が認められた。

