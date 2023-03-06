***

## cycle
コンソール用の設定。GUIでは読み込まれない

### out_cycle
_default : None_ <br>
セルの保存回数

### in_cycle
_default : None_ <br>
何ステップ回してから保存するか


***

## world

### WORLD_DT
_default : 0.01_ <br>
シミュレーション時間の刻み幅

### WORLD_G
_default : 5.0_ <br>
重力加速度

### BLOCK_EMD_FORCE_K
_default : 2000.0_ <br>
ブロック同士の弾性力の定数

### BLOCK_EMD_FORCE_R
_default : 10.0_ <br>
ブロック同士の弾性力の範囲

### CELL_EMD_FORCE_K
_default : 200.0_ <br>
セル同士の弾性力の定数

### CELL_EMD_FORCE_R
_default : 4.0_ <br>
セル同士の弾性力の範囲

### AIR_RESISTANCE_K
_default : 0.01_ <br>
空気抵抗の定数

### FIELD_INIT_FLAG
_default : 0_ <br>
フィールド初期化の方法
- 0 : read
- 1 : flat
- 2 : random
- 3 : maze

### FIELD_ADJUST_FLAG
_default : 0_ <br>
セルがフィールドに埋もれないようにセルの位置を調整するかどうか
- 0 : しない
- 1 : する

### FIELD_SIZE_X
_default : 128_ <br>
フィールドのX方向のサイズ

### FIELD_SIZE_Y
_default : 128_ <br>
フィールドのY方向のサイズ

### FIELD_SIZE_Z
_default : 128_ <br>
フィールドのZ方向のサイズ

### FIELD_CENTER_X
_default : 0_ <br>
フィールドの中心座標のX成分

### FIELD_CENTER_Y
_default : 16_ <br>
フィールドの中心座標のY成分

### FIELD_CENTER_Z
_default : 0_ <br>
フィールドの中心座標のZ成分

### SUN_INIT_FLAG
_default : 1_ <br>
太陽を初期化するかどうか
- 0 : しない
- 1 : する

### DISTANCE2SUN
_default : 30.0_ <br>
地上から太陽までの距離

### SUM_LIMIT_NUM
_default : 5000_ <br>
光を除くセル数がこの数を超えたら、太陽光を止める

### SUN_CYCLE
_default : 1000.0_ <br>
太陽が何ステップで1往復するか。

### OUT_INTERVAL_STEP
_default : 10_ <br>
シミュレーション情報を出力する間隔(ステップ数)。
GUIでは、自動的に1になる

### DEPTH_LIMIT
_default : 3_ <br>
この深さ以下に落ちたセルは消される

### CONTACT_AREA_CORR
_default : 0.0_ <br>
接触面積補正の定数


***


## cell

### CELLS_INIT_FLAG
_default : 0_ <br>
セルの初期化方法を指定する
- 0 : read1
- 1 : read2
- 2 : create
- 3 : read and create
- 
### EVERY_STEP_COST_UPDATE_FLAG
_default : .FALSE._ <br>
毎ステップEVERY_STEP_COST_Aを更新するかどうか

### EVERY_STEP_COST_E
_default : 0.01d0_ <br>
エネルギー$E$を持つセルが、1ステップで消費するエネルギー量(COST)は次の式で与えられる。
```math
{\rm COST} = {\rm EVERY\_STEP\_COST\_E} \times \cfrac{{\rm EVERY\_STEP\_COST\_A}^{N_C}}{{\rm EVERY\_STEP\_COST\_A}^{N_{\rm max}}} \times E
```
ここで、$N_C$はそのセルの結合数であり、$N_{\rm max}$は結合可能な数の最大値である。
EVERY_STEP_COST_Eは、このエネルギー量の大きさをスケールさせる役割を持つ

エネルギー消費の定数(E)。EVERY_STEP_COST_A、EVERY_STEP_COST_Dと共に使用される。

### EVERY_STEP_COST_A
_default : 2.0d0_ <br>
EVERY_STEP_COST_Aは、結合数に対するペナルティ量として働く

### EVERY_STEP_COST_D
_default : 2.0d-8_ <br>
EVERY_STEP_COST_Aを調整する際のパラメータ

### TARGET_CELLS_NUM
_default : 4000_ <br>
目標とするセルの数。この数になるようにEVERY_STEP_COST_Aが調整される

### TRANS_INTERVAL_STEP
_default : 200_ <br>
遺伝子の読み出し間隔(ステップ数)

### RESET_INTERVAL_STEP
_default : 5_ <br>
結合待ち、結合解除待ちの持続ステップ数

### MUTATION_RATE_FLAG
_default : 0_ <br>
突然変異率を設定する方法を指定する
- 0 : constant
- 1 : circle
- 2 : liner
- 3 : sin

### MUTATION_DIVISION_RATE
_default : 0.5d0_ <br>
分裂時の突然変異率を設定するパラメータ

### MUTATION_RATE_AMP
_default : 0.0d0_ <br>
突然変異確率P_A

### MUTATION_COEFF4EXPANSION
_default : 0.0d0_ <br>
突然変異確率P_Bの係数C(P_B = C*P_A)

### NUMBER_OF_CELL
_default : 15000_ <br>
何個分のセルのメモリを確保するか。
この値がシミュレーション可能なセル数の上限となる。
ここで、光の粒子もメモリ上はセルとして扱われていることに注意。
すなわち、ここで言う上限とは、生物の体を構成しているセルと
光の粒子の合計に対する上限である

### ABS_KEEP_NUM
_default : 100_ <br>
セル数がこの数を下回ると、エネルギー消費量が0になる

### ECEOL_E2L
_default : 1.0d0_ <br>
エネルギーから光への変換効率

### ECEOL_L2E
_default : 1.0d0_ <br>
光からエネルギーへの変換効率

### ECEOLR
_default : 0.8d0_ <br>
赤の光から得られるエネルギーの効率

### ECEOLG
_default : 0.2d0_ <br>
緑の光から得られるエネルギーの効率

### ECEOLB
_default : 0.8d0_ <br>
青の光から得られるエネルギーの効率

### ECEOE
_default : 1.0d0_ <br>
他のセルを食べた際のエネルギー変換効率

### ENERGY_TRANSIT_FLAG
_default : 4_ <br>
エネルギー輸送の方法を指定する
- 0 : 固定値
- 1 : 固定値(多い方から少ない方へ)
- 2 : エネルギー差にスケール(多い方から少ない方へ)
- 3 : エネルギー差とNNの出力の積にスケール(多い方から少ない方へ)
- 4 : エネルギー量とNNの出力の積にスケール
- 5 : エネルギー差にスケール

### ENERGY_TRANSIT_MIN
_default : 1.0d0_ <br>
ENERGY_TRANSIT_FLAGが0か1のときに使われる。
輸送されるエネルギー量を指定する

### ENERGY_TRANSIT_C
_default : 0.1d0_ <br>
ENERGY_TRANSIT_FLAGが2か5のときに使われる。
エネルギー差に、この値が乗じられた量が
輸送されるエネルギー量となる

### GENE_COST_C
_default : 0.1d0_ <br>
セル生成時のコスト係数

### BDRC
_default : 1.0d0_ <br>
セルの最大半径を決定するパラメータ(0.1d0から1.0d0まで)

### MIN_R
_default : 0.2d0_ <br>
セルの最小半径

### SPRING_LIMIT_C
_default : 2.0d0_ <br>
ニューラルネットワークで調整できるバネの自然長の最大値を指定する係数

### SPRING_CATCH_C
_default : 2.0d0_ <br>
近傍セルと結合するときの距離の最大値を指定する係数

### SPRING_BREAK_C
_default : 2.0d0_ <br>
バネの長さが(SPRING_BREAK_C*バネの自然長)を超えると、バネが千切れる

### THRESHOLD_LIGHT
_default : 0.0d0_ <br>
発光しきい値

### RATE_OF_VARI_S
_default : 0.1d0_ <br>
結合強度更新パラメータΔs


***

## system

### RANDOM_SEED_FLAG
_default : 0_ <br>
乱数の設定
- 0 : no set
- 1 : read
- 2 : random

### TRACE_FLAG
_default : 0_ <br>
指定したIDのセルのニューラルネットワーク情報を書き出すかどうか。
trace.iniファイルが必要
- 0 : しない
- 1 : する

### FIX_FLAG
_default : 0_ <br>
指定したIDのセルを固定するかどうか。
fix.iniファイルが必要
- 0 : しない
- 1 : 位置固定
- 2 : 位置とエネルギー固定

### FRAGMENT_FLAG
_default : 0_ <br>
指定したフラグメントを持つセルの情報を書き出すかどうか。
fragment.iniファイルが必要
- 0 : しない
- 1 : 指定したフラグメントの数を数える
- 2 : 指定したフラグメントを持つセルの情報を書き出す

### FRAGMENT_INTERVAL_STEP
_default : 1_ <br>
FRAGMENT_FLAGが0でないとき、その書き出し間隔(ステップ数)を指定する
