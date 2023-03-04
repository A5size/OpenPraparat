## world

### WORLD_DT
_default : 0.01_
シミュレーション時間の刻み幅

### WORLD_G
_default : 10.0_
重力加速度

### BLOCK_EMD_FORCE_K
_default : 2000.0_
ブロック同士の弾性力の定数

### BLOCK_EMD_FORCE_R
_default : 10.0_
ブロック同士の弾性力の範囲

### CELL_EMD_FORCE_K
_default : 200.0_
セル同士の弾性力の定数

### CELL_EMD_FORCE_R
_default : 4.0_
セル同士の弾性力の範囲

### AIR_RESISTANCE_K
_default : 0.01_
空気抵抗の定数

### FIELD_INIT_FLAG
_default : 0_
フィールド初期化の方法
- 0 : read
- 1 : flat
- 2 : random
- 3 : maze

### FIELD_ADJUST_FLAG
_default : 0_
セルがフィールドに埋もれないようにセルの位置を調整するかどうか
- 0 : しない
- 1 : する

### FIELD_SIZE_X
_default : 32_
フィールドのX方向のサイズ

### FIELD_SIZE_Y
_default : 64_
フィールドのY方向のサイズ

### FIELD_SIZE_Z
_default : 32_
フィールドのZ方向のサイズ

### FIELD_CENTER_X
_default : 0_
フィールドの中心座標のX成分

### FIELD_CENTER_Y
_default : 16_
フィールドの中心座標のY成分

### FIELD_CENTER_Z
_default : 0_
フィールドの中心座標のZ成分

### SUN_INIT_FLAG
_default : 1_
太陽を初期化するかどうか
- 0 : しない
- 1 : する

### DISTANCE2SUN
_default : 40.0_
地上から太陽までの距離

### SUM_LIMIT_NUM
_default : 5000_
光を除くセル数がこの数を超えたら、太陽光を止める

### SUN_CYCLE
_default : 1000.0_
太陽が何ステップで1往復するか。

### OUT_INTERVAL_STEP
_default : 10_
シミュレーション情報を出力する間隔(ステップ数)
GUIでは、自動的に1になる

### DEPTH_LIMIT
_default : 2_
この深さ以下に落ちたセルは消される

### CONTACT_AREA_CORR
_default : 0.0_
接触面積補正の定数



## cell

### CELLS_INIT_FLAG
_default : 0_
セルの初期化方法を指定する
- 0 : read1
- 1 : read2
- 2 : create
- 3 : read and create
- 
### EVERY_STEP_COST_UPDATE_FLAG
_default : .FALSE._
毎ステップEVERY_STEP_COST_Aを更新するかどうか

### EVERY_STEP_COST_E
_default : 0.01d0_
エネルギー消費の定数(E)。EVERY_STEP_COST_A、EVERY_STEP_COST_Dと共に使用される。

### EVERY_STEP_COST_A
_default : 2.0d0_
エネルギー消費の定数(A)。EVERY_STEP_COST_E、EVERY_STEP_COST_Dと共に使用される。

### EVERY_STEP_COST_D
_default : 1.0d-7_
エネルギー消費の定数(D)。EVERY_STEP_COST_E、EVERY_STEP_COST_Aと共に使用される。

### TARGET_CELLS_NUM
_default : None_
目標とするセルの数。この数になるようにEVERY_STEP_COST_Aが調整される

### TRANS_INTERVAL_STEP
_default : 100_
遺伝子の読み出し間隔(ステップ数)

### RESET_INTERVAL_STEP
_default : 5_
結合待ち、結合解除待ちの持続ステップ数

### MUTATION_RATE_FLAG
_default : 0_
突然変異率を設定する方法を指定する
- 0 : mr=MRA
- 1 : circle
- 2 : liner
- 3 : sin

### MUTATION_DIVISION_RATE
_default : 0.1d0_
分裂時の突然変異率を設定するパラメータ

### MUTATION_RATE_AMP
_default : 0.5d0_
突然変異確率P_A

### MUTATION_COEFF4EXPANSION
_default : 2.0d0_
突然変異確率P_Bの係数C(P_B = C*P_A)

### NUMBER_OF_CELL
_default : 50000_
生成するセルの数

### ABS_KEEP_NUM
_default : 100_
セル数がこの数を下回ると、エネルギー消費量が0になる

### ECEOL_E2L
_default : 2000.0d0_
エネルギーから光への変換効率

### ECEOL_L2E
_default : 500.0d0_
光からエネルギーへの変換効率

### ECEOLR
_default : 0.8d0_
赤の光から得られるエネルギーの効率

### ECEOLG
_default : 0.2d0_
緑の光から得られるエネルギーの効率

### ECEOLB
_default : 0.8d0_
青の光から得られるエネルギーの効率

### ECEOE
_default : 0.1d0_
他のセルを食べた際のエネルギー変換効率

### ENERGY_TRANSIT_FLAG
_default : 2_
エネルギー輸送の方法を指定する

### GENE_COST_C
_default : 0.1d0_
セル生成時のコスト係数

### BDRC
_default : 1.0d0_
セルの最大半径を決定するパラメータ(0.1d0から1.0d0まで)

### MIN_R
_default : 0.2d0_
セルの最小半径

### SPRING_LIMIT_C
_default : 2.0d0_
ニューラルネットワークで調整できるバネの自然長の最大値を指定する係数

### SPRING_CATCH_C
_default : 2.0d0_
近傍セルと結合するときの距離の最大値を指定する係数

### SPRING_BREAK_C
_default : 2.0d0_
バネの長さが(SPRING_BREAK_C*バネの自然長)を超えると、バネが千切れる

### THRESHOLD_LIGHT
_default : 0.0d0_
発光しきい値

### RATE_OF_VARI_S
_default : 0.1d0_
結合強度更新パラメータΔs
