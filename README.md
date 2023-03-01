# OpenPraparat

OpenPraparat is a code that simulates an artificial life model presented in the paper titled "Artificial Life using a Book and Bookmarker." 

>__Artificial Life using a Book and Bookmarker__ [[arXiv](https://arxiv.org/abs/2210.12854)] <br>
>Reproduction, development, and individual interactions are essential topics in artificial life. The cellular automata, which can handle these in a composite way, is highly restricted in its form and behavior because it represents life as a pattern of cells. In contrast, the virtual creatures proposed by Karl Sims have a very high degree of freedom in terms of morphology and behavior. However, they have limited expressive capacity in terms of those viewpoints. This study carefully extracts the characteristics of the cellular automata and Sims models to propose a new artificial life model that can simulate reproduction, development, and individual interactions while exhibiting high expressive power for morphology and behavior. The simulation was performed by sequentially reading a book with genetic information and repeatedly executing four actions: expansion, connection, disconnection, and transition. The virtual creatures in the proposed model exhibit unique survival strategies and lifestyles and acquire interesting properties in reproduction, development, and individual interactions while having freedom in morphology and behavior.

## Install 

### On Ubuntu
Open the terminal, and install the necessary packages using the following commands:
```
sudo apt install build-essential gfortran
sudo apt install libglu1-mesa-dev mesa-common-dev
sudo apt install libglfw3 libglfw3-dev
```

Compile OpenPraparat using the following commands:
```
git clone https://github.com/A5size/OpenPraparat
cd ./OpenPraparat/src

make all install
```

### On macOS
To compile OpenPraparat on macOS, you need to install homebrew. 
The instructions for installing homebrew are omitted here.

Open the terminal, and install the necessary packages using the following commands:
```
brew install gcc
brew install glfw3
```

Compile OpenPraparat using the following commands:
```
git clone https://github.com/A5size/OpenPraparat
cd ./OpenPraparat/src

make all install
```

### On Windows

To compile OpenPraparat on Windows, you will need to install MSYS2. To do so, follow the procedure 1 to 5 outlined in the installation section on the MSYS2 website: https://www.msys2.org/.

Once you have completed procedure 5, close the MSYS2 window and launch mingw64.exe in MSYS2. Then, use the following commands to install the necessary packages:

```
pacman -Sy
pacman -Su

pacman -S mingw-w64-x86_64-gcc
pacman -S make
pacman -S git
pacman -S mingw-w64-x86_64-pkg-config
pacman -S mingw-w64-x86_64-glfw
pacman -S mingw-w64-x86_64-gcc-fortran
pacman -S mingw-w64-x86_64-gcc-libgfortran
```

It is important to execute these commands one line at a time.
Finally, launch mingw64.exe and compile OpenPraparat using the following commands:

```
git clone https://github.com/A5size/OpenPraparat
cd ./OpenPraparat/src

make all install
./link4win.sh
```


## Operation Guide

### praparat_gui
This program launches the GUI to visualize the simulation. To start, click the "Target Directory" button and select the working directory where the input files are located.

#### Keys
Here is a list of some of the keys and the corresponding operation they perform: 
>W: Move forward <br>
>A: Move left <br>
>S: Move backward <br>
>D: Move right <br> 
>Space: Move up <br> 
>F: Move down <br> 
>M: Start simulation <br>
>P: Stop simulation <br>
>H: Hide photon <br>
>C: Change the cell drawing style <br>
>K: Change viewpoint <br>
>R: Change the field drawing style <br>
>Y: Take a screenshot <br>
>V: Start recording (sequentially output images) <br>
>O: Switch between day and night mode <br>
>Q: Exit the program <br>

#### Mouse
Left-click to select cells, right-click and drag to move the viewpoint. 


## Examples
```
tar -zxvf examples.tar.gz 
tree examples
examples
├── 01dumbbell_shaped
│   └── 01normal
│       ├── cells_file
│       ├── field.d
│       ├── input
│       └── seed.ini
└── 02reticulated
    ├── 01normal
    │   ├── cells_file
    │   ├── field.d
    │   └── input
    └── 02fix
        ├── cells_file
        ├── field.d
        ├── fix.ini
        ├── fragment.ini
        ├── input
        └── seed.ini
```

## inputの説明 (Sorry for the Japanese!)
```
!
!各パラメータの説明を書いていますが、
!そのせいで、このまま読み込ませると計算が落ちます。
!使うときは、「!」から始まるコメントを消してください。
!

&cycle
  !コンソール版用の設定
  !10000回に1回セルの状態を保存し、それを5回行う。
  out_cycle = 5
  in_cycle  = 10000
/

&world
  !物理演算用の設定
  WORLD_DT          = 0.01d0
  WORLD_G           = 5.0d0
  BLOCK_EMD_FORCE_K = 2000.0d0
  BLOCK_EMD_FORCE_R = 10.0d0
  CELL_EMD_FORCE_K  = 200.0d0
  CELL_EMD_FORCE_R  = 4.0d0
  AIR_RESISTANCE_K  = 0.01d0

　!フィールドの設定
  FIELD_INIT_FLAG   = 0        ! 0:read, 1:flat, 2:random, 3:maze
  FIELD_ADJUST_FLAG = 0
  FIELD_SIZE_X      = 32
  FIELD_SIZE_Y      = 64
  FIELD_SIZE_Z      = 32
  FIELD_CENTER_X    = 0
  FIELD_CENTER_Y    = 16
  FIELD_CENTER_Z    = 0
  SUN_INIT_FLAG     = 1
  DISTANCE2SUN      = 30.0d0   !地上から太陽までの距離
  SUM_LIMIT_NUM     = 5000     !光を除くセル数が5000を超えたら太陽光を止める
  SUN_CYCLE         = 1000.0d0 !太陽が何ステップで1往復するか
  OUT_INTERVAL_STEP = 1
  DEPTH_LIMIT       = 3
  CONTACT_AREA_CORR = 0.2d0
/

&cell
  !セルの設定
  CELLS_INIT_FLAG             = 1      ! 0:read1, 1:read2, 2:create, 3:read and create
  EVERY_STEP_COST_UPDATE_FLAG = .TRUE.
  EVERY_STEP_COST_E           = 0.01d0
  EVERY_STEP_COST_A           = 1.27d0
  EVERY_STEP_COST_D           = 2.0d-8
  TARGET_CELLS_NUM            = 4000
  TRANS_INTERVAL_STEP         = 200
  RESET_INTERVAL_STEP         = 5
  MUTATION_RATE_FLAG          = 0      ! 0:mr=MRA, 1:circle, 2:liner, 3:sin
  MUTATION_DIVISION_RATE      = 0.5d0
  MUTATION_RATE_AMP           = 0.0d0  !突然変異確率P_A
  MUTATION_COEFF4EXPANSION    = 0.0d0  !突然変異確率P_Bの係数C(P_B = C*P_A)
  NUMBER_OF_CELL              = 15000
  ABS_KEEP_NUM                = 100    !セル数がこの数を下回ると、エネルギー消費が止まる。
  ECEOL_E2L                   = 1.0d0  !エネルギーから光への変換効率
  ECEOL_L2E                   = 1.0d0  !光からエネルギーへの変換効率
  ECEOLR                      = 0.8d0  !赤の光から得られるエネルギーの効率
  ECEOLG                      = 0.2d0  !緑の光から得られるエネルギーの効率
  ECEOLB                      = 0.8d0  !青の光から得られるエネルギーの効率
  ECEOE                       = 1.0d0  ! Energy Conversion Efficiency Of Eat
  ENERGY_TRANSIT_FLAG         = 4
  GENE_COST_C                 = 0.5d0
  BDRC                        = 0.4d0  ! セルの最大半径を決定する(1.0d0まで)
  MIN_R                       = 0.1d0
  SPRING_LIMIT_C              = 1.10d0
  SPRING_CATCH_C              = 1.95d0
  SPRING_BREAK_C              = 2.00d0
  THRESHOLD_LIGHT             = 0.2d0
  RATE_OF_VARI_S              = 0.1d0  ! 結合強度後進パラメータΔs
/

&system
  RANDOM_SEED_FLAG = 2 ! 0:no set, 1:read, 2:random
/

```
