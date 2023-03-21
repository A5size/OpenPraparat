# OpenPraparat

OpenPraparat is a code that simulates an artificial life model presented in the paper titled "Artificial Life using a Book and Bookmarker." 

>__Artificial Life using a Book and Bookmarker__ [[arXiv](https://arxiv.org/abs/2210.12854)] <br>
>Reproduction, development, and individual interactions are essential topics in artificial life. The cellular automata, which can handle these in a composite way, is highly restricted in its form and behavior because it represents life as a pattern of cells. In contrast, the virtual creatures proposed by Karl Sims have a very high degree of freedom in terms of morphology and behavior. However, they have limited expressive capacity in terms of those viewpoints. This study carefully extracts the characteristics of the cellular automata and Sims models to propose a new artificial life model that can simulate reproduction, development, and individual interactions while exhibiting high expressive power for morphology and behavior. The simulation was performed by sequentially reading a book with genetic information and repeatedly executing four actions: expansion, connection, disconnection, and transition. The virtual creatures in the proposed model exhibit unique survival strategies and lifestyles and acquire interesting properties in reproduction, development, and individual interactions while having freedom in morphology and behavior.

## YouTube
An introductory video is available on YouTube.
The video is in Japanese, but English subtitles are available. <br>

#0 Introduction of the game Praparat <br>
[![](https://user-images.githubusercontent.com/41696627/226199549-68aef66d-6c29-46fc-8ed5-cab10b9d80a2.png)](https://www.youtube.com/watch?v=jyWKE1IfTyE)

#1 Dumbbell-Shaped Creature <br>
[![](https://img.youtube.com/vi/1jfdDKxs1CU/0.jpg)](https://www.youtube.com/watch?v=1jfdDKxs1CU)

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

