# OpenPraparat

##Paper
Artificial Life using a Book and Bookmarker [arXiv link](https://arxiv.org/abs/2210.12854)
>Reproduction, development, and individual interactions are essential topics in artificial life. The cellular automata, which can handle these in a composite way, is highly restricted in its form and behavior because it represents life as a pattern of cells. In contrast, the virtual creatures proposed by Karl Sims have a very high degree of freedom in terms of morphology and behavior. However, they have limited expressive capacity in terms of those viewpoints. This study carefully extracts the characteristics of the cellular automata and Sims models to propose a new artificial life model that can simulate reproduction, development, and individual interactions while exhibiting high expressive power for morphology and behavior. The simulation was performed by sequentially reading a book with genetic information and repeatedly executing four actions: expansion, connection, disconnection, and transition. The virtual creatures in the proposed model exhibit unique survival strategies and lifestyles and acquire interesting properties in reproduction, development, and individual interactions while having freedom in morphology and behavior.

## Install 

### On Ubuntu
```
sudo apt install libglu1-mesa-dev mesa-common-dev
sudo apt install -y libglfw3 libglfw3-dev

git clone https://github.com/A5size/OpenPraparat
cd ./OpenPraparat/src

make all install
```

### On macOS
```
brew tap homebrew/versions
brew install glfw3

git clone https://github.com/A5size/OpenPraparat
cd ./OpenPraparat/src

make all install
```

