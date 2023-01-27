#
# Cross Platform Makefile
#

#CXX = g++
CXX = g++-12
#CXX = clang++

FC = gfortran

EXE_BLD = praparat_builder
EXE_GUI = praparat_gui
EXE_DEB = praparat_deb
EXE_CUI = praparat_cui

IMGUI_DIR = ./imgui

SRC_BLD    = builder.cpp
SRC_BLD   += $(IMGUI_DIR)/imgui.cpp $(IMGUI_DIR)/imgui_demo.cpp $(IMGUI_DIR)/imgui_draw.cpp $(IMGUI_DIR)/imgui_tables.cpp $(IMGUI_DIR)/imgui_widgets.cpp
SRC_BLD   += $(IMGUI_DIR)/backends/imgui_impl_glfw.cpp $(IMGUI_DIR)/backends/imgui_impl_opengl2.cpp
SRC_BLD   += lib/ImGuiFileDialog/ImGuiFileDialog.cpp
OBJS_BLD   = $(addsuffix .o, $(basename $(notdir $(SRC_BLD))))

SRC_GUI    = gui.cpp
SRC_GUI   += math.f90 praparat.f90
SRC_GUI   += $(IMGUI_DIR)/imgui.cpp $(IMGUI_DIR)/imgui_demo.cpp $(IMGUI_DIR)/imgui_draw.cpp $(IMGUI_DIR)/imgui_tables.cpp $(IMGUI_DIR)/imgui_widgets.cpp
SRC_GUI   += $(IMGUI_DIR)/backends/imgui_impl_glfw.cpp $(IMGUI_DIR)/backends/imgui_impl_opengl2.cpp
SRC_GUI   += lib/ImGuiFileDialog/ImGuiFileDialog.cpp
OBJS_GUI   = $(addsuffix .o, $(basename $(notdir $(SRC_GUI))))
FFLAGS_GUI = -Wall -fbounds-check -O0 -Wuninitialized -fbacktrace -g

SRC_DEB    = math.f90 praparat.f90 main.f90
OBJS_DEB   = math_deb.o praparat_deb.o main_deb.o
FFLAGS_DEB = -Wall -fbounds-check -O0 -Wuninitialized -fbacktrace -g

SRC_CUI    = math.f90 praparat.f90 main.f90
OBJS_CUI   = math_cui.o praparat_cui.o main_cui.o
FFLAGS_CUI = -O3


UNAME_S := $(shell uname -s)
LINUX_GL_LIBS = -lGL

CXXFLAGS = -std=c++17 -I$(IMGUI_DIR) -I$(IMGUI_DIR)/backends
CXXFLAGS += -g -Wall -Wformat
CXXFLAGS += -Wall -fbounds-check -O0 -Wuninitialized -g
#CXXFLAGS += -static
CXXFLAGS += -lgfortran -lpthread
LIBS =


##---------------------------------------------------------------------
## BUILD FLAGS PER PLATFORM
##---------------------------------------------------------------------

ifeq ($(UNAME_S), Linux) #LINUX
	ECHO_MESSAGE = "Linux"
	LIBS += -lGL -lGLU `pkg-config --static --libs glfw3`

	CXXFLAGS += `pkg-config --cflags glfw3`
	CFLAGS = $(CXXFLAGS)
endif

ifeq ($(UNAME_S), Darwin) #APPLE
	ECHO_MESSAGE = "Mac OS X"
	LIBS += -framework OpenGL -framework Cocoa -framework IOKit -framework CoreVideo
	LIBS += -L/usr/local/lib -L/opt/homebrew/lib
	LIBS += -lglfw

	CXXFLAGS += -I/usr/local/include -I/opt/local/include -I/opt/homebrew/include
	CFLAGS = $(CXXFLAGS)
endif

ifeq ($(OS), Windows_NT)
	ECHO_MESSAGE = "MinGW"
	LIBS += -lglfw3 -lgdi32 -lopengl32 -limm32

	CXXFLAGS += `pkg-config --cflags glfw3`
	CFLAGS = $(CXXFLAGS)
endif

##---------------------------------------------------------------------
## BUILD RULES
##---------------------------------------------------------------------

all: $(EXE_GUI) $(EXE_BLD) $(EXE_DEB) $(EXE_CUI) 
	@echo Build complete for $(ECHO_MESSAGE)

install:
	mkdir -p ../build
	mv $(EXE_GUI) $(EXE_BLD) $(EXE_DEB) $(EXE_CUI) ../build/


%.o:%.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

%.o:$(IMGUI_DIR)/%.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

%.o:$(IMGUI_DIR)/backends/%.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

%.o:lib/ImGuiFileDialog/%.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<


math.o:math.f90
	$(FC) $(FFLAGS_GUI) -c math.f90 -o math.o

praparat.o:praparat.f90
	$(FC) $(FFLAGS_GUI) -c praparat.f90 -o praparat.o

main.o:main.f90
	$(FC) $(FFLAGS_GUI) -c main.f90 -o main.o


math_deb.o:math.f90
	$(FC) $(FFLAGS_DEB) -c math.f90 -o math_deb.o

praparat_deb.o:praparat.f90
	$(FC) $(FFLAGS_DEB) -c praparat.f90 -o praparat_deb.o

main_deb.o:main.f90
	$(FC) $(FFLAGS_DEB) -c main.f90 -o main_deb.o


math_cui.o:math.f90
	$(FC) $(FFLAGS_CUI) -c math.f90 -o math_cui.o

praparat_cui.o:praparat.f90
	$(FC) $(FFLAGS_CUI) -c praparat.f90 -o praparat_cui.o

main_cui.o:main.f90
	$(FC) $(FFLAGS_CUI) -c main.f90 -o main_cui.o


$(EXE_GUI): $(OBJS_GUI)
	$(CXX) -o $@ $^ $(CXXFLAGS) $(LIBS)

$(EXE_BLD): $(OBJS_BLD)
	$(CXX) -o $@ $^ $(CXXFLAGS) $(LIBS)

$(EXE_DEB): $(OBJS_DEB)
	$(FC) -o $@ $^ $(FFLAGS_DEB) 

$(EXE_CUI): $(OBJS_CUI)
	$(FC) -o $@ $^ $(FFLAGS_CUI) 


clean:
	rm -f $(EXE_GUI) $(EXE_BLD) $(EXE_DEB) $(EXE_CUI) $(OBJS_GUI) $(OBJS_BLD) $(OBJS_DEB) $(OBJS_CUI)

