bin_PROGRAMS=praparat_builder praparat_gui praparat_deb praparat_cui
IMGUI_DIR = ./imgui
praparat_builder_SOURCES=builder.cpp $(IMGUI_DIR)/imgui.cpp $(IMGUI_DIR)/imgui_demo.cpp $(IMGUI_DIR)/imgui_draw.cpp $(IMGUI_DIR)/imgui_tables.cpp $(IMGUI_DIR)/imgui_widgets.cpp $(IMGUI_DIR)/backends/imgui_impl_glfw.cpp $(IMGUI_DIR)/backends/imgui_impl_opengl2.cpp lib/ImGuiFileDialog/ImGuiFileDialog.cpp  $(IMGUI_DIR)imconfig.h  $(IMGUI_DIR)imgui_internal.h  $(IMGUI_DIR)imstb_textedit.h $(IMGUI_DIR)imgui.h $(IMGUI_DIR)imstb_rectpack.h  $(IMGUI_DIR)imstb_truetype.h
praparat_gui_SOURCES=gui.cpp math.f90 praparat.f90 $(IMGUI_DIR)/imgui.cpp $(IMGUI_DIR)/imgui_demo.cpp $(IMGUI_DIR)/imgui_draw.cpp $(IMGUI_DIR)/imgui_tables.cpp $(IMGUI_DIR)/imgui_widgets.cpp $(IMGUI_DIR)/backends/imgui_impl_glfw.cpp $(IMGUI_DIR)/backends/imgui_impl_opengl2.cpp $(IMGUI_DIR)imconfig.h  $(IMGUI_DIR)imgui_internal.h  $(IMGUI_DIR)imstb_textedit.h $(IMGUI_DIR)imgui.h $(IMGUI_DIR)imstb_rectpack.h  $(IMGUI_DIR)imstb_truetype.h lib/ImGuiFileDialog/ImGuiFileDialog.cpp lib/ImGuiFileDialog/ImGuiFileDialog.h lib/ImGuiFileDialog/ImGuiFileDialogConfig.h
praparat_gui_FFLAGS = -Wall -fbounds-check -O0 -Wuninitialized -fbacktrace -g
praparat_deb_SOURCES=math.f90 praparat.f90 main.f90
praparat_deb_FFLAGS =-Wall -fbounds-check -O0 -Wuninitialized -fbacktrace -g
praparat_cui_SOURCES=math.f90 praparat.f90 main.f90
praparat_SOURCES_FFLAGS=-O3
CXXFLAGS = -std=c++17 -I$(IMGUI_DIR) -I$(IMGUI_DIR)/backends -Wall -Wformat -Wall -fbounds-check -O0 -Wuninitialized -g
