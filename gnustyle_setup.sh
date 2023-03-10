#!/bin/sh
touch NEWS README AUTHORS ChangeLog COPYING
autoscan
sed -e "s/FULL-PACKAGE-NAME/OpenPraparat/" -i configure.scan
sed -e "s/VERSION/0.1/" -i configure.scan
sed -e "13aAC_PROG_FC" -i configure.scan
sed -e "15aAM_INIT_AUTOMAKE" -i configure.scan
sed -e "17a# FIXME: Replace 'main' with a function in '-lquadmath':\nAC_CHECK_LIB([quadmath], [main])\n# FIXME: Replace 'main' with a function in '-lGL':\nAC_CHECK_LIB([GL], [main])\n# FIXME: Replace 'main' with a function in '-lGLU':\nAC_CHECK_LIB([GLU], [main])\n# FIXME: Replace 'main' with a function in '-lgdi32':\nAC_CHECK_LIB([gdi32], [main])\n# FIXME: Replace 'main' with a function in '-lgfortran':\nAC_CHECK_LIB([gfortran], [main])\n# FIXME: Replace 'main' with a function in '-lglfw':\nAC_CHECK_LIB([glfw], [main])\n# FIXME: Replace 'main' with a function in '-lglfw3':\nAC_CHECK_LIB([glfw3], [main])\n# FIXME: Replace 'main' with a function in '-limm32':\nAC_CHECK_LIB([imm32], [main])\n# FIXME: Replace 'main' with a function in '-lopengl32':\nAC_CHECK_LIB([opengl32], [main])\n# FIXME: Replace 'main' with a function in '-lpthread':\nAC_CHECK_LIB([pthread], [main])" -i configure.scan
sed -e "s/src\/Makefile/Makefile src\/Makefile/" -i configure.scan
mv configure.scan configure.ac
echo SUBDIRS=src >> Makefile.am
cat <<EOF > src/Makefile.am
bin_PROGRAMS=praparat_builder praparat_gui praparat_deb praparat_cui
IMGUI_DIR = ./imgui
praparat_builder_SOURCES=builder.cpp \$(IMGUI_DIR)/imgui.cpp \$(IMGUI_DIR)/imgui_demo.cpp \$(IMGUI_DIR)/imgui_draw.cpp \$(IMGUI_DIR)/imgui_tables.cpp \$(IMGUI_DIR)/imgui_widgets.cpp \$(IMGUI_DIR)/backends/imgui_impl_glfw.cpp \$(IMGUI_DIR)/backends/imgui_impl_opengl2.cpp lib/ImGuiFileDialog/ImGuiFileDialog.cpp  \$(IMGUI_DIR)imconfig.h  \$(IMGUI_DIR)imgui_internal.h  \$(IMGUI_DIR)imstb_textedit.h \$(IMGUI_DIR)imgui.h \$(IMGUI_DIR)imstb_rectpack.h  \$(IMGUI_DIR)imstb_truetype.h
praparat_gui_SOURCES=gui.cpp math.f90 praparat.f90 \$(IMGUI_DIR)/imgui.cpp \$(IMGUI_DIR)/imgui_demo.cpp \$(IMGUI_DIR)/imgui_draw.cpp \$(IMGUI_DIR)/imgui_tables.cpp \$(IMGUI_DIR)/imgui_widgets.cpp \$(IMGUI_DIR)/backends/imgui_impl_glfw.cpp \$(IMGUI_DIR)/backends/imgui_impl_opengl2.cpp \$(IMGUI_DIR)imconfig.h  \$(IMGUI_DIR)imgui_internal.h  \$(IMGUI_DIR)imstb_textedit.h \$(IMGUI_DIR)imgui.h \$(IMGUI_DIR)imstb_rectpack.h  \$(IMGUI_DIR)imstb_truetype.h lib/ImGuiFileDialog/ImGuiFileDialog.cpp lib/ImGuiFileDialog/ImGuiFileDialog.h lib/ImGuiFileDialog/ImGuiFileDialogConfig.h
praparat_gui_FFLAGS = -Wall -fbounds-check -O0 -Wuninitialized -fbacktrace -g
praparat_deb_SOURCES=math.f90 praparat.f90 main.f90
praparat_deb_FFLAGS =-Wall -fbounds-check -O0 -Wuninitialized -fbacktrace -g
praparat_cui_SOURCES=math.f90 praparat.f90 main.f90
praparat_SOURCES_FFLAGS=-O3
CXXFLAGS = -std=c++17 -I\$(IMGUI_DIR) -I\$(IMGUI_DIR)/backends -Wall -Wformat -Wall -fbounds-check -O0 -Wuninitialized -g
EOF
aclocal
autoheader
autoconf
autoupdate
automake --add-missing --copy --force-missing
echo "please! ./configure && make!"
