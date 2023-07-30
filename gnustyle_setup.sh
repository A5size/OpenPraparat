#!/bin/sh
touch NEWS README AUTHORS ChangeLog
cat <<EOF > COPYING
MIT License

Copyright (c) $(date +%Y) A5size

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
EOF
autoscan
sed -e "s/FULL-PACKAGE-NAME/OpenPraparat/" -i configure.scan
sed -e "s/VERSION/0.0.1-beta/" -i configure.scan
sed -e "13aAC_PROG_FC" -i configure.scan
sed -e "15aAM_INIT_AUTOMAKE([subdir-objects])" -i configure.scan
sed -e "15aLT_INIT([disable-shared])" -i configure.scan
sed -e "15aAC_PROG_LIBTOOL" -i configure.scan
sed -e "15aAM_PROG_AR" -i configure.scan
sed -e "15aAC_SUBST(LIBTOOL_DEPS)" -i configure.scan
sed -e "17a# FIXME: Replace 'main' with a function in '-lquadmath':\nAC_CHECK_LIB([quadmath], [main])\n# FIXME: Replace 'main' with a function in '-lGL':\nAC_CHECK_LIB([GL], [main])\n# FIXME: Replace 'main' with a function in '-lGLU':\nAC_CHECK_LIB([GLU], [main])\n# FIXME: Replace 'main' with a function in '-lgdi32':\nAC_CHECK_LIB([gdi32], [main])\n# FIXME: Replace 'main' with a function in '-lgfortran':\nAC_CHECK_LIB([gfortran], [main])\n# FIXME: Replace 'main' with a function in '-lglfw':\nAC_CHECK_LIB([glfw], [main])\n# FIXME: Replace 'main' with a function in '-lglfw3':\nAC_CHECK_LIB([glfw3], [main])\n# FIXME: Replace 'main' with a function in '-limm32':\nAC_CHECK_LIB([imm32], [main])\n# FIXME: Replace 'main' with a function in '-lopengl32':\nAC_CHECK_LIB([opengl32], [main])\n# FIXME: Replace 'main' with a function in '-lpthread':\nAC_CHECK_LIB([pthread], [main])" -i configure.scan
#sed -e "s/src\/Makefile/Makefile src\/Makefile/" -i configure.scan
mv configure.scan configure.ac
echo SUBDIRS=src >> Makefile.am
cat <<EOF > src/Makefile.am
bin_PROGRAMS=praparat_builder praparat_gui praparat_deb praparat_cui
IMGUI_DIR = ./imgui
praparat_builder_SOURCES=builder.cpp \$(IMGUI_DIR)/imgui.cpp \$(IMGUI_DIR)/imgui_demo.cpp \$(IMGUI_DIR)/imgui_draw.cpp \$(IMGUI_DIR)/imgui_tables.cpp \$(IMGUI_DIR)/imgui_widgets.cpp \$(IMGUI_DIR)/backends/imgui_impl_glfw.cpp \$(IMGUI_DIR)/backends/imgui_impl_opengl2.cpp lib/ImGuiFileDialog/ImGuiFileDialog.cpp  \$(IMGUI_DIR)imconfig.h  \$(IMGUI_DIR)imgui_internal.h  \$(IMGUI_DIR)imstb_textedit.h \$(IMGUI_DIR)imgui.h \$(IMGUI_DIR)imstb_rectpack.h  \$(IMGUI_DIR)imstb_truetype.h \$(IMGUI_DIR)backends/imgui_impl_glfw.h \$(IMGUI_DIR)backends/imgui_opengl2_glfw.h
praparat_gui_SOURCES=gui.cpp math.f90 praparat.f90 \$(IMGUI_DIR)/imgui.cpp \$(IMGUI_DIR)/imgui_demo.cpp \$(IMGUI_DIR)/imgui_draw.cpp \$(IMGUI_DIR)/imgui_tables.cpp \$(IMGUI_DIR)/imgui_widgets.cpp \$(IMGUI_DIR)/backends/imgui_impl_glfw.cpp \$(IMGUI_DIR)/backends/imgui_impl_opengl2.cpp \$(IMGUI_DIR)imconfig.h  \$(IMGUI_DIR)imgui_internal.h  \$(IMGUI_DIR)imstb_textedit.h \$(IMGUI_DIR)imgui.h \$(IMGUI_DIR)imstb_rectpack.h  \$(IMGUI_DIR)imstb_truetype.h lib/ImGuiFileDialog/ImGuiFileDialog.cpp lib/ImGuiFileDialog/ImGuiFileDialog.h lib/ImGuiFileDialog/ImGuiFileDialogConfig.h \$(IMGUI_DIR)backends/imgui_impl_glfw.h \$(IMGUI_DIR)backends/imgui_opengl2_glfw.h
praparat_gui_FFLAGS = -O2 -cpp
praparat_deb_SOURCES=math.f90 praparat.f90 main.f90
praparat_deb_FFLAGS =-Wall -fbounds-check -O0 -Wuninitialized -fbacktrace -g -cpp
praparat_cui_SOURCES=math.f90 praparat.f90 main.f90
praparat_cui_FFLAGS = -O2 -cpp
praparat_SOURCES_FFLAGS=-O3
CXXFLAGS = -std=c++17 -I\$(IMGUI_DIR) -I\$(IMGUI_DIR)/backends -Wall -Wformat -Wall -fbounds-check -O0 -Wuninitialized -g
EOF
aclocal
autoheader
autoconf
automake --add-missing --copy --force-missing
autoreconf -fiv
echo "please! ./configure && make!"
