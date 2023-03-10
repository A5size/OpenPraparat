#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.71])
AC_INIT([OpenPraparat],[0.1],[BUG-REPORT-ADDRESS])
AC_CONFIG_SRCDIR([src/gui.cpp])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_AWK
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP
AC_PROG_FC
AC_PROG_LN_S
AM_INIT_AUTOMAKE
AC_PROG_MAKE_SET
# FIXME: Replace 'main' with a function in '-lquadmath':
AC_CHECK_LIB([quadmath], [main])
# FIXME: Replace 'main' with a function in '-lGL':
AC_CHECK_LIB([GL], [main])
# FIXME: Replace 'main' with a function in '-lGLU':
AC_CHECK_LIB([GLU], [main])
# FIXME: Replace 'main' with a function in '-lgdi32':
AC_CHECK_LIB([gdi32], [main])
# FIXME: Replace 'main' with a function in '-lgfortran':
AC_CHECK_LIB([gfortran], [main])
# FIXME: Replace 'main' with a function in '-lglfw':
AC_CHECK_LIB([glfw], [main])
# FIXME: Replace 'main' with a function in '-lglfw3':
AC_CHECK_LIB([glfw3], [main])
# FIXME: Replace 'main' with a function in '-limm32':
AC_CHECK_LIB([imm32], [main])
# FIXME: Replace 'main' with a function in '-lopengl32':
AC_CHECK_LIB([opengl32], [main])
# FIXME: Replace 'main' with a function in '-lpthread':
AC_CHECK_LIB([pthread], [main])

# Checks for libraries.
# FIXME: Replace 'main' with a function in '-lGL':
AC_CHECK_LIB([GL], [main])
# FIXME: Replace 'main' with a function in '-lGLU':
AC_CHECK_LIB([GLU], [main])
# FIXME: Replace 'main' with a function in '-lgdi32':
AC_CHECK_LIB([gdi32], [main])
# FIXME: Replace 'main' with a function in '-lgfortran':
AC_CHECK_LIB([gfortran], [main])
# FIXME: Replace 'main' with a function in '-lglfw':
AC_CHECK_LIB([glfw], [main])
# FIXME: Replace 'main' with a function in '-lglfw3':
AC_CHECK_LIB([glfw3], [main])
# FIXME: Replace 'main' with a function in '-lglu32':
AC_CHECK_LIB([glu32], [main])
# FIXME: Replace 'main' with a function in '-limm32':
AC_CHECK_LIB([imm32], [main])
# FIXME: Replace 'main' with a function in '-lopengl32':
AC_CHECK_LIB([opengl32], [main])
# FIXME: Replace 'main' with a function in '-lpthread':
AC_CHECK_LIB([pthread], [main])

# Checks for header files.
AC_CHECK_HEADERS([malloc.h stdint.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_TYPE_UID_T
AC_C_INLINE
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_SIZE_T
AC_CHECK_MEMBERS([struct stat.st_blksize])
AC_STRUCT_ST_BLOCKS
AC_CHECK_MEMBERS([struct stat.st_rdev])
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T
AC_CHECK_TYPES([ptrdiff_t])

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_FSEEKO
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_STRCOLL
AC_CHECK_FUNCS([atexit clock_gettime floor localeconv memchr memmove memset pow realpath setlocale sqrt strchr strstr])

AC_CONFIG_FILES([Makefile src/Makefile])
AC_OUTPUT
