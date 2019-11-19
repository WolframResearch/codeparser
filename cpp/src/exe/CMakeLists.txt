
#
# require 3.14:
# installing targets from different directories
# https://gitlab.kitware.com/cmake/cmake/merge_requests/2152
#
cmake_minimum_required(VERSION 3.14)

#
# Build executable
#

set(CPP_EXE_SOURCES
	${CMAKE_SOURCE_DIR}/cpp/src/exe/main.cpp
)

add_executable(ast-exe
	${CPP_EXE_SOURCES}
)

target_include_directories(ast-exe
	PRIVATE ${CMAKE_SOURCE_DIR}/cpp/include
	PRIVATE ${CMAKE_BINARY_DIR}/generated/cpp/include
	PRIVATE ${MATHLINK_INCLUDE_DIR}
	PRIVATE ${WOLFRAMLIBRARY_INCLUDE_DIR}
)

target_link_libraries(ast-exe ast-lib)

set_target_properties(ast-exe PROPERTIES
	OUTPUT_NAME
		ast
	CXX_STANDARD
		11
	CXX_STANDARD_REQUIRED
		ON
	INSTALL_RPATH
		"@executable_path/../lib"
)

#
# Setup warnings
#
if(MSVC)
	target_compile_options(ast-exe
		# specify /MT to statically link runtime on Windows
		# This prevents "The program can't start because ucrtbased.dll is missing from your computer" error on Windows 7
		PRIVATE /W3 /EHsc /MT
	)
else(MSVC)
	target_compile_options(ast-exe
		PRIVATE -Wextra -Wall -Weffc++ -Wno-unused-parameter -Wno-unused-function -Wno-comment
	)
endif(MSVC)




#
# setup installation
#

install(TARGETS ast-exe
	DESTINATION bin)

if(APPLE)

install(TARGETS ast-lib
	DESTINATION lib)

#
# mathlink.framework has rpath @executable_path/../Frameworks, so accomodate that
#

#install(DIRECTORY ${MATHLINK_LIB}
#	DESTINATION Frameworks)

elseif(WIN32)

#
# On Windows, link to .lib, and install the .dll
#

#set(MATHLINK_DLL_DIR /Applications/Mathematica.app/Contents/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions CACHE FILEPATH "Path to MathLink DLL")

#message(STATUS "MATHLINK_DLL_DIR: ${MATHLINK_DLL_DIR}")

#
# find_library does not find DLLs on Windows
#
#find_file(MATHLINK_DLL
#	NAMES ml64i4.dll ml32i4.dll
#	HINTS ${MATHLINK_DLL_DIR}
#)

#message(STATUS "MATHLINK_DLL: ${MATHLINK_DLL}")

#
# install AST.dll and ml64i4.dll to same directory as ast.exe (bin)
#

install(TARGETS ast-lib
	DESTINATION bin)

#install(FILES ${MATHLINK_DLL}
#	DESTINATION bin)

endif(APPLE)
