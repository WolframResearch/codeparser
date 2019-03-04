
#
# Build executable
#

set(CPP_EXE_SOURCES
	${CMAKE_SOURCE_DIR}/AST/cpp/src/exe/main.cpp
)

add_executable(ast-exe
	${CPP_EXE_SOURCES}
)

target_include_directories(ast-exe
	PRIVATE ${CMAKE_SOURCE_DIR}/AST/cpp/include
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
else()
	target_compile_options(ast-exe
		PRIVATE -Wextra -Wall -Weffc++ -Wno-unused-parameter -Wno-unused-function -Wno-comment
	)
endif(MSVC)
