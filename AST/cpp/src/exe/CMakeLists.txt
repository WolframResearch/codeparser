
set(EXE_CPP_SOURCES
	${CMAKE_SOURCE_DIR}/AST/cpp/src/main.cpp
)

add_executable(wl-ast-exe
	${EXE_CPP_SOURCES}
)

target_include_directories(wl-ast-exe
	PRIVATE ${CMAKE_SOURCE_DIR}/AST/cpp/include
	PRIVATE ${CMAKE_BINARY_DIR}/generated/cpp/include
)

target_link_libraries(wl-ast-exe wl-ast-lib)

set_target_properties(wl-ast-exe PROPERTIES
	OUTPUT_NAME
		wl-ast
	CXX_STANDARD
		11
	CXX_STANDARD_REQUIRED
		ON
	RUNTIME_OUTPUT_DIRECTORY
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/${PACLET}Resources/${SYSTEMID}
	# make sure that it is not created inside ${SYSTEMID}/Debug on Multi-configuration generators (VS, Xcode)
	RUNTIME_OUTPUT_DIRECTORY_DEBUG
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/${PACLET}Resources/${SYSTEMID}
	# make sure that it is not created inside ${SYSTEMID}/Release on Multi-configuration generators (VS, Xcode)
	RUNTIME_OUTPUT_DIRECTORY_RELEASE
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/${PACLET}Resources/${SYSTEMID}
)

#
# Setup warnings
#
if(MSVC)
	target_compile_options(wl-ast-exe
		# specify /MT to statically link runtime on Windows
		# This prevents "The program can't start because ucrtbased.dll is missing from your computer" error on Windows 7
		PRIVATE /W3 /EHsc /MT
	)
else()
	target_compile_options(wl-ast-exe
		PRIVATE -Wextra -Wall -Weffc++ -Wno-unused-parameter -Wno-unused-function -Wno-comment
	)
endif(MSVC)
