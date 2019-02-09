
set(LIB_CPP_SOURCES
	${CMAKE_SOURCE_DIR}/AST/cpp/src/ByteDecoder.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/ByteEncoder.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/CharacterDecoder.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/Node.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/Parselet.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/Parser.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/SourceManager.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/SyntaxIssue.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/ToInputFormString.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/Tokenizer.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/Utils.cpp
)

add_library(wl-ast-lib SHARED
	${GENERATED_CPP_INCLUDES}
	${GENERATED_CPP_SOURCES}
	${LIB_CPP_INCLUDES}
	${LIB_CPP_SOURCES}
)

target_include_directories(wl-ast-lib
	PRIVATE ${CMAKE_SOURCE_DIR}/AST/cpp/include
	PRIVATE ${CMAKE_BINARY_DIR}/generated/cpp/include
)

set(MAX_EXPRESSION_DEPTH 100 CACHE STRING "Maximum depth allowed by the parser")
set(MAX_EXPRESSION_BREADTH 1000 CACHE STRING "Maximum breadth allowed by the parser")

message(STATUS "MAX_EXPRESSION_DEPTH: ${MAX_EXPRESSION_DEPTH}")
message(STATUS "MAX_EXPRESSION_BREADTH: ${MAX_EXPRESSION_BREADTH}")

target_compile_definitions(wl-ast-lib
	PRIVATE VERSION_NUMBER=${VERSION_NUMBER}
	PRIVATE MAX_EXPRESSION_DEPTH=${MAX_EXPRESSION_DEPTH}
	PRIVATE MAX_EXPRESSION_BREADTH=${MAX_EXPRESSION_BREADTH}
)

# get $SystemID
execute_process(
	COMMAND ${WOLFRAMKERNEL} -noinit -script ${CMAKE_SOURCE_DIR}/scripts/SystemID.wl
	OUTPUT_VARIABLE SYSTEMID
	OUTPUT_STRIP_TRAILING_WHITESPACE
	WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)

message(STATUS "SYSTEMID: ${SYSTEMID}")

set_target_properties(wl-ast-lib PROPERTIES
	OUTPUT_NAME
		wl-ast
	CXX_STANDARD
		11
	CXX_STANDARD_REQUIRED
		ON
	LIBRARY_OUTPUT_DIRECTORY
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/${PACLET}Resources/${SYSTEMID}
	# make sure that it is not created inside ${SYSTEMID}/Debug on Multi-configuration generators (VS, Xcode)
	LIBRARY_OUTPUT_DIRECTORY_DEBUG
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/${PACLET}Resources/${SYSTEMID}
	# make sure that it is not created inside ${SYSTEMID}/Release on Multi-configuration generators (VS, Xcode)
	LIBRARY_OUTPUT_DIRECTORY_RELEASE
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/${PACLET}Resources/${SYSTEMID}
)

#
# Setup warnings
#
if(MSVC)
	target_compile_options(wl-ast-lib
		# specify /MT to statically link runtime on Windows
		# This prevents "The program can't start because ucrtbased.dll is missing from your computer" error on Windows 7
		PRIVATE /W3 /EHsc /MT
	)
else()
	target_compile_options(wl-ast-lib
		PRIVATE -Wextra -Wall -Weffc++ -Wno-unused-parameter -Wno-unused-function -Wno-comment
	)
endif(MSVC)
