
cmake_minimum_required(VERSION 3.14)

find_package(GTest REQUIRED)
if (NOT GTest_FOUND)
    message(FATAL_ERROR "Cannot find Google Test Framework!")
endif()

#
# Build tests
#

set(CPP_TEST_SOURCES
	${CMAKE_SOURCE_DIR}/cpp/test/TestCharacterDecoder.cpp
)

add_executable(test-exe
	${CPP_TEST_SOURCES}
)

target_include_directories(test-exe
	PRIVATE ${GTEST_INCLUDE_DIRS}
	PRIVATE ${CMAKE_SOURCE_DIR}/cpp/include
	PRIVATE ${CMAKE_BINARY_DIR}/generated/cpp/include
	PRIVATE ${MATHLINK_INCLUDE_DIR}
	PRIVATE ${WOLFRAMLIBRARY_INCLUDE_DIR}
)

target_link_libraries(test-exe ast-lib ${GTEST_BOTH_LIBRARIES})

set_target_properties(test-exe PROPERTIES
	OUTPUT_NAME
		test
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
	target_compile_options(test-exe
		# specify /MT to statically link runtime on Windows
		# This prevents "The program can't start because ucrtbased.dll is missing from your computer" error on Windows 7
		PRIVATE /W3 /EHsc /MT
	)
else(MSVC)
	target_compile_options(test-exe
		PRIVATE -Wextra -Wall -Weffc++ -Wno-unused-parameter -Wno-unused-function -Wno-comment
	)
endif(MSVC)



#
# setup installation
#

install(TARGETS test-exe
	DESTINATION bin)

if(APPLE)

install(TARGETS ast-lib
	DESTINATION lib)

#
# mathlink.framework has rpath @executable_path/../Frameworks, so accomodate that
#

install(DIRECTORY ${MATHLINK_LIB}
	DESTINATION Frameworks)

elseif(WIN32)

#
# On Windows, link to .lib, and install the .dll
#

set(MATHLINK_DLL_DIR /Applications/Mathematica.app/Contents/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions CACHE FILEPATH "Path to MathLink DLL")

message(STATUS "MATHLINK_DLL_DIR: ${MATHLINK_DLL_DIR}")

#
# find_library does not find DLLs on Windows
#
find_file(MATHLINK_DLL
	NAMES ml64i4.dll ml32i4.dll
	HINTS ${MATHLINK_DLL_DIR}
)

message(STATUS "MATHLINK_DLL: ${MATHLINK_DLL}")

#
# install AST.dll and ml64i4.dll to same directory as ast.exe (bin)
#

install(TARGETS ast-lib
	DESTINATION bin)

install(FILES ${MATHLINK_DLL}
	DESTINATION bin)

endif(APPLE)

