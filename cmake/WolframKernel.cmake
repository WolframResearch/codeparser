
if(NOT DEFINED INSTALLATION_DIRECTORY)
if(CMAKE_HOST_WIN32 OR CYGWIN)
	set(INSTALLATION_DIRECTORY C:/Program Files/Wolfram Research/Mathematica/11.0/)
elseif(CMAKE_HOST_APPLE)
	set(INSTALLATION_DIRECTORY /Applications/Mathematica.app/Contents/)
endif()
endif()

if(CMAKE_HOST_WIN32 OR CYGWIN)
	set(WOLFRAMKERNEL_DEFAULT ${INSTALLATION_DIRECTORY}/WolframKernel)
	set(WOLFRAMLIBRARY_INCLUDE_DIR_DEFAULT ${INSTALLATION_DIRECTORY}/SystemFiles/IncludeFiles/C)
	set(MATHLINK_INCLUDE_DIR_DEFAULT ${INSTALLATION_DIRECTORY}/SystemFiles/Links/MathLink/DeveloperKit/Windows-x86-64/CompilerAdditions/mldev64/include)
	set(MATHLINK_LIB_DIR_DEFAULT ${INSTALLATION_DIRECTORY}/SystemFiles/Links/MathLink/DeveloperKit/Windows-x86-64/CompilerAdditions/mldev64/lib)
elseif (CMAKE_HOST_APPLE)
	set(WOLFRAMKERNEL_DEFAULT ${INSTALLATION_DIRECTORY}/MacOS/WolframKernel)
	set(WOLFRAMLIBRARY_INCLUDE_DIR_DEFAULT ${INSTALLATION_DIRECTORY}/SystemFiles/IncludeFiles/C)
	set(MATHLINK_INCLUDE_DIR_DEFAULT ${INSTALLATION_DIRECTORY}/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions)
	set(MATHLINK_LIB_DIR_DEFAULT ${INSTALLATION_DIRECTORY}/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions)
endif()

macro(CheckWolframKernel)
	# get $VersionNumber
	execute_process(
		COMMAND
			${WOLFRAMKERNEL} -noinit -noprompt -nopaclet -run Print[OutputForm[Floor[100\ \$VersionNumber\ +\ \$ReleaseNumber]]]\;Exit[]
		OUTPUT_VARIABLE
			VERSION_NUMBER
		OUTPUT_STRIP_TRAILING_WHITESPACE
		WORKING_DIRECTORY
			${CMAKE_SOURCE_DIR}
		TIMEOUT
			10
		RESULT_VARIABLE
			VERSION_NUMBER_RESULT
	)

	message(STATUS "VERSION_NUMBER: ${VERSION_NUMBER}")

	if(NOT ${VERSION_NUMBER} GREATER_EQUAL 1100)
		message(FATAL_ERROR "Wolfram Kernel must be at least version 11.0: ${VERSION_NUMBER}")
	endif()

	if(NOT ${VERSION_NUMBER_RESULT} EQUAL "0")
		message(WARNING "Bad exit code from VersionNumber script: ${VERSION_NUMBER_RESULT}; Continuing")
	endif()

	# get $SystemID
	execute_process(
		COMMAND
			${WOLFRAMKERNEL} -noinit -noprompt -nopaclet -run Print[OutputForm[\$SystemID]]\;Exit[]
		OUTPUT_VARIABLE
			SYSTEMID
		OUTPUT_STRIP_TRAILING_WHITESPACE
		WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
		TIMEOUT
			10
		RESULT_VARIABLE
			SYSTEMID_RESULT
	)

	message(STATUS "SYSTEMID: ${SYSTEMID}")

	if(NOT ${SYSTEMID_RESULT} EQUAL "0")
		message(WARNING "Bad exit code from SystemID script: ${SYSTEMID_RESULT}; Continuing")
	endif()

endmacro(CheckWolframKernel)
