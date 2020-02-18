
if(NOT DEFINED INSTALLATION_DIRECTORY)
if(CMAKE_HOST_WIN32 OR CYGWIN)
	set(INSTALLATION_DIRECTORY C:/Program Files/Wolfram Research/Mathematica/12.0/)
elseif(CMAKE_HOST_APPLE)
	set(INSTALLATION_DIRECTORY /Applications/Mathematica.app/Contents/)
else()
	set(INSTALLATION_DIRECTORY /usr/local/Wolfram/Mathematica/12.0/)
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
	
	#
	# Canary test
	#
	string(TIMESTAMP CANARY_BEFORE "%s")
	execute_process(
		COMMAND
			${WOLFRAMKERNEL} -noinit -noprompt -nopaclet -runfirst Exit[]
		WORKING_DIRECTORY
			${PROJECT_SOURCE_DIR}
		TIMEOUT
			300
		RESULT_VARIABLE
			CANARY_RESULT
	)
	string(TIMESTAMP CANARY_AFTER "%s")

	math(EXPR CANARY_TIME "${CANARY_AFTER} - ${CANARY_BEFORE}")

	message(STATUS "Canary test took ${CANARY_TIME} seconds")

	if(NOT ${CANARY_RESULT} EQUAL "0")
		message(WARNING "Bad exit code from Canary script: ${CANARY_RESULT}; Continuing")
	endif()

	#
	# get $VersionNumber
	#
	execute_process(
		COMMAND
			${WOLFRAMKERNEL} -noinit -noprompt -nopaclet -runfirst Print[OutputForm[Floor[100\ \$VersionNumber\ +\ \$ReleaseNumber]]]\;Exit[]
		OUTPUT_VARIABLE
			VERSION_NUMBER
		OUTPUT_STRIP_TRAILING_WHITESPACE
		WORKING_DIRECTORY
			${PROJECT_SOURCE_DIR}
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

	#
	# get $SystemID
	#
	execute_process(
		COMMAND
			${WOLFRAMKERNEL} -noinit -noprompt -nopaclet -runfirst Print[OutputForm[\$SystemID]]\;Exit[]
		OUTPUT_VARIABLE
			SYSTEMID
		OUTPUT_STRIP_TRAILING_WHITESPACE
		WORKING_DIRECTORY
			${PROJECT_SOURCE_DIR}
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
