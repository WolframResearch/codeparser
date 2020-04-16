
if(NOT DEFINED MATHEMATICA_INSTALL_DIR)
if(CMAKE_HOST_WIN32 OR CYGWIN)
	set(MATHEMATICA_INSTALL_DIR "C:/Program Files/Wolfram Research/Mathematica/12.0")
elseif(CMAKE_HOST_APPLE)
	set(MATHEMATICA_INSTALL_DIR /Applications/Mathematica.app/Contents)
else()
	set(MATHEMATICA_INSTALL_DIR /usr/local/Wolfram/Mathematica/12.0)
endif()
endif()

if(CMAKE_HOST_WIN32 OR CYGWIN)
	set(WOLFRAMKERNEL_DEFAULT ${MATHEMATICA_INSTALL_DIR}/wolfram.exe)
	set(WOLFRAMLIBRARY_INCLUDE_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/IncludeFiles/C)
	#
	# in versions before 11.2, there were 2 separate paths:
	# SystemFiles/Links/MathLink/DeveloperKit/Windows-x86-64/CompilerAdditions/mldev64/include
	# SystemFiles/Links/MathLink/DeveloperKit/Windows-x86-64/CompilerAdditions/mldev64/lib
	#
	# starting in 11.2, the single path for MathLink includes and MathLink libs is:
	# SystemFiles/Links/MathLink/DeveloperKit/Windows-x86-64/CompilerAdditions
	#
	set(MATHLINK_INCLUDE_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/Links/MathLink/DeveloperKit/Windows-x86-64/CompilerAdditions)
	set(MATHLINK_LIB_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/Links/MathLink/DeveloperKit/Windows-x86-64/CompilerAdditions)
elseif (CMAKE_HOST_APPLE)
	set(WOLFRAMKERNEL_DEFAULT ${MATHEMATICA_INSTALL_DIR}/MacOS/WolframKernel)
	set(WOLFRAMLIBRARY_INCLUDE_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/IncludeFiles/C)
	set(MATHLINK_INCLUDE_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions)
	set(MATHLINK_LIB_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions)
else()
	set(WOLFRAMKERNEL_DEFAULT ${MATHEMATICA_INSTALL_DIR}/Executables/WolframKernel)
	set(WOLFRAMLIBRARY_INCLUDE_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/IncludeFiles/C)
	set(MATHLINK_INCLUDE_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/Links/MathLink/DeveloperKit/Linux-x86-64/CompilerAdditions)
	set(MATHLINK_LIB_DIR_DEFAULT ${MATHEMATICA_INSTALL_DIR}/SystemFiles/Links/MathLink/DeveloperKit/Linux-x86-64/CompilerAdditions)
endif()

macro(CheckWolframKernel)

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
			#
			# Evidence suggests that when bug 349779 strikes, the kernel does exit after 30 minutes
			# So double that and cross fingers.
			#
			# Related bugs: 349779
			# Related issues: RE-514227
			#
			3600
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
			#
			# Evidence suggests that when bug 349779 strikes, the kernel does exit after 30 minutes
			# So double that and cross fingers.
			#
			# Related bugs: 349779
			# Related issues: RE-514227
			#
			3600
		RESULT_VARIABLE
			SYSTEMID_RESULT
	)

	message(STATUS "SYSTEMID: ${SYSTEMID}")

	if(NOT ${SYSTEMID_RESULT} EQUAL "0")
		message(WARNING "Bad exit code from SystemID script: ${SYSTEMID_RESULT}; Continuing")
	endif()

endmacro(CheckWolframKernel)
