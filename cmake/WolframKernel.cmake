
macro(CheckWolframKernel)
	# get $VersionNumber
	execute_process(
		COMMAND
			${WOLFRAMKERNEL} -noinit -noprompt -run Print[OutputForm[Floor[100\ \$VersionNumber\ +\ \$ReleaseNumber]]]\;Exit[]
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
			${WOLFRAMKERNEL} -noinit -noprompt -run Print[OutputForm[\$SystemID]]\;Exit[]
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


	execute_process(
		COMMAND
			${WOLFRAMKERNEL} -noinit -noprompt -run Print[OutputForm[Version\ /.\ List\ @@\ Get["${PACLETINFO_SOURCE}"]]]\;Exit[]
		OUTPUT_VARIABLE
			PACLET_VERSION
		OUTPUT_STRIP_TRAILING_WHITESPACE
		WORKING_DIRECTORY
			${CMAKE_SOURCE_DIR}
		TIMEOUT
			10
		RESULT_VARIABLE
			PACLETINFO_RESULT
	)

	message(STATUS "PACLET_VERSION: ${PACLET_VERSION}")

	if(NOT ${PACLETINFO_RESULT} EQUAL "0")
		message(WARNING "Bad exit code from PacletInfo script: ${PACLETINFO_RESULT}; Continuing")
	endif()

endmacro(CheckWolframKernel)
