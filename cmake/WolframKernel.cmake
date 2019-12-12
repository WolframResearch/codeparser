
macro(CheckWolframKernel)
	# get $VersionNumber
	execute_process(
		COMMAND ${WOLFRAMKERNEL} -noinit -noprompt -run Print[Floor[100\ \$VersionNumber\ +\ \$ReleaseNumber]]\;Exit[]
		OUTPUT_VARIABLE VERSION_NUMBER
		OUTPUT_STRIP_TRAILING_WHITESPACE
		WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
	)

	message(STATUS "VERSION_NUMBER: ${VERSION_NUMBER}")

	if("${VERSION_NUMBER}" STREQUAL "")
		message(FATAL_ERROR "Wolfram Kernel failed")
	endif()

	if(NOT ${VERSION_NUMBER} GREATER_EQUAL 1100)
		message(FATAL_ERROR "Wolfram Kernel must be at least version 11.0")
	endif()

	# get $SystemID
	execute_process(
		COMMAND ${WOLFRAMKERNEL} -noinit -noprompt -run Print[OutputForm[\$SystemID]]\;Exit[]
		OUTPUT_VARIABLE SYSTEMID
		OUTPUT_STRIP_TRAILING_WHITESPACE
		WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
	)

	message(STATUS "SYSTEMID: ${SYSTEMID}")

	# get Version from PacletInfo.m
	execute_process(
		COMMAND ${WOLFRAMKERNEL} -noinit -noprompt -run Print[OutputForm[Version\ /.\ List\ @@\ Get["${PACLETINFO_SOURCE}"]]]\;Exit[]
		OUTPUT_VARIABLE PACLET_VERSION
		OUTPUT_STRIP_TRAILING_WHITESPACE
		WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
	)

	message(STATUS "PACLET_VERSION: ${PACLET_VERSION}")
endmacro(CheckWolframKernel)
