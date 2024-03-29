set(
	sources
	CommandLineInterface.cpp CommandLineInterface.h
	main.cpp
)

add_executable(solc ${sources} ${BACKWARD_ENABLE})
add_backward(solc)

target_link_libraries(solc PRIVATE solidity Boost::boost Boost::program_options)

include(GNUInstallDirs)
install(TARGETS solc DESTINATION "${CMAKE_INSTALL_BINDIR}")

if(SOLC_LINK_STATIC AND UNIX AND NOT APPLE)
	# Produce solc as statically linked binary (includes C/C++ standard libraries)
	# This is not supported on macOS, see
	# https://developer.apple.com/library/content/qa/qa1118/_index.html.
	set_target_properties(
		solc PROPERTIES
		LINK_FLAGS -static
		LINK_SEARCH_START_STATIC ON
		LINK_SEARCH_END_STATIC ON
	)
endif()

# LLVM
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
target_link_libraries(solc PUBLIC ${LLVM_LIBS} ${CMAKE_DL_LIBS})
