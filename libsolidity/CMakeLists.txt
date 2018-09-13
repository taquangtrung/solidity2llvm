# Until we have a clear separation, libjulia has to be included here
file(GLOB_RECURSE sources "*.cpp" "../libjulia/*.cpp")
file(GLOB_RECURSE headers "*.h" "../libjulia/*.h")

# find_package(CLANG QUIET)
# if (${CLANG_FOUND})
#   include_directories(${CLANG_INCLUDE_DIR})
#   message("CLANG library found.")
# else()
#   message("CLANG library NOT found.")
# endif()


find_package(Z3 QUIET)
if (${Z3_FOUND})
  include_directories(${Z3_INCLUDE_DIR})
  add_definitions(-DHAVE_Z3)
  message("Z3 SMT solver found. This enables optional SMT checking with Z3.")
  list(REMOVE_ITEM sources "${CMAKE_CURRENT_SOURCE_DIR}/formal/CVC4Interface.cpp")
else()
  list(REMOVE_ITEM sources "${CMAKE_CURRENT_SOURCE_DIR}/formal/Z3Interface.cpp")
  find_package(GMP QUIET)
  find_package(CVC4 QUIET)
  if (${CVC4_FOUND})
    if (${GMP_FOUND})
      include_directories(${CVC4_INCLUDE_DIR})
      add_definitions(-DHAVE_CVC4)
      message("CVC4 SMT solver and GMP found. This enables optional SMT checking with CVC4.")
    else()
      message("CVC4 SMT solver found but its dependency GMP was NOT found. Optional SMT checking with CVC4 will not be available. Please install GMP if it is desired.")
      list(REMOVE_ITEM sources "${CMAKE_CURRENT_SOURCE_DIR}/formal/CVC4Interface.cpp")
    endif()
  else()
    message("No SMT solver found (Z3 or CVC4). Optional SMT checking will not be available. Please install Z3 or CVC4 if it is desired.")
    list(REMOVE_ITEM sources "${CMAKE_CURRENT_SOURCE_DIR}/formal/CVC4Interface.cpp")
  endif()
endif()

add_library(solidity ${sources} ${headers})
target_link_libraries(solidity PUBLIC evmasm devcore ${Boost_FILESYSTEM_LIBRARY} ${Boost_SYSTEM_LIBRARY})

if (${Z3_FOUND})
  target_link_libraries(solidity PUBLIC ${Z3_LIBRARY})
endif()

if (${CVC4_FOUND} AND ${GMP_FOUND})
  target_link_libraries(solidity PUBLIC ${CVC4_LIBRARY})
  target_link_libraries(solidity PUBLIC ${GMP_LIBRARY})
endif()

# Tutorial for embedding LLVM:
# https://llvm.org/docs/CMake.html#embedding-llvm-in-your-project

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
llvm_map_components_to_libnames(llvm_libs support core irreader)
target_link_libraries(solidity PUBLIC ${llvm_libs})


add_library(LLVM-6.0 SHARED IMPORTED) # or STATIC instead of SHARED
set_target_properties(LLVM-6.0 PROPERTIES
  IMPORTED_LOCATION "/usr/lib/llvm-6.0/lib/libLLVM-6.0.so"
  INTERFACE_INCLUDE_DIRECTORIES "/usr/lib/llvm-6.0/include"
)
add_library(clangBasic SHARED IMPORTED) # or STATIC instead of SHARED
set_target_properties(clangBasic PROPERTIES
  IMPORTED_LOCATION "/usr/lib/llvm-6.0/lib/libclangBasic.a"
  INTERFACE_INCLUDE_DIRECTORIES "/usr/lib/llvm-6.0/include"
)
target_link_libraries(solidity PUBLIC "LLVM-6.0;clangBasic")
