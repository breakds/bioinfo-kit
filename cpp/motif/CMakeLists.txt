CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

PROJECT(Motifs)

MESSAGE(STATUS "Project Name:           ${PROJECT_NAME}")
MESSAGE(STATUS "Project Source Dir:     ${PROJECT_SOURCE_DIR}")
MESSAGE(STATUS "Kernel:                 ${CMAKE_SYSTEM}")
EXECUTE_PROCESS(COMMAND hostname OUTPUT_VARIABLE HOSTNAME)
MESSAGE(STATUS "Hostname:               ${HOSTNAME}")
MESSAGE(STATUS "Build Type:             ${CMAKE_BUILD_TYPE}")
MESSAGE(STATUS "GCC:                    ${CMAKE_CXX_COMPILER}")

include_directories("~/pf/projects" ".")

# option (BUILD_UNITTEST "build executables in purpose of unittest." ON)

SET(CMAKE_CXX_FLAGS "${OPENCV_FLAGS} --std=c++0x")
SET(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS} -DNDEBUG -O3")
SET(CMAKE_CXX_FLAGS_DEBUG "-O0 -g")
SET(CMAKE_CXX_FLAGS_GPROF "-O1 -pg")

ADD_EXECUTABLE(greedy greedy_motif_search.cc)
ADD_EXECUTABLE(random random_motif_search.cc)
ADD_EXECUTABLE(gibbs gibbs_motif_search.cc)

