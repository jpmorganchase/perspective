cmake_minimum_required(VERSION 3.7.2)
project(psp)

set(CMAKE_BUILD_TYPE "Release")


set(CMAKE_CXX_STANDARD 14)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(NOT DEFINED PSP_CMAKE_MODULE_PATH)
	set(PSP_CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../../cmake/")
endif()
set(CMAKE_MODULE_PATH "${PSP_CMAKE_MODULE_PATH}/modules" ${CMAKE_MODULE_PATH})

set(PSP_ARROW_VERSION 0.16.0)
if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
	set(WIN32 ON)
    set(MACOS OFF)
    set(LINUX OFF)
elseif(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(WIN32 OFF)
    set(MACOS ON)
    set(LINUX OFF)
else()
    set(WIN32 OFF)
    set(MACOS OFF)
    set(LINUX ON)
endif()


## Helper function
function(string_starts_with str search)
  string(FIND "${str}" "${search}" out)
  if("${out}" EQUAL 0)
    return(true)
  endif()
  return(false)
endfunction()

set(BUILD_MESSAGE "")
function (psp_build_message message)
	set(BUILD_MESSAGE "${BUILD_MESSAGE}\n${message}")
endfunction()

###############################
# Helper to grab dependencies #
###############################
function (psp_build_dep name cmake_file)
	if(EXISTS ${CMAKE_BINARY_DIR}/${name}-build)
		psp_build_message("${Cyan}Dependency found - not rebuilding - ${CMAKE_BINARY_DIR}/${name}-build${ColorReset}")
	else()
		configure_file(${cmake_file} ${name}-download/CMakeLists.txt)

		execute_process(COMMAND ${CMAKE_COMMAND} -G "${CMAKE_GENERATOR}" .
			RESULT_VARIABLE result
			WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/${name}-download )

		if(result)
			message(FATAL_ERROR "CMake step for ${name} failed: ${result}")
		endif()

		execute_process(COMMAND ${CMAKE_COMMAND} --build .
			RESULT_VARIABLE result
			WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/${name}-download )

		if(result)
			message(FATAL_ERROR "Build step for ${name} failed: ${result}")
		endif()
	endif()

	if(${name} STREQUAL arrow)
		# Overwrite arrow's CMakeLists with our custom, minimal one
		configure_file(${PSP_CMAKE_MODULE_PATH}/arrow/CMakeLists.txt ${CMAKE_BINARY_DIR}/arrow-src/cpp/ COPYONLY)
		configure_file(${PSP_CMAKE_MODULE_PATH}/arrow/config.h ${CMAKE_BINARY_DIR}/arrow-src/cpp/src/arrow/util/ COPYONLY)
		add_subdirectory(${CMAKE_BINARY_DIR}/arrow-src/cpp/
			${CMAKE_BINARY_DIR}/arrow-build
			EXCLUDE_FROM_ALL)

		include_directories(${CMAKE_BINARY_DIR}/arrow-src/cpp/src/)
	else()
		add_subdirectory(${CMAKE_BINARY_DIR}/${name}-src
			${CMAKE_BINARY_DIR}/${name}-build
			EXCLUDE_FROM_ALL)

		include_directories(${CMAKE_BINARY_DIR}/${name}-src/extras/${name}/include)
		include_directories(${CMAKE_BINARY_DIR}/${name}-src/include)
		include_directories(${CMAKE_BINARY_DIR}/${name}-src)
	endif()
endfunction()
##############################


#######################
# BUILD CONFIGURATION #
#######################
find_package(Color)

option(CMAKE_BUILD_TYPE "Release/Debug build" RELEASE)
option(PSP_WASM_BUILD "Build the WebAssembly Project" ON)
option(PSP_CPP_BUILD "Build the C++ Project" OFF)
option(PSP_PYTHON_BUILD "Build the Python Bindings" OFF)
option(PSP_CPP_BUILD_STRICT "Build the C++ with strict warnings" OFF)
option(PSP_BUILD_DOCS "Build the Perspective documentation" OFF)

if (NOT DEFINED PSP_WASM_BUILD)
	set(PSP_WASM_BUILD ON)
	set(PSP_CPP_BUILD OFF)
	set(PSP_PYTHON_BUILD OFF)
endif()

if (PSP_WASM_BUILD AND PSP_CPP_BUILD)
	message(FATAL_ERROR "${Red}CPP and Emscripten builds must be done separately${ColorReset}")
endif()


if(DEFINED ENV{PSP_DEBUG})
	set(CMAKE_BUILD_TYPE DEBUG)
else()
	if (NOT DEFINED CMAKE_BUILD_TYPE)
		set(CMAKE_BUILD_TYPE RELEASE)
	endif()
endif()

if(DEFINED ENV{PSP_MANYLINUX})
	set(MANYLINUX ON)
else()
	set(MANYLINUX OFF)
endif()

if (NOT DEFINED PSP_CPP_BUILD)
	set(PSP_CPP_BUILD ON)
endif()

if (NOT DEFINED PSP_PYTHON_BUILD)
	set(PSP_PYTHON_BUILD OFF)
elseif(PSP_PYTHON_BUILD)
	# PSP_PYTHON_BUILD will always run PSP_CPP_BUILD
	set(PSP_CPP_BUILD ON)
	if(NOT DEFINED PSP_PYTHON_VERSION)
		set(PSP_PYTHON_VERSION 3.7)
	endif()
endif()

if (NOT DEFINED PSP_CPP_BUILD_STRICT)
	set(PSP_CPP_BUILD_STRICT OFF)
endif()


if(PSP_WASM_BUILD)
	set(BUILD_MESSAGE "${BUILD_MESSAGE}\n${Cyan}Building WASM binding${ColorReset}")
else()
	set(BUILD_MESSAGE "${BUILD_MESSAGE}\n${Yellow}Skipping WASM binding${ColorReset}")
endif()

if(NOT DEFINED PSP_CPP_SRC)
	set(PSP_CPP_SRC "${CMAKE_SOURCE_DIR}")
endif()
if(PSP_CPP_BUILD)
	set(BUILD_MESSAGE "${BUILD_MESSAGE}\n${Cyan}Building C++ binding${ColorReset}")
else()
	set(BUILD_MESSAGE "${BUILD_MESSAGE}\n${Yellow}Skipping C++ binding${ColorReset}")
endif()

if (PSP_PYTHON_BUILD)
	if(NOT DEFINED PSP_PYTHON_SRC)
		set(PSP_PYTHON_SRC "${CMAKE_SOURCE_DIR}/../../python/perspective/perspective")
	endif()
	set(BUILD_MESSAGE "${BUILD_MESSAGE}\n${Cyan}Building Python ${Red}${PSP_PYTHON_VERSION}${Cyan} binding${ColorReset}")
else()
	set(BUILD_MESSAGE "${BUILD_MESSAGE}\n${Yellow}Skipping Python binding${ColorReset}")
endif()

if (PSP_CPP_BUILD AND NOT PSP_CPP_BUILD_STRICT)
	set(BUILD_MESSAGE "${BUILD_MESSAGE}\n${Yellow}Building C++ without strict warnings${ColorReset}")
else()
	set(BUILD_MESSAGE "${BUILD_MESSAGE}\n${Cyan}Building C++ with strict warnings${ColorReset}")
endif()

string(TOLOWER "${CMAKE_BUILD_TYPE}" CMAKE_BUILD_TYPE_LOWER )
if(CMAKE_BUILD_TYPE_LOWER STREQUAL debug)
	set(BUILD_MESSAGE "${BUILD_MESSAGE}\n${Red}Building DEBUG${ColorReset}")
	add_definitions(-DPSP_DEBUG)
else()
	set(BUILD_MESSAGE "${BUILD_MESSAGE}\n${Cyan}Building RELEASE${ColorReset}")
endif()

if(PSP_BUILD_DOCS)
	set(BUILD_MESSAGE "${BUILD_MESSAGE}\n${Cyan}Building Perspective Documentation${ColorReset}")
else()
	set(BUILD_MESSAGE "${BUILD_MESSAGE}\n${Cyan}Skipping Perspective Documentation${ColorReset}")
endif()



#######################
include_directories("${CMAKE_SOURCE_DIR}/src/include")

if(NOT WIN32)
	set(CMAKE_CXX_FLAGS_RELEASE "-O3 -DNDEBUG")
	set(CMAKE_C_FLAGS_RELEASE "-O3 -DNDEBUG")
endif()

if (PSP_WASM_BUILD)
	####################
	# EMSCRIPTEN BUILD #
	####################
	execute_process(COMMAND which emcc OUTPUT_VARIABLE EMCC)
	execute_process(COMMAND which em++ OUTPUT_VARIABLE EMPP)
	string(STRIP ${EMCC} EMCC)
	string(STRIP ${EMPP} EMPP)
	set(CMAKE_C_COMPILER ${EMCC})
	set(CMAKE_CXX_COMPILER ${EMPP})
	set(CMAKE_TOOLCHAIN_FILE "$ENV{EMSCRIPTEN_ROOT}/cmake/Modules/Platform/Emscripten.cmake")
	set(CMAKE_AR emar)
	set(CMAKE_RANLIB emranlib)
	set(CMAKE_EXECUTABLE_SUFFIX ".js")
	list(APPEND CMAKE_PREFIX_PATH /usr/local)

	# Assumes that Boost includes will be in this folder.
	include_directories("/usr/local/include" SYSTEM)

    # Include this docker-only directory.
	include_directories("/boost_includes")

    # Build Rapidjson as it is used in the minimal arrow to be built later.
	psp_build_dep("rapidjson" "${PSP_CMAKE_MODULE_PATH}/rapidjson.txt.in")

	set(EXTENDED_FLAGS " \
		--bind \
		--source-map-base ./build/ \
		--memory-init-file 0 \
		-Wall \
		-s NO_EXIT_RUNTIME=1 \
		-s NO_FILESYSTEM=1 \
		-s ALLOW_MEMORY_GROWTH=1 \
		-s MODULARIZE=1 \
		-s EXPORT_NAME=\"load_perspective\" \
		-s EXPORT_ES6=1 \
		-s MAXIMUM_MEMORY=4gb \
		-s WASM_BIGINT=1 \
		-s USE_ES6_IMPORT_META=0 \
		-s EXPORTED_FUNCTIONS=\"['_main']\" \
		")

	if(CMAKE_BUILD_TYPE_LOWER STREQUAL debug)
		set(OPT_FLAGS " \
			-O0 \
			-g4 \
			--profiling \
			-s WARN_UNALIGNED=1 \
			-Wcast-align \
			-Wover-aligned \
			-s DISABLE_EXCEPTION_CATCHING=0 \
			-s ASSERTIONS=2 \
			-s DEMANGLE_SUPPORT=1 \
			")
	else()
		set(OPT_FLAGS " \
			-O3 \
			-g0 \
			-flto \
			--closure 1 \
			-s AGGRESSIVE_VARIABLE_ELIMINATION=1 \
			")
	endif()

	set(ASYNC_MODE_FLAGS "-s -s BINARYEN_ASYNC_COMPILATION=1 -s WASM=1")
elseif(PSP_CPP_BUILD OR PSP_PYTHON_BUILD)
	if(WIN32)
		if(CMAKE_BUILD_TYPE_LOWER STREQUAL debug)
			set(OPT_FLAGS " \
				/DEBUG \
				/Z7 \
				/Zi
				")
		else()
			set(OPT_FLAGS " \
				/NDEBUG \
				/O2 \
				")
		endif()
	else()
		if(CMAKE_BUILD_TYPE_LOWER STREQUAL debug)
			set(OPT_FLAGS " \
				-O0 \
				-g3 \
				")
		else()
			set(OPT_FLAGS " \
				-O3 \
				-g0 \
				")
		endif()
	endif()
	set(SYNC_MODE_FLAGS "")
	set(ASYNC_MODE_FLAGS "")
	set(ASMJS_MODE_FLAGS "")

	if (WIN32)
		# BOOST_ROOT has been removed on Windows VMs in Azure:
		#
		# - https://github.com/actions/virtual-environments/issues/687
		# - https://github.com/actions/virtual-environments/issues/319
		# 
		# `BOOST_ROOT` must be set in the environment, and policy CMP0074
		# must be set to `NEW` to allow BOOST_ROOT to be defined by env var
		cmake_policy(SET CMP0074 NEW)

		if(DEFINED ENV{BOOST_ROOT})
			set(Boost_NO_BOOST_CMAKE TRUE)

			psp_build_message("${Cyan}BOOST_ROOT: $ENV{BOOST_ROOT} ${ColorReset}")
			psp_build_message("${Cyan}BOOST_INCLUDEDIR: $ENV{BOOST_INCLUDEDIR} ${ColorReset}")
			psp_build_message("${Cyan}BOOST_LIBRARYDIR: $ENV{BOOST_LIBRARYDIR} ${ColorReset}")
		endif()
	endif()

	find_package(Boost COMPONENTS thread system REQUIRED)
	if(NOT Boost_FOUND)
		message(FATAL_ERROR "${Red}Boost could not be located${ColorReset}")
	else()
		psp_build_message("${Cyan}Found Boost: `Boost_INCLUDE_DIRS`: ${Boost_INCLUDE_DIRS}, `Boost_LIBRARY_DIRS` - ${Boost_LIBRARY_DIRS} ${ColorReset}")
		include_directories( ${Boost_INCLUDE_DIRS} )
	endif()

	find_package(TBB)
	if(NOT TBB_FOUND)
		psp_build_message("${Red}TBB could not be located - building TBB from external source ${ColorReset}")
		psp_build_dep("tbb" "${PSP_CMAKE_MODULE_PATH}/TBB.txt.in")
	else()
		psp_build_message("${Cyan}Found TBB: TBB_INCLUDE_DIRS - ${TBB_INCLUDE_DIRS}, TBB_LIBRARY_DIRS - ${TBB_LIBRARY_DIRS} ${ColorReset}")
		include_directories( ${TBB_INCLUDE_DIRS} )
	endif()

	if(WIN32)
		foreach(warning 4244 4251 4267 4275 4290 4786 4305 4996)
			SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd${warning}")
		endforeach(warning)
		# When linking against Boost on Azure, bcrypt fails to link:
		# - https://github.com/microsoft/vcpkg/issues/4481
		# - https://github.com/boostorg/uuid/issues/68
		#add_definitions("-DBOOST_UUID_FORCE_AUTO_LINK")
	else()
		include_directories("/usr/local/include")
	endif()

	if(PSP_PYTHON_BUILD)
		#########################
		# PYTHON BINDINGS BUILD #
		#########################
		include_directories("${PSP_PYTHON_SRC}/perspective/include")

		# Set CMP0094 to NEW - find the first version that matches constraints,
		# instead of the latest version installed.
		cmake_policy(SET CMP0094 NEW)

		if(MANYLINUX)
			# Manylinux docker images have no shared libraries
			# The instead use a statically built python.
			# Cmake's default FindPython can't find the python headers
			# without also finding (or failing to find) the python libraries
			# so we use a custom FindPythonHeaders that is the same as the
			# default, but ignores when the python libraries can't be found.
			psp_build_message("${Red}Manylinux build has no python shared libraries${ColorReset}")
			find_package(Python ${PSP_PYTHON_VERSION} EXACT REQUIRED COMPONENTS Interpreter)
			find_package(PythonHeaders ${PSP_PYTHON_VERSION} EXACT REQUIRED)

			# Run with exact version so its cached for pybind
			find_package(PythonInterp ${PSP_PYTHON_VERSION} EXACT REQUIRED)
		else()
			psp_build_message("${Cyan}Use python shared libraries${ColorReset}")
			find_package( Python ${PSP_PYTHON_VERSION} EXACT REQUIRED COMPONENTS Interpreter Development)

			# Run with exact version so its cached for pybind
			find_package(PythonInterp ${PSP_PYTHON_VERSION} EXACT REQUIRED)
			find_package(PythonLibs ${PSP_PYTHON_VERSION} EXACT REQUIRED)

			link_directories( ${Python_LIBRARY_DIRS} )
		endif()
		psp_build_message("${Cyan}Using Python ${Python_VERSION}, \nPython_INCLUDE_DIRS: ${Python_INCLUDE_DIRS}, \nPython_LIBRARIES: ${Python_LIBRARIES}, \nPython_EXECUTABLE: ${Python_EXECUTABLE} ${ColorReset}")
		include_directories( ${Python_INCLUDE_DIRS} )

		if(MACOS)
			# don't link against build python
			# https://blog.tim-smith.us/2015/09/python-extension-modules-os-x/
			set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -undefined dynamic_lookup")

			# on mac, use the vanilla pybind11 finder
			find_package(pybind11)
			if(pybind11_FOUND)
				# Found PyBind installed by Homebrew
				set(PYTHON_PYBIND_FOUND pybind11_FOUND)
			else()
				# Check if pip installed PyBind is available
				find_package(Pybind)
			endif()

		else()
			# On Linux/Docker, look for pip installed PyBind only
			find_package(Pybind)
		endif()

		if(NOT PYTHON_PYBIND_FOUND)
		    psp_build_message("${Red}PyBind11 could not be located - building from external source${ColorReset}")
			psp_build_dep("pybind11" "${PSP_CMAKE_MODULE_PATH}/Pybind.txt.in")
		else()
		    psp_build_message("${Cyan}Found PyBind11 in ${PYTHON_PYBIND_INCLUDE_DIR}${ColorReset}")
			include_directories( ${PYTHON_PYBIND_INCLUDE_DIR} )
		endif()

		find_package(NumPy)
		if(NOT PYTHON_NUMPY_FOUND)
			message(FATAL_ERROR "${Red}Numpy could not be located${ColorReset}")
		else()
			psp_build_message("${Cyan}Numpy found: ${PYTHON_NUMPY_INCLUDE_DIR}${ColorReset}")
			include_directories( ${PYTHON_NUMPY_INCLUDE_DIR})
		endif()

		find_package(PyArrow REQUIRED)

		if(NOT PYTHON_PYARROW_FOUND)
			message(FATAL_ERROR "${Red}PyArrow could not be located${ColorReset}")
		else()
			psp_build_message("${Cyan}PyArrow found: PYTHON_PYARROW_INCLUDE_DIR - ${PYTHON_PYARROW_INCLUDE_DIR}${ColorReset}")
			psp_build_message("${Cyan}Using pre-built ${PYTHON_PYARROW_PYTHON_SHARED_LIBRARY} ${PYTHON_PYARROW_ARROW_SHARED_LIBRARY} from: ${PYTHON_PYARROW_LIBRARY_DIR}${ColorReset}")
			include_directories(${PYTHON_PYARROW_INCLUDE_DIR})
			link_directories(${PYTHON_PYARROW_LIBRARY_DIR})
		endif()
		#####################
	endif()
endif()

# Build header-only dependencies from external source
psp_build_dep("date" "${PSP_CMAKE_MODULE_PATH}/date.txt.in")
psp_build_dep("hopscotch" "${PSP_CMAKE_MODULE_PATH}/hopscotch.txt.in")
psp_build_dep("ordered-map" "${PSP_CMAKE_MODULE_PATH}/ordered-map.txt.in")

# For WASM/CPP build, build minimal arrow from source
if (NOT PSP_PYTHON_BUILD)
	# build arrow + dependencies from source for Emscripten and C++
	message("${Cyan}Building minimal Apache Arrow${ColorReset}")

	psp_build_dep("double-conversion" "${PSP_CMAKE_MODULE_PATH}/double-conversion.txt.in")
	psp_build_dep("arrow" "${PSP_CMAKE_MODULE_PATH}/arrow.txt.in")

	find_package(Flatbuffers)
	if(NOT FLATBUFFERS_FOUND)
		message(FATAL_ERROR"${Red}Flatbuffers could not be located${ColorReset}")
	else()
		psp_build_message("${Cyan}Found Flatbuffers in ${FLATBUFFERS_INCLUDE_DIR}${ColorReset}")
		include_directories( ${FLATBUFFERS_INCLUDE_DIR} )
	endif()
endif()

#####################

set(CMAKE_C_FLAGS " \
	${CMAKE_C_FLAGS} \
	${EXTENDED_FLAGS} \
	${OPT_FLAGS} \
	")

if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c++1y")
endif()

set (SOURCE_FILES
	${PSP_CPP_SRC}/src/cpp/aggregate.cpp
	${PSP_CPP_SRC}/src/cpp/aggspec.cpp
	${PSP_CPP_SRC}/src/cpp/arg_sort.cpp
	${PSP_CPP_SRC}/src/cpp/arrow_loader.cpp
	${PSP_CPP_SRC}/src/cpp/arrow_writer.cpp
	${PSP_CPP_SRC}/src/cpp/base.cpp
	${PSP_CPP_SRC}/src/cpp/base_impl_linux.cpp
	${PSP_CPP_SRC}/src/cpp/base_impl_osx.cpp
	${PSP_CPP_SRC}/src/cpp/base_impl_wasm.cpp
	${PSP_CPP_SRC}/src/cpp/base_impl_win.cpp
	${PSP_CPP_SRC}/src/cpp/binding.cpp
	${PSP_CPP_SRC}/src/cpp/build_filter.cpp
	#${PSP_CPP_SRC}/src/cpp/calc_agg_dtype.cpp
	${PSP_CPP_SRC}/src/cpp/column.cpp
	${PSP_CPP_SRC}/src/cpp/comparators.cpp
	${PSP_CPP_SRC}/src/cpp/compat.cpp
	${PSP_CPP_SRC}/src/cpp/compat_impl_linux.cpp
	${PSP_CPP_SRC}/src/cpp/compat_impl_osx.cpp
	${PSP_CPP_SRC}/src/cpp/compat_impl_win.cpp
	${PSP_CPP_SRC}/src/cpp/computed.cpp
	${PSP_CPP_SRC}/src/cpp/computed_column_map.cpp
	${PSP_CPP_SRC}/src/cpp/computed_function.cpp
	${PSP_CPP_SRC}/src/cpp/config.cpp
	${PSP_CPP_SRC}/src/cpp/context_base.cpp
	${PSP_CPP_SRC}/src/cpp/context_grouped_pkey.cpp
	${PSP_CPP_SRC}/src/cpp/context_handle.cpp
	${PSP_CPP_SRC}/src/cpp/context_one.cpp
	${PSP_CPP_SRC}/src/cpp/context_two.cpp
	${PSP_CPP_SRC}/src/cpp/context_zero.cpp
	${PSP_CPP_SRC}/src/cpp/context_unit.cpp
	${PSP_CPP_SRC}/src/cpp/custom_column.cpp
	${PSP_CPP_SRC}/src/cpp/data.cpp
	${PSP_CPP_SRC}/src/cpp/data_slice.cpp
	${PSP_CPP_SRC}/src/cpp/data_table.cpp
	${PSP_CPP_SRC}/src/cpp/date.cpp
	${PSP_CPP_SRC}/src/cpp/dense_nodes.cpp
	${PSP_CPP_SRC}/src/cpp/dense_tree_context.cpp
	${PSP_CPP_SRC}/src/cpp/dense_tree.cpp
	${PSP_CPP_SRC}/src/cpp/dependency.cpp
	${PSP_CPP_SRC}/src/cpp/extract_aggregate.cpp
	${PSP_CPP_SRC}/src/cpp/filter.cpp
	${PSP_CPP_SRC}/src/cpp/flat_traversal.cpp
	${PSP_CPP_SRC}/src/cpp/get_data_extents.cpp
	${PSP_CPP_SRC}/src/cpp/gnode.cpp
	${PSP_CPP_SRC}/src/cpp/gnode_state.cpp
	${PSP_CPP_SRC}/src/cpp/histogram.cpp
	${PSP_CPP_SRC}/src/cpp/logtime.cpp
	${PSP_CPP_SRC}/src/cpp/mask.cpp
	${PSP_CPP_SRC}/src/cpp/min_max.cpp
	${PSP_CPP_SRC}/src/cpp/multi_sort.cpp
	${PSP_CPP_SRC}/src/cpp/none.cpp
	${PSP_CPP_SRC}/src/cpp/path.cpp
	${PSP_CPP_SRC}/src/cpp/pivot.cpp
	${PSP_CPP_SRC}/src/cpp/pool.cpp
	${PSP_CPP_SRC}/src/cpp/port.cpp
	${PSP_CPP_SRC}/src/cpp/process_state.cpp
	${PSP_CPP_SRC}/src/cpp/pyutils.cpp
	${PSP_CPP_SRC}/src/cpp/raii.cpp
	${PSP_CPP_SRC}/src/cpp/raii_impl_linux.cpp
	${PSP_CPP_SRC}/src/cpp/raii_impl_osx.cpp
	${PSP_CPP_SRC}/src/cpp/raii_impl_win.cpp
	${PSP_CPP_SRC}/src/cpp/range.cpp
	${PSP_CPP_SRC}/src/cpp/rlookup.cpp
	${PSP_CPP_SRC}/src/cpp/scalar.cpp
	${PSP_CPP_SRC}/src/cpp/schema_column.cpp
	${PSP_CPP_SRC}/src/cpp/schema.cpp
	${PSP_CPP_SRC}/src/cpp/slice.cpp
	${PSP_CPP_SRC}/src/cpp/sort_specification.cpp
	${PSP_CPP_SRC}/src/cpp/sparse_tree.cpp
	${PSP_CPP_SRC}/src/cpp/sparse_tree_node.cpp
	${PSP_CPP_SRC}/src/cpp/step_delta.cpp
	${PSP_CPP_SRC}/src/cpp/storage.cpp
	${PSP_CPP_SRC}/src/cpp/storage_impl_linux.cpp
	${PSP_CPP_SRC}/src/cpp/storage_impl_osx.cpp
	${PSP_CPP_SRC}/src/cpp/storage_impl_win.cpp
	${PSP_CPP_SRC}/src/cpp/sym_table.cpp
	${PSP_CPP_SRC}/src/cpp/table.cpp
	${PSP_CPP_SRC}/src/cpp/time.cpp
	${PSP_CPP_SRC}/src/cpp/traversal.cpp
	${PSP_CPP_SRC}/src/cpp/traversal_nodes.cpp
	${PSP_CPP_SRC}/src/cpp/tree_context_common.cpp
	${PSP_CPP_SRC}/src/cpp/utils.cpp
	${PSP_CPP_SRC}/src/cpp/update_task.cpp
	${PSP_CPP_SRC}/src/cpp/view.cpp
	${PSP_CPP_SRC}/src/cpp/view_config.cpp
	${PSP_CPP_SRC}/src/cpp/vocab.cpp
	)

set(PYTHON_SOURCE_FILES ${SOURCE_FILES}
	${PSP_PYTHON_SRC}/src/column.cpp
)

set(WASM_SOURCE_FILES ${SOURCE_FILES}
	${PSP_CPP_SRC}/src/cpp/arrow_csv.cpp
	${PSP_CPP_SRC}/src/cpp/vendor/arrow_single_threaded_reader.cpp
)


set (PYTHON_BINDING_SOURCE_FILES
	${PSP_PYTHON_SRC}/src/accessor.cpp
	${PSP_PYTHON_SRC}/src/computed.cpp
	${PSP_PYTHON_SRC}/src/context.cpp
	${PSP_PYTHON_SRC}/src/fill.cpp
	${PSP_PYTHON_SRC}/src/numpy.cpp
	${PSP_PYTHON_SRC}/src/python.cpp
	${PSP_PYTHON_SRC}/src/serialization.cpp
	${PSP_PYTHON_SRC}/src/table.cpp
	${PSP_PYTHON_SRC}/src/utils.cpp
	${PSP_PYTHON_SRC}/src/view.cpp
)

if (WIN32)
	set(CMAKE_CXX_FLAGS " /EHsc /MP")
else()
	set(CMAKE_CXX_FLAGS " ${CMAKE_CXX_FLAGS} ${CMAKE_C_FLAGS}")
endif()

message("${BUILD_MESSAGE}\n")

if (PSP_WASM_BUILD)
	add_library(psp ${WASM_SOURCE_FILES})
	target_compile_definitions(psp PRIVATE PSP_ENABLE_WASM=1)
	set_target_properties(psp PROPERTIES COMPILE_FLAGS "${ASYNC_MODE_FLAGS}")
	target_link_libraries(psp arrow)

	add_executable(perspective.async src/cpp/emscripten.cpp)
	target_link_libraries(perspective.async psp "${ASYNC_MODE_FLAGS}")
	target_compile_definitions(perspective.async PRIVATE PSP_ENABLE_WASM=1)
	set_target_properties(perspective.async PROPERTIES COMPILE_FLAGS "${ASYNC_MODE_FLAGS}")
	set_target_properties(perspective.async PROPERTIES RUNTIME_OUTPUT_DIRECTORY "./build/")
	set_target_properties(perspective.async PROPERTIES OUTPUT_NAME "psp.async")
elseif(PSP_CPP_BUILD OR PSP_PYTHON_BUILD)
    if(NOT WIN32)
		set(CMAKE_SHARED_LIBRARY_SUFFIX .so)
		# Look for the binary using @loader_path (relative to binary location) instead of @rpath
		# and include arrow in @rpath so it can be found by libbinding/libpsp
        set(CMAKE_MACOSX_RPATH TRUE)
        set(CMAKE_SKIP_BUILD_RPATH FALSE)
		set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)
        set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
        set(CMAKE_INSTALL_NAME_DIR "@rpath/")

		# module_origin_path is the location of the binary
        if(MACOS)
            set(module_origin_path "@loader_path/")
	    else()
            set(module_origin_path "\$ORIGIN")
		endif()
	else()
		set(CMAKE_SHARED_LIBRARY_PREFIX lib)
    endif()

	if(PSP_PYTHON_BUILD)
		########################
		# Python extra targets #
		########################
		add_library(psp SHARED ${PYTHON_SOURCE_FILES})
		add_library(binding SHARED ${PYTHON_BINDING_SOURCE_FILES})

		include_directories(${PSP_PYTHON_SRC}/include)

		target_compile_definitions(psp PRIVATE PSP_ENABLE_PYTHON=1)
		target_compile_definitions(binding PRIVATE PSP_ENABLE_PYTHON=1)

        if (WIN32)
			target_compile_definitions(binding PRIVATE WIN32=1)
			target_compile_definitions(binding PRIVATE _WIN32=1)

			# .dll not importable
			set_property(TARGET binding PROPERTY SUFFIX .pyd)
        elseif (MACOS OR NOT MANYLINUX)
			target_compile_options(binding PRIVATE -Wdeprecated-declarations)

			# Add a relative path to search for PyArrow - when Perspective is
			# installed from a wheel, PyArrow may not be in the same directory
			# as the PyArrow which was used to build the wheel.
			#
			# Assuming that both Pyarrow and Perspective are installed in
			# `site-packages`, the relative search path should be able to pick
            # up pyarrow. This is only enabled for *nix (non-manylinux), as `auditwheel`
			# will not delocate libarrow properly if it is in the rpath.
            set_property(TARGET psp PROPERTY INSTALL_RPATH ${CMAKE_INSTALL_RPATH} ${module_origin_path} ${module_origin_path}../../pyarrow/ ${PYTHON_PYARROW_LIBRARY_DIR} ${PSP_PYTHON_ARROWINSTALLDIR})
            set_property(TARGET binding PROPERTY INSTALL_RPATH ${CMAKE_INSTALL_RPATH} ${module_origin_path} ${module_origin_path}../../pyarrow/ ${PYTHON_PYARROW_LIBRARY_DIR} ${PSP_PYTHON_ARROWINSTALLDIR})
		else()
			target_compile_options(binding PRIVATE -Wdeprecated-declarations)
		endif()

		target_link_libraries(psp ${PYTHON_PYARROW_LIBRARIES})
		target_link_libraries(binding ${PYTHON_PYARROW_LIBRARIES})

		target_link_libraries(psp Boost::thread)
		target_link_libraries(binding Boost::thread)

		target_link_libraries(psp tbb)
		target_link_libraries(binding tbb)		

		target_link_libraries(binding psp)

		# The compiled libraries will be put in CMAKE_LIBRARY_OUTPUT_DIRECTORY by default. In the
		# setup.py file, we designate this to be in the build/lib.<platform> directory. However,
		# since we want to be able to test perspective in-source, we also copy the libraries into
		# the source folder. These two commands do that.
		add_custom_command(TARGET psp POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:psp> ${PSP_PYTHON_SRC}/table/)
		add_custom_command(TARGET binding POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:binding> ${PSP_PYTHON_SRC}/table/)

		if(WIN32)
			# inline arrow dlls
			file(GLOB ARROW_DLLS "${PYTHON_PYARROW_LIBRARY_DIR}/*.dll")

			add_custom_command(TARGET binding POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy ${ARROW_DLLS} ${PSP_PYTHON_SRC}/table/)
			if(NOT TBB_FOUND)
				add_custom_command(TARGET binding POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:tbb> ${PSP_PYTHON_SRC}/table/)
			endif()
		endif()
		########################
	else()
		add_library(psp SHARED ${WASM_SOURCE_FILES})

		# Link perspective against custom-built minimal arrow
		target_link_libraries(psp arrow)
	endif()

	if(PSP_CPP_BUILD_STRICT AND NOT WIN32)
		target_compile_options(psp PRIVATE -Wall -Werror)
		target_compile_options(psp PRIVATE $<$<CONFIG:DEBUG>:-fPIC -O0>)
		if(PSP_PYTHON_BUILD)
			target_compile_options(binding PRIVATE $<$<CONFIG:DEBUG>:-fPIC -O0>)
		endif()
	elseif(WIN32)
		target_compile_definitions(psp PRIVATE PERSPECTIVE_EXPORTS=1)
		target_compile_definitions(psp PRIVATE WIN32=1)
		target_compile_definitions(psp PRIVATE _WIN32=1)
	endif()
endif()

########
# Docs #
########
if(PSP_BUILD_DOCS)
	add_custom_target(doxygen)
	add_custom_command(TARGET doxygen
	    COMMAND doxygen doxygen.conf
	    WORKING_DIRECTORY ../../docs
	    COMMENT "Build doxygen xml files used by sphinx/breathe."
	)

	add_custom_target(docs-html ALL)
	add_custom_command(TARGET docs-html
		COMMAND sphinx-build -b html . build/html
			WORKING_DIRECTORY ../../docs
			COMMENT "Build html documentation"
		)
	add_dependencies(docs-html doxygen)

	# add_custom_target(docs-markdown ALL)
	# add_custom_command(TARGET docs-html
	# 	COMMAND sphinx-build -M markdown . build/
	# 		WORKING_DIRECTORY ../../docs
	# 		COMMENT "Build markdown documentation"
	# 	)
	# add_dependencies(docs-html doxygen)

endif()
##########
