#ifndef MLATU_HPP
#define MLATU_HPP

/*
	Headers are not included here for compilation
	speed reasons. Better to name all of your
	includes in the source file rather than force
	the preprocessor to track down files and
	potentially include them multiple times.

	#include <iostream>
	#include <utility>
	#include <cstddef>
	#include <cstdint>
*/

// Macros
namespace mlatu {
	// Utility macros.
	// Stringify a macro def.
	// i.e. MLATU_STR(__LINE__) => "42" as opposed to "__LINE__"
	#define MLATU_STR_IMPL_(x) #x
	#define MLATU_STR(x) MLATU_STR_IMPL_(x)

	// Concatenate macro defs.
	// i.e. MLATU_CAT(__FILE__, __LINE__) => "foo.c10"
	// as opposed to "__FILE____LINE__"
	#define MLATU_CAT_IMPL_(x, y) x##y
	#define MLATU_CAT(x, y) MLATU_CAT_IMPL_(x, y)

	// Create a uniquely named variable for use in a macro.
	#define MLATU_VAR(x) MLATU_CAT(var_, MLATU_CAT(x, MLATU_CAT(__LINE__, _)))

	// Evaluate expressions at beginning and ending of a scope.
	#define MLATU_SCOPE(open, close) \
		for ( \
			size_t MLATU_VAR(i) = ((open), 0); \
			!MLATU_VAR(i); \
			(MLATU_VAR(i)++), (close) \
		)

	// Evaluate expression at end of scope.
	#define MLATU_DEFER(close) \
		for ( \
			size_t MLATU_VAR(i) = 0; \
			!MLATU_VAR(i); \
			(MLATU_VAR(i)++), (close) \
		)

	// Print filename and line number `[foo.cpp:12]`
	#define MLATU_TRACE "[" __FILE__ ":" MLATU_STR(__LINE__) "] "
}

// I/O
namespace mlatu {
	template <typename T, typename... Ts>
	inline std::ostream& print(std::ostream& os, T&& arg, Ts&&... args) {
		return ((os << std::forward<Ts>(args)), ...);
	}

	template <typename T, typename... Ts>
	inline std::ostream& println(std::ostream& os, T&& arg, Ts&&... args) {
		return ((os << std::forward<Ts>(args)), ..., (os << '\n'));
	}
}

// Logging
namespace mlatu {
	#define MLATU_RESET "\x1b[0m"
	#define MLATU_BOLD  "\x1b[1m"

	#define MLATU_BLACK   "\x1b[30m"
	#define MLATU_RED     "\x1b[31m"
	#define MLATU_GREEN   "\x1b[32m"
	#define MLATU_YELLOW  "\x1b[33m"
	#define MLATU_BLUE    "\x1b[34m"
	#define MLATU_MAGENTA "\x1b[35m"
	#define MLATU_CYAN    "\x1b[36m"
	#define MLATU_WHITE   "\x1b[37m"

	#define MLATU_INF RESET "[-]"
	#define MLATU_WRN BLUE  "[*]"
	#define MLATU_ERR RED   "[!]"
	#define MLATU_OK  GREEN "[^]"

	#ifndef MLATU_DISABLE_ASSERT
		namespace detail {
			template <typename T> inline decltype(auto) dbg_impl(
				const char* file,
				const char* line,
				const char* expr_s,
				T&& expr
			) {
				println(std::cerr,
					"[", file, ":", line, "] ", expr_s, " = ", std::forward<T>(expr)
				);

				return std::forward<T>(expr);
			}
		}

		#define MLATU_DBG(expr) \
			(mlatu::detail::dbg_impl( \
				__FILE__, MLATU_STR(__LINE__), MLATU_STR(expr), (expr) \
			))

		#define MLATU_DBG_RUN(expr) \
			do { ((expr)); } while (0)
	#else
		#define MLATU_DBG(expr) ((expr))
		#define MLATU_DBG_RUN(expr) do {} while (0)
	#endif


	#define MLATU_LOG(...) \
		do { [MLATU_VAR(fn_name) = __func__] (size_t MLATU_VAR(lvl), auto&&... MLATU_VAR(args)) { \
			MLATU_DEBUG_RUN(( mlatu::err(MLATU_TRACE, mlatu::detail::lvl_to_style(MLATU_VAR(lvl)), " ") )); \
			MLATU_DEBUG_RUN(( mlatu::err("`", MLATU_VAR(fn_name), "`") )); \
			\
			if constexpr(sizeof...(MLATU_VAR(args)) > 0) { \
				MLATU_DEBUG_RUN( (mlatu::err(" => ")) ); \
				MLATU_DEBUG_RUN( (mlatu::errfmt(std::forward<decltype(MLATU_VAR(args))>(MLATU_VAR(args))...)) ); \
			} \
			MLATU_DEBUG_RUN(( mlatu::errln( MLATU_ANSI_RESET ) )); \
		} ( __VA_ARGS__ ); } while (0)
}

// Utilities
namespace mlatu {
	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool any(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) or std::forward<Ts>(rest)) or ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool all(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) and std::forward<Ts>(rest)) and ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool none(T&& first, Ts&&... rest) {
		return ((not std::forward<T>(first) and not std::forward<Ts>(rest)) and ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool cmp_all(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) == std::forward<Ts>(rest)) and ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool cmp_any(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) == std::forward<Ts>(rest)) or ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool cmp_none(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) != std::forward<Ts>(rest)) and ...);
	}
}

#endif
