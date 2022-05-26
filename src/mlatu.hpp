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
	#define MLATU_STR_IMPL_(x) #x
	#define MLATU_STR(x) MLATU_STR_IMPL_(x)

	#define MLATU_CAT_IMPL_(x, y) x##y
	#define MLATU_CAT(x, y) MLATU_CAT_IMPL_(x, y)

	#define MLATU_VAR(x) MLATU_CAT(var_, MLATU_CAT(x, MLATU_CAT(__LINE__, _)))

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

	#define LOG_LEVELS \
		X(INF, "[-]") \
		X(WRN, "[*]") \
		X(ERR, "[!]") \
		X(OK,  "[^]")

	#define X(a, b) a,
		enum class LogLevel: size_t { LOG_LEVELS };
	#undef X

	namespace detail {
		#define X(a, b) #b,
			constexpr const char* LOG2STR[] = { LOG_LEVELS };
		#undef X

		constexpr const char* log2str(LogLevel x) {
			return LOG2STR[static_cast<size_t>(x)];
		}
	}

	#define MLATU_LOG(...) \
		do { [MLATU_VAR(fn_name) = __func__] (size_t MLATU_VAR(x), auto&&... MLATU_VAR(args)) { \
			MLATU_DEBUG_RUN(( \
				mlatu::print(std::cerr, MLATU_TRACE, mlatu::detail::log2str(MLATU_VAR(x)), " ") \
			)); \
			MLATU_DEBUG_RUN(( mlatu::print(std::cerr, "`", MLATU_VAR(fn_name), "`") )); \
			if constexpr(sizeof...(MLATU_VAR(args)) > 0) { MLATU_DEBUG_RUN( \
				(mlatu::print(std::cerr, std::forward<decltype(MLATU_VAR(args))>(MLATU_VAR(args))...)) \
			); } \
			MLATU_DEBUG_RUN(( mlatu::println(std::cerr, MLATU_RESET) )); \
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

	constexpr auto fnv1a_64(const char* begin, const char* const end) {
		T hash = 14'695'981'039'346'656'037u;

		while (begin != end) {
			hash = (hash ^ static_cast<T>(*begin)) * 1'099'511'628'211u;
			begin++;
		}

		return hash;
	}

	namespace detail {
		constexpr size_t length(const char* str) {
			const char *ptr = str;
			while (*ptr) ++ptr;
			return ptr - str;
		}
	}
}

// String view
namespace mlatu {
	struct View {
		const char* begin = nullptr;
		const char* end   = nullptr;

		constexpr View() {}

		constexpr View(const char* begin_, const char* end_):
			begin(begin_), end(end_) {}

		constexpr View(const char* ptr):
			begin(ptr), end(ptr + detail::length(ptr)) {}
	};

	constexpr size_t length(View sv) const {
		return
			((sv.end - sv.begin) * (sv.end > sv.begin)) +
			((sv.begin - sv.end) * (sv.begin > sv.end));
	}

	[[nodiscard]] constexpr bool empty(View sv) {
		return sv.begin == sv.end;
	}

	inline std::ostream& operator<<(std::ostream& os, View sv) {
		os.write(sv.begin, sv.size());
		return os;
	}
}

constexpr mlatu::View operator""_sv(const char* str, size_t n) {
	return { str, str + n };
}

namespace mlatu {
	constexpr char chr(View sv) {
		return *sv.begin;
	}

	constexpr View stretch(View lhs, View rhs) {
		return { lhs.begin, rhs.end };
	}

	constexpr View next(View sv) {
		return { ++sv.begin, sv.end };
	}

	constexpr View peek(View sv) {
		return { sv.begin, next(sv.begin) };
	}

	constexpr View take(View& sv) {
		const char* ptr = sv.begin;
		sv = next(sv);
		return { ptr, sv.begin };
	}

	template <typename F>
	constexpr View take_while(View& sv, F&& fn) {
		View out = sv;

		while (not empty(sv) and fn(peek(sv)))
			out = stretch(out, take(sv));

		return out;
	}
}

// Errors
namespace mlatu {
	#define ERROR_KINDS \
		X(INF, "[-]") \
		X(WRN, "[*]") \
		X(ERR, "[!]") \
		X(OK,  "[^]")

	#define X(a, b) a,
		enum class Error: size_t { ERROR_KINDS };
	#undef X

	namespace detail {
		#define X(a, b) #b,
			constexpr const char* ERR2STR[] = { ERROR_KINDS };
		#undef X

		constexpr const char* err2str(Error x) {
			return ERR2STR[static_cast<size_t>(x)];
		}
	}

	struct Report {
		Error kind;
	};

	template <typename... Ts>
	[[noreturn]] inline void report(Error x) {
		throw Report { x };
	}

	inline std::ostream& report_handler(std::ostream& os, Report x) {
		println(os, "error: ", detail::err2str(x));
	}

	inline std::ostream& operator<<(std::ostream& os, Report x) {
		return report_handler(os, x);
	}
}

#endif
