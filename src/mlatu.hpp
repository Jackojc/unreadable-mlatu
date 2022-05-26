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
	namespace detail {
		template <typename... Ts>
		inline std::ostream& print_impl(std::ostream& os, Ts&&... xs) {
			return ((os << std::forward<Ts>(xs)), ..., os);
		}

		template <typename... Ts>
		inline std::ostream& println_impl(std::ostream& os, Ts&&... xs) {
			return ((os << std::forward<Ts>(xs)), ..., (os << '\n'));
		}
	}

	template <typename T, typename... Ts>
	inline std::ostream& print(std::ostream& os, T&& x, Ts&&... xs) {
		return detail::print_impl(os, std::forward<T>(x), std::forward<Ts>(xs)...);
	}

	template <typename T, typename... Ts>
	inline std::ostream& println(std::ostream& os, T&& x, Ts&&... xs) {
		return detail::println_impl(os, std::forward<T>(x), std::forward<Ts>(xs)...);
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
		#define X(a, b) b,
			constexpr const char* LOG2STR[] = { LOG_LEVELS };
		#undef X

		constexpr const char* log2str(LogLevel x) {
			return LOG2STR[static_cast<size_t>(x)];
		}
	}

	inline std::ostream& operator<<(std::ostream& os, LogLevel x) {
		return print(os, detail::log2str(x));
	}

	#define MLATU_LOG(...) \
		do { [MLATU_VAR(fn_name) = __func__] (size_t MLATU_VAR(x), auto&&... MLATU_VAR(args)) { \
			MLATU_DEBUG_RUN(( \
				mlatu::print(std::cerr, MLATU_TRACE, MLATU_VAR(x), " ") \
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

		constexpr View(const char* begin_, size_t length):
			begin(begin_), end(begin_ + length) {}

		constexpr View(const char* ptr):
			begin(ptr), end(ptr + detail::length(ptr)) {}
	};

	constexpr size_t length(View sv) {
		return
			((sv.end - sv.begin) * (sv.end > sv.begin)) +
			((sv.begin - sv.end) * (sv.begin > sv.end));
	}

	constexpr bool cmp(View lhs, View rhs) {
		if (lhs.begin == rhs.begin and lhs.end == rhs.end)
			return true;

		if (length(lhs) != length(rhs))
			return false;

		for (size_t i = 0; i < length(lhs); i++) {
			if (*(lhs.begin + i) != *(rhs.begin + i))
				return false;
		}

		return true;
	}

	[[nodiscard]] constexpr bool empty(View sv) {
		return sv.begin == sv.end;
	}

	inline std::ostream& operator<<(std::ostream& os, View sv) {
		os.write(sv.begin, length(sv));
		return os;
	}
}

constexpr mlatu::View operator""_sv(const char* str, size_t n) {
	return { str, str + n };
}

namespace mlatu {
	[[nodiscard]] constexpr char chr(View sv) {
		return *sv.begin;
	}

	[[nodiscard]] constexpr View stretch(View lhs, View rhs) {
		return { lhs.begin, rhs.end };
	}

	[[nodiscard]] constexpr View next(View sv) {
		return { sv.begin + 1, sv.end };
	}

	[[nodiscard]] constexpr View peek(View sv) {
		return { sv.begin, sv.begin + 1 };
	}

	[[nodiscard]] constexpr View take(View& sv) {
		const char* ptr = sv.begin;
		sv = next(sv);
		return { ptr, sv.begin };
	}

	template <typename F>
	[[nodiscard]] constexpr View take_while(View& sv, F&& fn) {
		View out = sv;

		while (not empty(sv) and fn(peek(sv)))
			out = stretch(out, take(sv));

		return out;
	}
}

// Errors
namespace mlatu {
	#define ERROR_KINDS \
		X(UNREACHABLE, "unreachable code") \
		X(NO_FILE,     "no file specified") \
		X(READ_FILE,   "could not read file")

	#define X(a, b) a,
		enum class ErrorKind: size_t { ERROR_KINDS };
	#undef X

	namespace detail {
		#define X(a, b) b,
			constexpr const char* ERR2STR[] = { ERROR_KINDS };
		#undef X

		constexpr const char* err2str(ErrorKind x) {
			return ERR2STR[static_cast<size_t>(x)];
		}
	}

	inline std::ostream& operator<<(std::ostream& os, ErrorKind x) {
		return print(os, detail::err2str(x));
	}

	struct Report {
		View sv;
		ErrorKind kind;
	};

	template <typename... Ts>
	[[noreturn]] inline void report(View sv, ErrorKind x) {
		throw Report { sv, x };
	}

	inline std::ostream& report_handler(std::ostream& os, Report x) {
		return empty(x.sv) ?
			println(os, "error => ", x.kind):
			println(os, "error: `", x.sv, "` => ", x.kind);
	}

	inline std::ostream& operator<<(std::ostream& os, Report x) {
		return report_handler(os, x);
	}
}

namespace mlatu {
	constexpr bool is_visible(View sv) {
		char x = chr(sv);
		return x >= 33 and x <= 126;
	}

	constexpr bool is_control(View sv) {
		char x = chr(sv);
		return x >= 0 and x <= 31;
	}

	constexpr bool is_whitespace(View sv) {
		char x = chr(sv);
		return any(x >= 9 and x <= 13, x == ' ');
	}

	#define TOKEN_KINDS \
		X(NONE,       "none") \
		X(TERMINATOR, "eof") \
		\
		X(COPY,    "+") \
		X(DISCARD, "-") \
		X(WRAP,    ">") \
		X(UNWRAP,  "<") \
		X(COMBINE, ",") \
		X(SWAP,    "~") \
		\
		X(IDENT,  "ident") \
		X(ASSIGN, "=") \
		X(END,    ".") \
		X(LPAREN, "(") \
		X(RPAREN, ")") \

	#define X(a, b) a,
		enum class TokenKind: size_t { TOKEN_KINDS };
	#undef X

	namespace detail {
		#define X(a, b) b,
			constexpr const char* TOK2STR[] = { TOKEN_KINDS };
		#undef X

		constexpr const char* tok2str(TokenKind x) {
			return TOK2STR[static_cast<size_t>(x)];
		}
	}

	inline std::ostream& operator<<(std::ostream& os, TokenKind x) {
		return print(os, detail::tok2str(x));
	}

	struct Token {
		View sv;
		TokenKind kind;

		constexpr Token(View sv_, TokenKind kind_): sv(sv_), kind(kind_) {}
	};

	struct Lexer {
		View src;
		View sv;

		Token peek;
		Token prev;

		constexpr Lexer(View src_):
			src(src_), sv(src_),
			peek(mlatu::peek(src_), TokenKind::NONE),
			prev(mlatu::peek(src_), TokenKind::NONE) {}
	};

	constexpr Token peek(Lexer lx) {
		return lx.peek;
	}

	constexpr Token take(Lexer& lx) {
		View ws = take_while(lx.sv, is_whitespace);
		Token tok { take(lx.sv), TokenKind::NONE };

		if (empty(lx.sv)) tok.kind = TokenKind::TERMINATOR;

		else if (cmp(tok.sv, "+"_sv)) tok.kind = TokenKind::COPY;
		else if (cmp(tok.sv, "-"_sv)) tok.kind = TokenKind::DISCARD;
		else if (cmp(tok.sv, ">"_sv)) tok.kind = TokenKind::WRAP;
		else if (cmp(tok.sv, "<"_sv)) tok.kind = TokenKind::UNWRAP;
		else if (cmp(tok.sv, ","_sv)) tok.kind = TokenKind::COMBINE;
		else if (cmp(tok.sv, "~"_sv)) tok.kind = TokenKind::SWAP;
		else if (cmp(tok.sv, "("_sv)) tok.kind = TokenKind::LPAREN;
		else if (cmp(tok.sv, ")"_sv)) tok.kind = TokenKind::RPAREN;
		else if (cmp(tok.sv, "="_sv)) tok.kind = TokenKind::ASSIGN;
		else if (cmp(tok.sv, "."_sv)) tok.kind = TokenKind::END;

		else tok.kind = TokenKind::IDENT;

		return tok;
	}
}

#endif
