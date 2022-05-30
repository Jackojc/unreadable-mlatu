#ifndef MLATU_HPP
#define MLATU_HPP

/*
	Headers are not included here for compilation
	speed reasons. Better to name all of your
	includes in the source file rather than force
	the preprocessor to track down files and
	potentially include them multiple times.

	#include <utility>
	#include <algorithm>
	#include <vector>
	#include <iostream>
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

	#ifndef NDEBUG
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
		X(INF, MLATU_RESET "[-]") \
		X(WRN, MLATU_BLUE  "[*]") \
		X(ERR, MLATU_RED   "[!]") \
		X(OK,  MLATU_GREEN "[^]")

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
		do { [MLATU_VAR(fn_name) = __func__] (LogLevel MLATU_VAR(x), auto&&... MLATU_VAR(args)) { \
			MLATU_DBG_RUN(( \
				mlatu::print(std::cerr, MLATU_VAR(x), " ", MLATU_TRACE) \
			)); \
			MLATU_DBG_RUN(( mlatu::print(std::cerr, "`", MLATU_VAR(fn_name), "`" MLATU_RESET) )); \
			if constexpr(sizeof...(MLATU_VAR(args)) > 0) { MLATU_DBG_RUN( \
				(mlatu::print(std::cerr, " ", std::forward<decltype(MLATU_VAR(args))>(MLATU_VAR(args))...)) \
			); } \
			MLATU_DBG_RUN(( mlatu::print(std::cerr, '\n') )); \
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
		X(UNREACHABLE,      "unreachable code") \
		X(NOT_IMPLEMENTED,  "not implemented") \
		X(NO_FILE,          "no file specified") \
		X(READ_FILE,        "could not read file") \
		X(UNKNOWN_CHAR,     "unknown character") \
		X(EXPECT_ASSIGN,    "expected `=`") \
		X(EXPECT_END,       "expected `.`") \
		X(EXPECT_TERM,      "expected term") \
		X(EXPECT_RPAREN,    "expected `)`") \
		X(EXPECT_STATEMENT, "expected `?` or `=`") \
		X(QUOTED_TERM,      "left side of rule contains quoted term") \
		X(EXPECT_NQ_TERM,   "expected non-quote term")

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
	[[noreturn]] constexpr void report(View sv, ErrorKind x) {
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

	constexpr bool is_primitive(View sv) {
		return cmp_any(chr(sv), '+', '-', '<', '>', ',', '~');
	}

	constexpr bool is_statement(View sv) {
		return cmp_any(chr(sv), '(', ')', '?', '=', '.');
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
		X(QUERY,  "?") \
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

		constexpr Token(View sv_, TokenKind kind_):
			sv(sv_), kind(kind_) {}
	};

	constexpr bool operator==(Token lhs, Token rhs) {
		return lhs.kind == rhs.kind and cmp(lhs.sv, rhs.sv);
	}

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

	[[nodiscard]] inline Token take(Lexer& lx) {
		View ws = take_while(lx.sv, is_whitespace);
		Token tok { peek(lx.sv), TokenKind::NONE };

		if (empty(lx.sv) or cmp(tok.sv, "\0"_sv))
			tok.kind = TokenKind::TERMINATOR;

		else if (cmp(tok.sv, "#"_sv)) {
			View comment = take_while(lx.sv, [] (View sv) {
				return not cmp(sv, "\n"_sv);
			});

			return take(lx);
		}

		else if (cmp(tok.sv, "+"_sv)) { tok.kind = TokenKind::COPY;    lx.sv = next(lx.sv); }
		else if (cmp(tok.sv, "-"_sv)) { tok.kind = TokenKind::DISCARD; lx.sv = next(lx.sv); }
		else if (cmp(tok.sv, ">"_sv)) { tok.kind = TokenKind::WRAP;    lx.sv = next(lx.sv); }
		else if (cmp(tok.sv, "<"_sv)) { tok.kind = TokenKind::UNWRAP;  lx.sv = next(lx.sv); }
		else if (cmp(tok.sv, ","_sv)) { tok.kind = TokenKind::COMBINE; lx.sv = next(lx.sv); }
		else if (cmp(tok.sv, "~"_sv)) { tok.kind = TokenKind::SWAP;    lx.sv = next(lx.sv); }

		else if (cmp(tok.sv, "("_sv)) { tok.kind = TokenKind::LPAREN;  lx.sv = next(lx.sv); }
		else if (cmp(tok.sv, ")"_sv)) { tok.kind = TokenKind::RPAREN;  lx.sv = next(lx.sv); }
		else if (cmp(tok.sv, "?"_sv)) { tok.kind = TokenKind::QUERY;   lx.sv = next(lx.sv); }
		else if (cmp(tok.sv, "="_sv)) { tok.kind = TokenKind::ASSIGN;  lx.sv = next(lx.sv); }
		else if (cmp(tok.sv, "."_sv)) { tok.kind = TokenKind::END;     lx.sv = next(lx.sv); }

		else if (is_visible(tok.sv)) {
			tok.kind = TokenKind::IDENT;
			tok.sv = take_while(lx.sv, [] (View sv) {
				return
					is_visible(sv) and
					none(is_primitive(sv), is_statement(sv));
			});
		}

		else
			report(tok.sv, ErrorKind::UNKNOWN_CHAR);

		Token out = lx.peek;

		lx.prev = lx.peek;
		lx.peek = tok;

		return out;
	}

	constexpr decltype(auto) is(TokenKind kind) {
		return [=] (Token other) { return kind == other.kind; };
	}

	template <typename F> constexpr void expect(Lexer& lx, F&& fn, ErrorKind x) {
		if (not fn(lx.peek))
			report(lx.peek.sv, x);
	}
}

namespace mlatu {
	using Terms = std::vector<Token>;

	inline std::ostream& operator<<(std::ostream& os, const Terms& x) {
		// return print(os, detail::tok2str(x));
		print(os, '[', x.front().sv);

		for (auto it = x.begin() + 1; it != x.end(); ++it)
			print(os, " ", it->sv);

		return print(os, ']');
	}

	struct Context {
		std::vector<std::pair<Terms, Terms>> rules;
	};

	constexpr bool is_nq_term(Token x) {
		return cmp_any(x.kind,
			TokenKind::COPY,
			TokenKind::DISCARD,
			TokenKind::WRAP,
			TokenKind::UNWRAP,
			TokenKind::COMBINE,
			TokenKind::SWAP,
			TokenKind::IDENT);
	}

	constexpr bool is_term(Token x) {
		return any(is_nq_term(x), cmp_any(x.kind,
			TokenKind::LPAREN));
	}

	inline void term(Context& ctx, Lexer& lx, Terms& terms, bool& quoted) {
		expect(lx, is_term, ErrorKind::EXPECT_TERM);

		Token tok = take(lx);
		auto [sv, kind] = tok;

		terms.emplace_back(tok);

		if (kind == TokenKind::LPAREN) {
			quoted = true;

			while (is_term(lx.peek))
				term(ctx, lx, terms, quoted);

			expect(lx, is(TokenKind::RPAREN), ErrorKind::EXPECT_RPAREN);
			Token rparen = take(lx);

			terms.emplace_back(rparen);
		}
	}

	template <typename F>
	[[nodiscard]] inline Terms execute(Context& ctx, Lexer& lx, F&& cb) {
		Terms terms;
		terms.reserve(25);

		while (lx.peek.kind != TokenKind::TERMINATOR) {
			bool quoted = false;

			do
				term(ctx, lx, terms, quoted);
			while (is_term(lx.peek));

			if (lx.peek.kind == TokenKind::QUERY) {
				Token query = take(lx);

				std::sort(ctx.rules.begin(), ctx.rules.end(), [] (auto& lhs, auto& rhs) {
					return lhs.first.size() > rhs.first.size();
				});

				auto it = terms.begin();

				for (auto rit = ctx.rules.begin(); rit != ctx.rules.end();) {
					auto& [lhs, rhs] = *rit;

					if (lhs.size() > terms.size()) {
						++rit;
						continue;
					}

					if (it == terms.end())
						break;

					MLATU_LOG(LogLevel::INF, "checking ", lhs, " against ", Terms { it, terms.end() });

					bool match = std::equal(
						it, it + lhs.size(),
						lhs.begin(), lhs.end());

					if (not match) {
						++rit;

						if (rit == ctx.rules.end()) {
							MLATU_LOG(LogLevel::ERR, MLATU_BOLD "no matches!" MLATU_RESET);
							it++;
							rit = ctx.rules.begin();
						}

						continue;
					}

					MLATU_LOG(LogLevel::WRN, MLATU_BOLD "  match! ", lhs, " => ", rhs, MLATU_RESET);

					size_t off = std::distance(terms.begin(), it);

					it = terms.erase(it, it + lhs.size());
					it = terms.insert(it, rhs.begin(), rhs.end());

					it = terms.begin() + off;
					rit = ctx.rules.begin();

					MLATU_LOG(LogLevel::OK, "    terms = ", terms);
				}

				cb(terms);
			}

			else if (lx.peek.kind == TokenKind::ASSIGN) {
				if (quoted)
					report(lx.peek.sv, ErrorKind::QUOTED_TERM);

				auto sep = terms.end();   // End of lhs, beginning of rhs.
				Token assign = take(lx);

				while (is_term(lx.peek))
					term(ctx, lx, terms, quoted);

				expect(lx, is(TokenKind::END), ErrorKind::EXPECT_END);
				Token end = take(lx);

				auto& [lhs, rhs] = ctx.rules.emplace_back();

				lhs.insert(lhs.end(), terms.begin(), sep);
				rhs.insert(rhs.end(), sep, terms.end());
			}

			else
				report(lx.peek.sv, ErrorKind::EXPECT_STATEMENT);

			terms.clear();  // Important.
		}

		return terms;
	}
}

#endif
