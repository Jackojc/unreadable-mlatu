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
		View out { sv.begin, sv.begin };

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
		X(EXPECT_IDENT,     "expected identifier")

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

	constexpr bool is_reserved(View sv) {
		return cmp_any(chr(sv), '*', '(', ')', '?', '=', '.');
	}

	#define TERM_KINDS \
		X(NONE,       "none") \
		X(TERMINATOR, "eof") \
		\
		X(IDENT,  "ident") \
		X(MANY,   "*") \
		X(SINGLE, "'") \
		X(ASSIGN, "=") \
		X(END,    ".") \
		X(QUERY,  "?") \
		X(LPAREN, "(") \
		X(RPAREN, ")") \

	#define X(a, b) a,
		enum class TermKind: size_t { TERM_KINDS };
	#undef X

	namespace detail {
		#define X(a, b) b,
			constexpr const char* TOK2STR[] = { TERM_KINDS };
		#undef X

		constexpr const char* trm2str(TermKind x) {
			return TOK2STR[static_cast<size_t>(x)];
		}
	}

	inline std::ostream& operator<<(std::ostream& os, TermKind x) {
		return print(os, detail::trm2str(x));
	}

	struct Term {
		View sv;
		TermKind kind;

		constexpr Term(View sv_, TermKind kind_):
			sv(sv_), kind(kind_) {}
	};

	inline std::ostream& operator<<(std::ostream& os, Term x) {
		return print(os, '{', x.kind, ",'", x.sv, "'}");
	}

	constexpr bool operator==(Term lhs, Term rhs) {
		return
			(lhs.kind == rhs.kind and
			cmp(lhs.sv, rhs.sv));
	}

	constexpr bool operator!=(Term lhs, Term rhs) {
		return not(lhs == rhs);
	}

	struct Lexer {
		View src;
		View sv;

		Term peek;
		Term prev;

		constexpr Lexer(View src_):
			src(src_), sv(src_),
			peek(mlatu::peek(src_), TermKind::NONE),
			prev(mlatu::peek(src_), TermKind::NONE) {}
	};

	[[nodiscard]] inline Term take(Lexer& lx) {
		View ws = take_while(lx.sv, is_whitespace);
		Term trm { peek(lx.sv), TermKind::NONE };

		if (empty(lx.sv) or cmp(trm.sv, "\0"_sv))
			trm.kind = TermKind::TERMINATOR;

		else if (cmp(trm.sv, "#"_sv)) {
			View comment = take_while(lx.sv, [] (View sv) {
				return not cmp(sv, "\n"_sv);
			});

			return take(lx);
		}

		else if (cmp(trm.sv, "("_sv)) { trm.kind = TermKind::LPAREN;  lx.sv = next(lx.sv); }
		else if (cmp(trm.sv, ")"_sv)) { trm.kind = TermKind::RPAREN;  lx.sv = next(lx.sv); }
		else if (cmp(trm.sv, "?"_sv)) { trm.kind = TermKind::QUERY;   lx.sv = next(lx.sv); }
		else if (cmp(trm.sv, "="_sv)) { trm.kind = TermKind::ASSIGN;  lx.sv = next(lx.sv); }
		else if (cmp(trm.sv, "."_sv)) { trm.kind = TermKind::END;     lx.sv = next(lx.sv); }

		else if (cmp(trm.sv, "*"_sv)) {
			trm.kind = TermKind::MANY;
			View star = take(lx.sv);

			trm.sv = take_while(lx.sv, [] (View sv) {
				return is_visible(sv) and not is_reserved(sv);
			});

			if (empty(trm.sv))
				report(trm.sv, ErrorKind::EXPECT_IDENT);

			trm.sv = stretch(star, trm.sv);
		}

		else if (is_visible(trm.sv)) {
			trm.kind = TermKind::IDENT;
			trm.sv = take_while(lx.sv, [] (View sv) {
				return is_visible(sv) and not is_reserved(sv);
			});
		}

		else
			report(trm.sv, ErrorKind::UNKNOWN_CHAR);

		Term out = lx.peek;

		lx.prev = lx.peek;
		lx.peek = trm;

		return out;
	}

	constexpr decltype(auto) is(TermKind kind) {
		return [=] (Term other) { return kind == other.kind; };
	}

	template <typename F> constexpr void expect(Lexer& lx, F&& fn, ErrorKind x) {
		if (not fn(lx.peek))
			report(lx.peek.sv, x);
	}
}

namespace mlatu {
	using Terms = std::vector<Term>;

	inline std::ostream& operator<<(std::ostream& os, const Terms& x) {
		print(os, '[', x.front().sv);

		for (auto it = x.begin() + 1; it != x.end(); ++it)
			print(os, " ", it->sv);

		return print(os, ']');
	}

	struct Context {
		std::vector<std::pair<Terms, Terms>> rules;
	};

	constexpr bool is_term(Term x) {
		return cmp_any(x.kind,
			TermKind::LPAREN,
			TermKind::MANY,
			TermKind::IDENT);
	}

	inline Terms::iterator match_parens(Terms::iterator it) {
		size_t depth = 1;

		while (depth > 0) {
			if      (it->kind == TermKind::LPAREN) depth++;
			else if (it->kind == TermKind::RPAREN) depth--;

			it++;
		}

		return it;
	}

	inline void term(Context& ctx, Lexer& lx, Terms& terms) {
		expect(lx, is_term, ErrorKind::EXPECT_TERM);

		Term trm = take(lx);
		auto [sv, kind] = trm;

		terms.emplace_back(trm);

		if (kind == TermKind::LPAREN) {
			while (is_term(lx.peek))
				term(ctx, lx, terms);

			expect(lx, is(TermKind::RPAREN), ErrorKind::EXPECT_RPAREN);
			Term rparen = take(lx);

			terms.emplace_back(rparen);
		}
	}

	template <typename F>
	[[nodiscard]] inline Terms execute(Context& ctx, Lexer& lx, const F& cb) {
		Terms terms;

		while (lx.peek.kind != TermKind::TERMINATOR) {
			do
				term(ctx, lx, terms);
			while (is_term(lx.peek));

			if (lx.peek.kind == TermKind::QUERY) {
				Term query = take(lx);

				std::stable_sort(ctx.rules.begin(), ctx.rules.end(), [] (auto& lhs, auto& rhs) {
					return lhs.first.size() > rhs.first.size();
				});

				if (ctx.rules.empty())
					continue;

				for (auto it = terms.begin(); it != terms.end();) {
					loop_begin:
					for (auto& [match, replacement]: ctx.rules) {
						auto rw_begin = it;
						auto match_it = match.begin();

						while (it != terms.end() and match_it != match.end()) {
							if (match_it->kind == TermKind::MANY) {
								if (it->kind != TermKind::LPAREN)
									break;  // Goto next rule

								it++, match_it++;
								it = match_parens(it);

								continue;  // Skip incrementing iterators at the end of loop.
							}

							else if (not (*it == *match_it))
								break;  // Goto next rule

							it++; match_it++;
						}

						if (match_it == match.end()) {
							MLATU_LOG(LogLevel::WRN, Terms { rw_begin, it });
							MLATU_LOG(LogLevel::WRN, "   ", match, " => ", replacement);

							it = terms.erase(rw_begin, it);
							it = terms.insert(rw_begin, replacement.begin(), replacement.end());

							it = terms.begin();

							goto loop_begin;
						}

						it = rw_begin;
						MLATU_LOG(LogLevel::ERR, Terms { it, terms.end() });
					}

					it++;
				}

				cb(terms);  // User callback.
			}

			else if (lx.peek.kind == TermKind::ASSIGN) {
				size_t sep = std::distance(terms.begin(), terms.end());

				Term assign = take(lx);

				while (is_term(lx.peek))
					term(ctx, lx, terms);

				expect(lx, is(TermKind::END), ErrorKind::EXPECT_END);
				Term end = take(lx);

				auto& [lhs, rhs] = ctx.rules.emplace_back();

				lhs.insert(lhs.end(), terms.begin(), terms.begin() + sep);
				rhs.insert(rhs.end(), terms.begin() + sep, terms.end());
			}

			else
				report(lx.peek.sv, ErrorKind::EXPECT_STATEMENT);

			terms.clear();  // Important.
		}

		return terms;
	}
}

#endif
