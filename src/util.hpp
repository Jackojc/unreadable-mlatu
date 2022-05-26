#ifndef MLATU_UTIL_HPP
#define MLATU_UTIL_HPP

#include <cstddef>

namespace mlatu {
	// Utility macros.
	// Stringify a macro def.
	// i.e. MLATU_STR(__LINE__) => "42" as opposed to "__LINE__"
	#define MLATU_STR_IMPL_(x) #x
	#define MLATU_STR(x) MLATU_STR_IMPL_(x)

	// Concatenate macro defs.
	// i.e. MLATU_CAT(__FILE__, __LINE__) => "foo.c10" as opposed to "__FILE____LINE__"
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


	// Check if any arguments are true.
	template <typename T, typename... Ts>
	constexpr bool any(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) or std::forward<Ts>(rest)) or ...);
	}

	// Check if all arguments are true.
	template <typename T, typename... Ts>
	constexpr bool all(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) and std::forward<Ts>(rest)) and ...);
	}

	// Check if all arguments are false.
	template <typename T, typename... Ts>
	constexpr bool none(T&& first, Ts&&... rest) {
		return ((not std::forward<T>(first) and not std::forward<Ts>(rest)) and ...);
	}


	// Check if all arguments are equal to first.
	template <typename T, typename... Ts>
	constexpr bool eq_all(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) == std::forward<Ts>(rest)) and ...);
	}

	// Check if any arguments are equal to first.
	template <typename T, typename... Ts>
	constexpr bool eq_any(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) == std::forward<Ts>(rest)) or ...);
	}

	// Check if none of the arguments are equal to first.
	template <typename T, typename... Ts>
	constexpr bool eq_none(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) != std::forward<Ts>(rest)) and ...);
	}


	// Higher order predicates.
	template <typename F> constexpr decltype(auto) negate(F&& fn) {
		return [fn = std::forward<F>(fn)] (const auto& x) {
			return not fn(x);
		};
	}

	template <typename T> constexpr decltype(auto) equal(T&& a) {
		return [a = std::forward<T>(a)] (const auto& b) {
			return b == a;
		};
	}

	template <typename T> constexpr decltype(auto) not_equal(T&& a) {
		return [a = std::forward<T>(a)] (const auto& b) {
			return b != a;
		};
	}

	template <typename T> constexpr decltype(auto) less(T&& a) {
		return [a = std::forward<T>(a)] (const auto& b) {
			return b < a;
		};
	}

	template <typename T> constexpr decltype(auto) less_equal(T&& a) {
		return [a = std::forward<T>(a)] (const auto& b) {
			return b <= a;
		};
	}

	template <typename T> constexpr decltype(auto) more(T&& a) {
		return [a = std::forward<T>(a)] (const auto& b) {
			return b > a;
		};
	}

	template <typename T> constexpr decltype(auto) more_equal(T&& a) {
		return [a = std::forward<T>(a)] (const auto& b) {
			return b >= a;
		};
	}

	template <typename... Ts>
	constexpr decltype(auto) partial_eq_any(Ts&&... args) {
		return [=] (const auto& x) {
			return eq_any(x, args...);
		};
	}

	template <typename... Ts>
	constexpr decltype(auto) partial_eq_all(Ts&&... args) {
		return [=] (const auto& x) {
			return eq_all(x, args...);
		};
	}

	template <typename... Ts>
	constexpr decltype(auto) partial_eq_none(Ts&&... args) {
		return [=] (const auto& x) {
			return eq_none(x, args...);
		};
	}

	// FNV-1a hash for a range of bytes.
	constexpr auto fnv1a_64(const char* begin, const char* const end) {
		T hash = 14'695'981'039'346'656'037u;

		while (begin != end) {
			hash = (hash ^ static_cast<T>(*begin)) * 1'099'511'628'211u;
			begin++;
		}

		return hash;
	}
}

#endif

