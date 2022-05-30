#include <utility>
#include <algorithm>
#include <vector>
#include <string>
#include <string_view>
#include <filesystem>
#include <chrono>
#include <iostream>
#include <fstream>

#include <cstddef>
#include <cstdint>

#include <mlatu.hpp>

using namespace mlatu;

namespace fs = std::filesystem;

int main(int argc, const char* argv[]) {
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(nullptr);

	try {
		if (argc != 2)
			report(""_sv, ErrorKind::NO_FILE);

		std::string_view fname = argv[1];
		std::ifstream is(fs::path { fname });

		if (not is.is_open())
			report(View { fname.data(), fname.size() }, ErrorKind::READ_FILE);

		std::stringstream ss;
		ss << is.rdbuf();

		std::string src = ss.str();
		View sv { src.data(), src.size() };

		auto t1 = std::chrono::steady_clock::now();

		Lexer lx { sv };
		Token tok = take(lx);  // Prepare the lexer.

		Context ctx {};
		Terms ts = execute(ctx, lx);

		// for (Token x: ts)
		// 	println(std::cout, x.sv);

		// for (auto& [lhs, rhs]: ctx.rules) {
		// 	for (Token x: lhs)
		// 		print(std::cout, x.sv, " ");
		// 	print(std::cout, "=> ");

		// 	for (Token x: rhs)
		// 		print(std::cout, x.sv, " ");
		// 	print(std::cout, '\n');
		// }

		auto t2 = std::chrono::steady_clock::now();
		MLATU_LOG(LogLevel::OK, std::chrono::duration<double, std::micro> { t2 - t1 }.count(), "µs");
	}

	catch (Report x) {
		report_handler(std::cerr, x);
	}

	return 0;
}
