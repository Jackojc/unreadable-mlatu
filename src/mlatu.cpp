#include <utility>
#include <algorithm>
#include <vector>
#include <unordered_map>
#include <string>
#include <string_view>
#include <filesystem>
#include <chrono>
#include <thread>
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
		Term t = take(lx);  // Prepare the lexer.

		Context ctx {};
		Terms ts = execute(ctx, lx, [&] (Terms x) {
			println(std::cout, x);
		});

		auto t2 = std::chrono::steady_clock::now();
		// MLATU_LOG(LogLevel::OK, std::chrono::duration<double, std::micro> { t2 - t1 }.count(), "µs");
		println(std::cerr, std::chrono::duration<double, std::micro> { t2 - t1 }.count(), "µs");
	}

	catch (Report x) {
		report_handler(std::cerr, x);
	}

	return 0;
}
