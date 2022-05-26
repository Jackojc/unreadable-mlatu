#include <utility>
#include <string>
#include <string_view>
#include <filesystem>
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
	}

	catch (Report x) {
		report_handler(std::cerr, x);
	}

	return 0;
}
