#include <cstdio>
#include <cstdlib>
#include <fstream>

void usage(void);

int main(int argc, char** argv)
{
    if (argc < 3) {
        usage();
        exit(1);
    }

    const char* outfile_path = argv[1];
    std::ofstream outfile(outfile_path, std::ios::out | std::ios::binary);
    if (!outfile.is_open()) {
        fprintf(stderr, "error: unable to open file `%s'\n", outfile_path);
        exit(1);
    }

    // we ignore errors since the user is an adult anyways
    for (int i = 2; i < argc; i++) {
        try {
            uint8_t digit = static_cast<uint8_t>(
                    std::stoul(argv[i], nullptr, 16));
            outfile.write((char*)&digit, sizeof(digit));
        } catch (const std::exception& e) {
            fprintf(stderr, "warning: at `%s', ignoring error from `%s'\n",
                    argv[i], e.what());
        }
    }
}

void usage(void)
{
    fprintf(stderr, "Usage:\n\twrite_binary_data <file> [hex_digit, ...]\n");
}
