/*
 * $ clang++-7 powerline.cpp -std=c++17 -O3 -Wall -o powerline
 *
 * usage:
 * ./powerline "#FFFFFF" "#000000" "#AAAAAA" "some text to wrap" [flags]
 *
 * --no-arrow                  doesn't print the powerline arrow
 * --no-left-bg                doesn't set background for arrow
 * --no-left-fg                doesn't set foreground for arrow
 * --no-right-bg               doesn't set background for text
 * --no-right-fg               doesn't set foreground for text
 * 
 * Notes:
 *   If less than four arguments are provided nothing will be printed
 * 
 *   If the input text is exactly 3 lines and the third line exactly matches ^#[a-fA-F0-9]{6}$
 *   then the third input line will be used in place of the text's foreground color
 *
 *   If input text happens to be empty there will still be an "empty" segment printed
 *
 * Example:
 * $ ./powerline "#002b36" "#338fcc" "#111111" " $(date '+%T') "
 * <span background="#002b36"  foreground="#338fcc"></span><span background="#338fcc"  foreground="#111111"> 11:08:43 </span>

 */


#include <string>
#include <cstring>
#include <vector>
#include <stdio.h>
#include <sstream>

template<typename Out>
static inline void split(const std::string &s, char delim, Out result) {
    std::stringstream ss(s);
    std::string item;
    while (std::getline(ss, item, delim)) {
        *(result++) = item;
    }
}

inline bool is_hex_digit(char c) {
    return ('a' <= c && c <= 'f') ||
        ('A' <= c && c <= 'F') ||
        ('0' <= c && c <= '9');
}

int main(int argc, char **argv) {
    --argc; ++argv;
    if (argc < 4) {
        exit(0);
    }

    std::string color_a(argv[0]);
    std::string color_b(argv[1]);
    std::string text_color(argv[2]);


    std::vector<std::string> lines;
    split(argv[3], '\n', std::back_inserter(lines));
    
    std::string text("");
    if (lines.size() != 0) {
        text = lines[0];
    }

    if (lines.size() == 3) {
        const auto &tcolor = lines[2];
        if (tcolor.size() == 7 && tcolor[0] == '#') {
            bool matches = true;
            for (int i = 1; i < 7; ++i) {
                if (!is_hex_digit(tcolor[i])) {
                    matches = false;
                    break;
                }
            }
            if (matches) {
                text_color = tcolor;
            }
        }
    }

    bool no_left_bg = false;
    bool no_left_fg = false;
    bool no_right_bg = false;
    bool no_right_fg = false;
    bool no_arrow = false;

    for (int i = 3; i < argc; ++i) {
        const char *p = argv[i];
        if (0 == strcmp("--no-left-bg", p)) {
            no_left_bg = true;
        }
        else if (0 == strcmp("--no-left-fg", p)) {
            no_left_fg = true;
        }
        else if (0 == strcmp("--no-right-bg", p)) {
            no_right_bg = true;
        }
        else if (0 == strcmp("--no-right-fg", p)) {
            no_right_fg = true;
        }
        else if (0 == strcmp("--no-arrow", p)) {
            no_arrow = true;
        }
    }
    
    std::string output;
    if (!no_arrow) {
        output.append("<span ");
        if (!no_left_bg) {
            output.append("background=\"");
            output.append(color_a);
            output.append("\" ");
        }
        output.append(" ");
        if (!no_left_fg) {
            output.append("foreground=\"");
            output.append(color_b);
            output.append("\"");
        }
        output.append("></span>");
    }
    output.append("<span ");
    if (!no_right_bg) {
        output.append("background=\"");
        output.append(color_b);
        output.append("\" ");
    }
    output.append(" ");
    if (!no_right_fg) {
        output.append("foreground=\"");
        output.append(text_color);
        output.append("\"");
    }
    output.append(">");
    output.append(text);
    output.append("</span>");
    
    puts(output.c_str());
    
    exit(0);
}
