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
 * --pad-left     -l           adds 1 space left of text
 * --pad-right    -r           adds 1 space right of text
 * --pad-both     -b           adds 1 space to both left AND right of text
 * --rainbow-text              randomizes the foreground of each character in text 
 *                             (supports valid single codepoint utf-8)
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
#include <array>
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

inline bool arg_match(const char* a, const char *b) {
    return 0 == strcmp(a, b);
}

static const std::array<const char*, 256> color_table = {
    "#000000", "#800000", "#008000", "#808000", "#000080", "#800080", "#008080", "#c0c0c0",
    "#808080", "#ff0000", "#00ff00", "#ffff00", "#0000ff", "#ff00ff", "#00ffff", "#ffffff",
    "#000000", "#00005f", "#000087", "#0000af", "#0000d7", "#0000ff", "#005f00", "#005f5f",
    "#005f87", "#005faf", "#005fd7", "#005fff", "#008700", "#00875f", "#008787", "#0087af",
    "#0087d7", "#0087ff", "#00af00", "#00af5f", "#00af87", "#00afaf", "#00afd7", "#00afff",
    "#00d700", "#00d75f", "#00d787", "#00d7af", "#00d7d7", "#00d7ff", "#00ff00", "#00ff5f",
    "#00ff87", "#00ffaf", "#00ffd7", "#00ffff", "#5f0000", "#5f005f", "#5f0087", "#5f00af",
    "#5f00d7", "#5f00ff", "#5f5f00", "#5f5f5f", "#5f5f87", "#5f5faf", "#5f5fd7", "#5f5fff",
    "#5f8700", "#5f875f", "#5f8787", "#5f87af", "#5f87d7", "#5f87ff", "#5faf00", "#5faf5f",
    "#5faf87", "#5fafaf", "#5fafd7", "#5fafff", "#5fd700", "#5fd75f", "#5fd787", "#5fd7af",
    "#5fd7d7", "#5fd7ff", "#5fff00", "#5fff5f", "#5fff87", "#5fffaf", "#5fffd7", "#5fffff",
    "#870000", "#87005f", "#870087", "#8700af", "#8700d7", "#8700ff", "#875f00", "#875f5f",
    "#875f87", "#875faf", "#875fd7", "#875fff", "#878700", "#87875f", "#878787", "#8787af",
    "#8787d7", "#8787ff", "#87af00", "#87af5f", "#87af87", "#87afaf", "#87afd7", "#87afff",
    "#87d700", "#87d75f", "#87d787", "#87d7af", "#87d7d7", "#87d7ff", "#87ff00", "#87ff5f",
    "#87ff87", "#87ffaf", "#87ffd7", "#87ffff", "#af0000", "#af005f", "#af0087", "#af00af",
    "#af00d7", "#af00ff", "#af5f00", "#af5f5f", "#af5f87", "#af5faf", "#af5fd7", "#af5fff",
    "#af8700", "#af875f", "#af8787", "#af87af", "#af87d7", "#af87ff", "#afaf00", "#afaf5f",
    "#afaf87", "#afafaf", "#afafd7", "#afafff", "#afd700", "#afd75f", "#afd787", "#afd7af",
    "#afd7d7", "#afd7ff", "#afff00", "#afff5f", "#afff87", "#afffaf", "#afffd7", "#afffff",
    "#d70000", "#d7005f", "#d70087", "#d700af", "#d700d7", "#d700ff", "#d75f00", "#d75f5f",
    "#d75f87", "#d75faf", "#d75fd7", "#d75fff", "#d78700", "#d7875f", "#d78787", "#d787af",
    "#d787d7", "#d787ff", "#d7af00", "#d7af5f", "#d7af87", "#d7afaf", "#d7afd7", "#d7afff",
    "#d7d700", "#d7d75f", "#d7d787", "#d7d7af", "#d7d7d7", "#d7d7ff", "#d7ff00", "#d7ff5f",
    "#d7ff87", "#d7ffaf", "#d7ffd7", "#d7ffff", "#ff0000", "#ff005f", "#ff0087", "#ff00af",
    "#ff00d7", "#ff00ff", "#ff5f00", "#ff5f5f", "#ff5f87", "#ff5faf", "#ff5fd7", "#ff5fff",
    "#ff8700", "#ff875f", "#ff8787", "#ff87af", "#ff87d7", "#ff87ff", "#ffaf00", "#ffaf5f",
    "#ffaf87", "#ffafaf", "#ffafd7", "#ffafff", "#ffd700", "#ffd75f", "#ffd787", "#ffd7af",
    "#ffd7d7", "#ffd7ff", "#ffff00", "#ffff5f", "#ffff87", "#ffffaf", "#ffffd7", "#ffffff",
    "#080808", "#121212", "#1c1c1c", "#262626", "#303030", "#3a3a3a", "#444444", "#4e4e4e",
    "#585858", "#626262", "#6c6c6c", "#767676", "#808080", "#8a8a8a", "#949494", "#9e9e9e",
    "#a8a8a8", "#b2b2b2", "#bcbcbc", "#c6c6c6", "#d0d0d0", "#dadada", "#e4e4e4", "#eeeeee",
};
    

void rainbowize(std::string &output, const std::string &text, const std::string &background) {
    for(size_t i = 0; i < text.length();) {
        int cplen = 1;

        if ((text[i] & 0xf8) == 0xf0) {
            cplen = 4;
        }
        else if ((text[i] & 0xf0) == 0xe0) {
            cplen = 3;
        }
        else if ((text[i] & 0xe0) == 0xc0) {
            cplen = 2;
        }

        if ((i + cplen) > text.length()) {
            cplen = 1;
        }
        output.append("<span ");
        output.append(background);
        output.append(" foreground=\"");
        
        output.append(color_table[rand() % color_table.size()]);

        output.append("\">");
        output.append(text.substr(i, cplen));
        output.append("</span>");
        i += cplen;
    }
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
    bool pad_left = false;
    bool pad_right = false;
    bool rainbow_text = false;

    for (int i = 3; i < argc; ++i) {
        const char *p = argv[i];
        if (arg_match("--no-left-bg", p)) {
            no_left_bg = true;
        }
        else if (arg_match("--no-left-fg", p)) {
            no_left_fg = true;
        }
        else if (arg_match("--no-right-bg", p)) {
            no_right_bg = true;
        }
        else if (arg_match("--no-right-fg", p)) {
            no_right_fg = true;
        }
        else if (arg_match("--no-arrow", p)) {
            no_arrow = true;
        }
        else if (arg_match("-l", p) || arg_match("--pad-left", p)) {
            pad_left = true;
        }
        else if (arg_match("-r", p) || arg_match("--pad-right", p)) {
            pad_right = true;
        }
        else if (arg_match("-b", p) || arg_match("--pad-both", p)) {
            pad_left = true;
            pad_right = true;
        }
        else if (arg_match("--rainbow-text", p)) {
            rainbow_text = true;
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

    if (pad_left) {
        output.append(" ");
    }

    if (rainbow_text) {
        srand(time(0));
        rainbowize(output, text, "");
    }
    else {
        output.append(text);
    }

    if (pad_right) {
        output.append(" ");
    }

    output.append("</span>");
    
    puts(output.c_str());
    
    exit(0);
}
