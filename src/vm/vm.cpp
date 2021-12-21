#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <deque>
#include <iostream>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <queue>
#include <set>
#include <sstream>
#include <fstream>
#include <istream>
#include <stack>
#include <string>
#include <utility>
#include <variant>
#include <vector>
#include <exception>
#include <stdexcept>

#include "types.h"

long readLong(std::ifstream & stream) {
    char buffer[8];
    stream.read(buffer, 8);
    return 
        (long)buffer[7] | (long)buffer[6] << 8 | (long)buffer[5] << 16 | (long)buffer[4] << 24; 
        (long)buffer[3] << 32 | (long)buffer[2]<< 40 | (long)buffer[1] << 48 | (long)buffer[0] << 56; 
}

secd::Code readInst(std::ifstream & stream) {
    long out;
    char buffer[8];
    secd::Stack * s = new secd::Stack();

    while (!stream.eof()) {
        out = readLong(stream);
        switch (out) {
            case 0x00 : {
                break;
            }
            case 0x01 : {
                s->push
                break;
            }
            case 0x02 : {
                if (stream.eof())
                    throw std::runtime_error("After LDC must be number");
                out = readLong(stream);
                break;
            }
            case 0x03 : {
                break;
            }
            case 0x04 : {
                break;
            }
            case 0x05 : {
                break;
            }
            case 0x06 : {
                break;
            }
            case 0x07 : {
                break;
            }
        }
    }
    return secd::Code(s);
} 

int main(int argc, char ** argv) {
    if (argc != 2) return 1;
    std::ifstream file(argv[1], std::ios::in | std::ios::binary);
    readInst(file);
    return 0;
};