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
    secd::Stack<inst::Inst> * s = new secd::Stack<inst::Inst>();

    while (!stream.eof()) {
        out = readLong(stream);
        switch (out) {
            case 0x00 : {
                break;
            }
            case 0x01 : {
                inst::Inst i = std::make_shared<inst::ADD>(inst::ADD());
                s->push(std::make_shared<inst::ADD>(inst::ADD()));
                break;
            }
            case 0x02 : {
                if (stream.eof())
                    throw std::runtime_error("After LDC must be number");
                out = readLong(stream);
                s->push(std::make_shared<inst::LDC>(inst::LDC(out)));
                break;
            }
            case 0x03 : {
                s->push(std::make_shared<inst::NIL>(inst::NIL()));
                break;
            }
            case 0x04 : {
                s->push(std::make_shared<inst::SUB>(inst::SUB()));
                break;
            }
            case 0x05 : {
                s->push(std::make_shared<inst::MUL>(inst::MUL()));
                break;
            }
            case 0x06 : {
                s->push(std::make_shared<inst::DIV>(inst::DIV()));
                break;
            }
            case 0x07 : {
                s->push(std::make_shared<inst::CONS>(inst::CONS()));
                break;
            }
            case 0x08 : {
                s->push(std::make_shared<inst::CAR>(inst::CAR()));
                break;
            }
            case 0x09 : {
                s->push(std::make_shared<inst::CDR>(inst::CDR()));
                break;
            }
            case 0x0a : {
                s->push(std::make_shared<inst::CONSP>(inst::CONSP()));
                break;
            }
            case 0x0b : {
                s->push(std::make_shared<inst::SEL>(inst::SEL()));
                break;
            }
            case 0x0c : {
                s->push(std::make_shared<inst::JOIN>(inst::JOIN()));
                break;
            }
            case 0xff : {
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