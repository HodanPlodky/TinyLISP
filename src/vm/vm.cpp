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

struct LDC;
struct NIL;
struct ADD;
struct SUB;
struct MUL;
struct DIV;
struct CONS;
struct CAR;
struct CDR;
struct CONSP;
struct SEL;
struct JOIN;
struct LD; 
struct LDF;
struct AP;
struct RTN;

namespace inst {
using Inst = std::variant<
    std::shared_ptr<LDC>,
    std::shared_ptr<NIL>,
    std::shared_ptr<ADD>,
    std::shared_ptr<SUB>,
    std::shared_ptr<MUL>,
    std::shared_ptr<CONS>,
    std::shared_ptr<CAR>,
    std::shared_ptr<CDR>,
    std::shared_ptr<CONSP>,
    std::shared_ptr<SEL>,
    std::shared_ptr<JOIN>,
    std::shared_ptr<LD>,
    std::shared_ptr<LDF>,
    std::shared_ptr<AP>,
    std::shared_ptr<RTN>>;

struct LDC {
    LDC(int number) : number(number) {}
    int number;
};

struct LD {
    LD(int i, int j) : i(i), j(j) {}
    int i, j;
};

struct LDF {
    LDF(Inst inner) : inner(inner) {}
    Inst inner;
};
}

struct ConsCell;

using StackData = std::variant<
    std::shared_ptr<ConsCell>, std::shared_ptr<int>>;

struct ConsCell {
    ConsCell(StackData car, StackData cdr) : 
        car(car),
        cdr(cdr) {}
    StackData car, cdr;
};

ConsCell readInst(std::ifstream & stream) {
    long out;
    char buffer[8];
    while (!stream.eof()) {
        stream.read(buffer, sizeof out);
        out = 
            (long)buffer[7] | (long)buffer[6] << 8 | (long)buffer[5] << 16 | (long)buffer[4] << 24; 
            (long)buffer[3] << 32 | (long)buffer[2]<< 40 | (long)buffer[1] << 48 | (long)buffer[0] << 56; 
        switch (out) {

        }
    }
    return ConsCell(std::shared_ptr<int>(0), std::shared_ptr<int>(0));
} 

int main(int argc, char ** argv) {
    if (argc != 2) return 1;
    std::ifstream file(argv[1], std::ios::in | std::ios::binary);
    readInst(file);
    return 0;
};