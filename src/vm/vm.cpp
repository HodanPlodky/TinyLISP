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
        (long)buffer[7] | (long)buffer[6] << 8 | (long)buffer[5] << 16 | (long)buffer[4] << 24 | 
        (long)buffer[3] << 32 | (long)buffer[2]<< 40 | (long)buffer[1] << 48 |
        (long)buffer[0] << 56; 
}

std::shared_ptr<secd::Code> readInst(std::ifstream & stream) {
    long out;
    char buffer[8];
    auto code = std::make_shared<secd::Code>(secd::Code());

    while (!stream.eof()) {
        out = readLong(stream);
        switch (out) {
            case 0x00 : {
                auto codeinner = readInst(stream);
                code->add(std::move(codeinner->getData()));
                break;
            }
            case 0x01 : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::ADD>(inst::ADD())));
                break;
            }
            case 0x02 : {
                if (stream.eof())
                    throw std::runtime_error("After LDC must be number");
                out = readLong(stream);
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::LDC>(inst::LDC(out))));
                break;
            }
            case 0x03 : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::NIL>(inst::NIL())));
                break;
            }
            case 0x04 : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::SUB>(inst::SUB())));
                break;
            }
            case 0x05 : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::MUL>(inst::MUL())));
                break;
            }
            case 0x06 : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::DIV>(inst::DIV())));
                break;
            }
            case 0x07 : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::CONS>(inst::CONS())));
                break;
            }
            case 0x08 : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::CAR>(inst::CAR())));
                break;
            }
            case 0x09 : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::CDR>(inst::CDR())));
                break;
            }
            case 0x0a : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::CONSP>(inst::CONSP())));
                break;
            }
            case 0x0b : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::SEL>(inst::SEL())));
                break;
            }
            case 0x0c : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::JOIN>(inst::JOIN())));
                break;
            }
            case -1 : {
                return code;
            }
        }
    }
    return code;
} 

void writeValue(secd::Value<int> val) {
    if (std::holds_alternative<std::shared_ptr<int>>(val)) {
        std::cout << *std::get<std::shared_ptr<int>>(val) << std::endl;
    }
    else {
        throw std::runtime_error("Not implemented");
    }
}

void run(std::shared_ptr<secd::Code> code) {
    auto datastack = secd::Stack<int>();
    while(!code->empty()) {
        if (code->isHeadList()) {
            code = std::make_shared<secd::Code>(std::move(secd::Code(code->head())));
            continue;
        }
        auto instruction = code->nextInst();
        if (std::holds_alternative<std::shared_ptr<inst::LDC>>(instruction)) {
            std::cout << "LDC" << std::endl;
            auto ldc = std::get<std::shared_ptr<inst::LDC>>(instruction);
            std::cout << "number : " << ldc->number << std::endl;
            datastack.push(std::make_shared<int>(ldc->number));
        }
        else if (std::holds_alternative<std::shared_ptr<inst::ADD>>(instruction)) {
            std::cout << "ADD" << std::endl;
            std::get<std::shared_ptr<inst::ADD>>(instruction);
            if (datastack.empty())
                throw std::runtime_error("ADD require two arguments on stack found 0");
            auto x = datastack.top();
            datastack.pop();
            if (datastack.empty())
                throw std::runtime_error("ADD require two arguments on stack found 1");
            auto y = datastack.top();
            datastack.pop();
            if (
                std::holds_alternative<std::shared_ptr<int>>(x) &&
                std::holds_alternative<std::shared_ptr<int>>(y)
            ) {
                int nx = *std::get<std::shared_ptr<int>>(x);
                int ny = *std::get<std::shared_ptr<int>>(y);
                datastack.push(std::make_shared<int>(nx + ny));
            }
            else {
                throw std::runtime_error("ADD requires two numbers, got different type");
            }
        }
    }

    if (!datastack.empty())
        writeValue(datastack.top());
}

int main(int argc, char ** argv) {
    if (argc != 2) return 1;
    std::ifstream file(argv[1], std::ios::in | std::ios::binary);
    auto code = readInst(file);
    try {
        run(code);
    }
    catch(const std::exception& e) {
        std::cout << "runtime error : " << e.what() << std::endl;
    }
    return 0;
};