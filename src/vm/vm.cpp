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
#include <functional>

#include "types.h"

using secd::Data;

long readLong(std::ifstream & stream) {
    uint8_t buffer[8];
    stream.read((char*)buffer, 8);
    /*
    for (int i = 0; i < 8; i++) {
        unsigned int x = 0;
        x += buffer[i];
        std::cout << std::hex << x << " ";
    }
    std::cout << std::dec << std::endl;
    */
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
                code->addData(codeinner->getData());
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
            case 0x0d : {
                if (stream.eof())
                    throw std::runtime_error("After LD must be number");
                auto x = readLong(stream);
                if (stream.eof())
                    throw std::runtime_error("After LD must be two number");
                auto y = readLong(stream);
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::LD>(inst::LD(x, y))));
                break;
            }
            case 0x0e : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::LDF>(inst::LDF())));
                break;
            }
            case 0x0f : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::AP>(inst::AP())));
                break;
            }
            case 0x10 : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::RTN>(inst::RTN())));
                break;
            }
            case 0x11 : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::EQ>(inst::EQ())));
                break;
            }
            case 0x12 : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::GT>(inst::GT())));
                break;
            }
            case 0x13 : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::LT>(inst::LT())));
                break;
            }
            case 0x14 : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::DUM>(inst::DUM())));
                break;
            }
            case 0x15 : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::RAP>(inst::RAP())));
                break;
            }
            case 0xfe : {
                code->add(std::make_shared<inst::Inst>
                    (std::make_shared<inst::ERR>(inst::ERR())));
                break;
            }
            case 0xff : {
                return code;
            }
        }
    }
    return code;
} 

void binaryop(
    secd::Stack<Data> & datastack, 
    std::function<secd::Value<Data>(secd::Value<Data>, secd::Value<Data>)> op, 
    std::string name
) {
    if (datastack.empty())
        throw std::runtime_error("ADD require two arguments on stack found 0");
    auto x = datastack.top();
    datastack.pop();
    if (datastack.empty())
        throw std::runtime_error("ADD require two arguments on stack found 1");
    auto y = datastack.top();
    datastack.pop();
    datastack.push(op(x, y));
}

void numbinaryop(
    secd::Stack<Data> & datastack, 
    std::function<int(int, int)> op, 
    std::string name
) {
    auto numop = [name, op](secd::Value<Data> x, secd::Value<Data> y) -> secd::Value<Data>{
        if (
            std::holds_alternative<std::shared_ptr<Data>>(x) &&
            std::holds_alternative<std::shared_ptr<Data>>(y)
        ) {
            auto dx = *std::get<std::shared_ptr<Data>>(x);
            auto dy = *std::get<std::shared_ptr<Data>>(y);
            if (
               std::holds_alternative<int>(dx) && 
               std::holds_alternative<int>(dy)
            ) {
                int nx = std::get<int>(dx);
                int ny = std::get<int>(dy);
                return std::make_shared<Data>(op(nx, ny));
            }
            else { 
                throw std::runtime_error(
                    name + std::string(" requires two numbers, got different type"));
            }
        }
        else {
            throw std::runtime_error(
                name + std::string(" requires two numbers, got different type"));
        }
        return secd::Nil;
    };
    binaryop(datastack, numop, name);
}

void traverse_stack(secd::Stack<Data> & st) {
    if(st.empty())
        return;
    secd::Value<Data> x = st.top();
    st.pop();
    traverse_stack(st);
    secd::showValue(x);
    std::cout << std::endl;
    st.push(x);
} 

void verboseWriteout(
    std::shared_ptr<secd::Code> code,
    secd::Stack<Data> & datastack,
    secd::Dump & dump,
    secd::Enviroment & env,
    int cycle
) {
            //system("clear");
            std::cout << "cycle : " << cycle << std::endl;
            std::cout << "stack : " << std::endl;
            traverse_stack(datastack);
            std::cout << std::endl;
            std::cout << "dump : " << std::endl;
            traverse_stack(dump.getData());
            std::cout << std::endl;
            std::cout << "env : ";
            secd::showValue(env.get());
            std::cout << std::endl;
            std::cout << std::endl;
            std::cout << "code : ";
            secd::showValue(code->getData());
            std::cout << std::endl;
            std::cout << std::endl;
            std::cin.get();
            std::cout << std::endl;
            std::cout << std::endl;
            std::cout << std::endl;
            std::cout << std::endl;
            std::cout << std::endl;
            std::cout << std::endl;
            std::cout << std::endl;
            std::cout << std::endl;
            std::cout << std::endl;
            std::cout << std::endl;
            std::cout << std::endl;
            std::cout << std::endl;
            std::cout << std::endl;
            std::cout << std::endl;
}

bool tmpCompareData(secd::Value<Data> x, secd::Value<Data> y) {
    if (
        std::holds_alternative<std::shared_ptr<Data>>(x) &&
        std::holds_alternative<std::shared_ptr<Data>>(y)
    ) {
        auto dx = *std::get<std::shared_ptr<Data>>(x);
        auto dy = *std::get<std::shared_ptr<Data>>(y);
        if (
            std::holds_alternative<int>(dx) &&
            std::holds_alternative<int>(dy)
        ) {
            int nx = std::get<int>(dx);
            int ny = std::get<int>(dy);
            return ny == nx;
        }
        return false;
    }
    else if (
        std::holds_alternative<std::shared_ptr<secd::NilT>>(x) &&
        std::holds_alternative<std::shared_ptr<secd::NilT>>(y)
    ) {
        return true;
    }
    else if (
        std::holds_alternative<std::shared_ptr<secd::ConsCell<Data>>>(x) &&
        std::holds_alternative<std::shared_ptr<secd::ConsCell<Data>>>(y)
    ) {
        return 
            tmpCompareData(secd::car(x), secd::car(y)) &&
            tmpCompareData(secd::cdr(x), secd::cdr(y));
    }
    return false;
}

void run(
    std::shared_ptr<secd::Code> code,
    secd::Stack<Data> & datastack,
    secd::Dump & dump,
    secd::Enviroment & env,
    bool verbose
) {
    int cycle = -1;
    while(!code->empty()) {
        cycle++;
        if (verbose) {
            verboseWriteout(code, datastack, dump, env, cycle);
        }
        if (code->isHeadList()) {
            auto list = code->next();
            code = std::make_shared<secd::Code>(secd::Code(secd::appendLists(list, code->getData())));
            continue;
        }
        auto instruction = code->nextInst();
        if (std::holds_alternative<std::shared_ptr<inst::LDC>>(instruction)) {
            auto ldc = std::get<std::shared_ptr<inst::LDC>>(instruction);
            datastack.push(std::make_shared<Data>(ldc->number));
        }
        else if (std::holds_alternative<std::shared_ptr<inst::ADD>>(instruction)) {
            numbinaryop(datastack, [](int x, int y) {return x + y;}, "ADD");
        }
        else if (std::holds_alternative<std::shared_ptr<inst::SUB>>(instruction)) {
            numbinaryop(datastack, [](int x, int y) {return y - x;}, "SUB");
        }
        else if (std::holds_alternative<std::shared_ptr<inst::MUL>>(instruction)) {
            numbinaryop(datastack, [](int x, int y) {return y * x;}, "MUL");
        }
        else if (std::holds_alternative<std::shared_ptr<inst::DIV>>(instruction)) {
            numbinaryop(datastack, [](int x, int y) {return y / x;}, "DIV");
        }
        else if (std::holds_alternative<std::shared_ptr<inst::EQ>>(instruction)) {
            auto op = [](secd::Value<Data> x, secd::Value<Data> y) -> secd::Value<Data> {
                return std::make_shared<Data>(tmpCompareData(x, y) ? 1 : 0);
            };
            binaryop(datastack, op, "EQ");
            ///numbinaryop(datastack, [](int x, int y) {return y == x ? 1 : 0;}, "EQ");
        }
        else if (std::holds_alternative<std::shared_ptr<inst::GT>>(instruction)) {
            numbinaryop(datastack, [](int x, int y) {return y > x ? 1 : 0;}, "GT");
        }
        else if (std::holds_alternative<std::shared_ptr<inst::LT>>(instruction)) {
            numbinaryop(datastack, [](int x, int y) {return y < x ? 1 : 0;}, "LT");
        }
        else if (std::holds_alternative<std::shared_ptr<inst::CAR>>(instruction)) {
            auto tmp = datastack.top();
            datastack.pop();
            datastack.push(secd::car(tmp));
        }
        else if (std::holds_alternative<std::shared_ptr<inst::CDR>>(instruction)) {
            auto tmp = datastack.top();
            datastack.pop();
            datastack.push(secd::cdr(tmp));
        }
        else if (std::holds_alternative<std::shared_ptr<inst::JOIN>>(instruction)) {
            //std::cout << "JOIN" << std::endl;
            auto recovered = dump.recover();
            code->prepend(recovered);
        }
        else if (std::holds_alternative<std::shared_ptr<inst::SEL>>(instruction)) {
            //std::cout << "SEL" << std::endl;
            if (datastack.empty())
                throw std::runtime_error("SEL needs one argument on stack");
            auto tmp = datastack.top();
            datastack.pop();
            if (!std::holds_alternative<std::shared_ptr<Data>>(tmp))
                throw std::runtime_error("SEL needs one number on stack, found different type");
            auto datatmp = *std::get<std::shared_ptr<Data>>(tmp);
            if (!std::holds_alternative<int>(datatmp))
                throw std::runtime_error("SEL needs one number on stack, found different type");
            int number = std::get<int>(datatmp);
            if (number != 0) {
                auto ncode = std::make_shared<secd::Code>(secd::Code(code->next()));
                code->next();
                dump.dump(code->getData());
                code = ncode;
            }
            else {
                code->next();
                auto nexttmp = code->next();
                auto ncode = std::make_shared<secd::Code>(secd::Code(nexttmp));
                auto tmpdump = code->getData();
                dump.dump(tmpdump);
                code = ncode;
            }
        }
        else if (std::holds_alternative<std::shared_ptr<inst::NIL>>(instruction)) {
            datastack.push(secd::Nil);
        }
        else if (std::holds_alternative<std::shared_ptr<inst::CONS>>(instruction)) {
            auto op = [](secd::Value<Data> x, secd::Value<Data> y) -> secd::Value<Data> {
                return secd::cons(x, y);
            };
            binaryop(datastack, op, "CONS");
        }
        else if (std::holds_alternative<std::shared_ptr<inst::LD>>(instruction)) {
            auto ld = std::get<std::shared_ptr<inst::LD>>(instruction);
            datastack.push(env.get(ld->i, ld->j));
        }
        else if (std::holds_alternative<std::shared_ptr<inst::LDF>>(instruction)) {
            auto tmpenv = env.get();
            secd::Value<Data> tmpnext = code->next();
            auto tmpcons = secd::cons(tmpnext, tmpenv);
            datastack.push(tmpcons); 
        }
        else if (std::holds_alternative<std::shared_ptr<inst::AP>>(instruction)) {
            auto closure = datastack.top();
            datastack.pop();
            auto args = datastack.top();
            datastack.pop();
            auto dumpdata = secd::cons(datastack.getData(), secd::cons(code->getData(), env.get()));
            dump.dump(dumpdata);
            code = std::make_shared<secd::Code>(secd::car(closure));
            env.set(secd::cons(args, secd::cdr(closure)));
        }
        else if (std::holds_alternative<std::shared_ptr<inst::RTN>>(instruction)) {
            auto res = datastack.top();
            auto recovered = dump.recover();
            datastack.set(secd::car(recovered));
            datastack.push(res);
            code = std::make_shared<secd::Code>(secd::car(secd::cdr(recovered)));
            auto tmp = secd::cdr(secd::cdr(recovered));
            if (std::holds_alternative<std::shared_ptr<secd::NilT>>(tmp))
                env.set(secd::Nil);
            else
                env.set(
                    secd::cdr(secd::cdr(recovered)));
        }
        else if (std::holds_alternative<std::shared_ptr<inst::DUM>>(instruction)) {
            env.set(secd::cons<Data>(std::move(secd::Nil), env.get()));
        }
        else if (std::holds_alternative<std::shared_ptr<inst::RAP>>(instruction)) {
            auto closure = datastack.top();
            datastack.pop();
            auto args = datastack.top();
            datastack.pop();
            auto tmp = env.get();
            if (!std::holds_alternative<std::shared_ptr<secd::ConsCell<Data>>>(tmp)) {
                throw std::runtime_error("mate and dont know what would i tell you really fuck up");
            }
            if (!std::holds_alternative<std::shared_ptr<secd::ConsCell<Data>>>(args)) {
                throw std::runtime_error("mate and dont know what would i tell you really fuck up");
            }
            auto dummyenv = std::get<std::shared_ptr<secd::ConsCell<Data>>>(tmp);
            *dummyenv->car = secd::cons<Data>(secd::car(args), secd::cons<Data>(dummyenv, secd::Nil));
            auto dumpdata = secd::cons(datastack.getData(), secd::cons(code->getData(), cdr(env.get())));
            dump.dump(dumpdata);
            code = std::make_shared<secd::Code>(car(closure));
            env.set(cdr(closure));
        }
        else {
            throw std::runtime_error("Not implemented");
        }
    }
    if (verbose)
        verboseWriteout(code, datastack, dump, env, cycle);
}

int main(int argc, char ** argv) {
    if (argc < 2) return 1;
    std::ifstream file(argv[1], std::ios::in | std::ios::binary);
    auto code = readInst(file);
    bool verbose = false;
    if (argc >= 3) {
        verbose = std::string(argv[2]) == "-v";
    }
    try {
        auto datastack = secd::Stack<secd::Data>();
        auto dump = secd::Dump();
        auto env = secd::Enviroment();
        run(code, datastack, dump, env, verbose);

        traverse_stack(datastack);
    }
    catch(const std::exception& e) {
        std::cout << "runtime error : " << e.what() << std::endl;
    }
    return 0;
};