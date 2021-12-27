#include <variant>
#include <memory>
#include <stack>
#include <stdexcept>
#include <sstream>

namespace inst {
    struct ERR{};
    struct LDC;
    struct NIL{};
    struct ADD{};
    struct SUB{};
    struct MUL{};
    struct DIV{};
    struct EQ{};
    struct GT{};
    struct LT{};
    struct CONS{};
    struct CAR{};
    struct CDR{};
    struct CONSP{};
    struct SEL{};
    struct JOIN{};
    struct LD;
    struct LDF{};
    struct AP{};
    struct RTN{};
    struct DUM{};
    struct RAP{};
    struct PRT{};
    struct READ{};

    using Inst = std::variant<
        std::shared_ptr<ERR>,
        std::shared_ptr<LDC>,
        std::shared_ptr<NIL>,
        std::shared_ptr<ADD>,
        std::shared_ptr<SUB>,
        std::shared_ptr<MUL>,
        std::shared_ptr<DIV>,
        std::shared_ptr<EQ>,
        std::shared_ptr<GT>,
        std::shared_ptr<LT>,
        std::shared_ptr<CONS>,
        std::shared_ptr<CAR>,
        std::shared_ptr<CDR>,
        std::shared_ptr<CONSP>,
        std::shared_ptr<SEL>,    
        std::shared_ptr<JOIN>,
        std::shared_ptr<LD>,
        std::shared_ptr<LDF>,
        std::shared_ptr<AP>,
        std::shared_ptr<RTN>,
        std::shared_ptr<DUM>,
        std::shared_ptr<RAP>,
        std::shared_ptr<PRT>,
        std::shared_ptr<READ>>;
    
    struct LDC {
        LDC(int number) : number(number) {}
        int number;
    };

    struct LD {
        LD(int i, int j) : i(i), j(j) {}
        int i, j;
    };

    void show(Inst instruction, std::ostream & stream) {
        if (std::holds_alternative<std::shared_ptr<inst::LDC>>(instruction)) {
            auto tmp = std::get<std::shared_ptr<LDC>>(instruction);
            stream << "LCD" << tmp->number;
        }
        else if (std::holds_alternative<std::shared_ptr<inst::ADD>>(instruction)) {
            stream << "ADD";
        }
        else if (std::holds_alternative<std::shared_ptr<inst::SUB>>(instruction)) {
            stream << "SUB";
        }
        else if (std::holds_alternative<std::shared_ptr<inst::MUL>>(instruction)) {
            stream << "MUL";
        }
        else if (std::holds_alternative<std::shared_ptr<inst::DIV>>(instruction)) {
            stream << "DIV";
        }
        else if (std::holds_alternative<std::shared_ptr<inst::EQ>>(instruction)) {
            stream << "EQ";
        }
        else if (std::holds_alternative<std::shared_ptr<inst::GT>>(instruction)) {
            stream << "GT";
        }
        else if (std::holds_alternative<std::shared_ptr<inst::LT>>(instruction)) {
            stream << "LT";
        }
        else if (std::holds_alternative<std::shared_ptr<inst::JOIN>>(instruction)) {
            stream << "JOIN";
        }
        else if (std::holds_alternative<std::shared_ptr<inst::SEL>>(instruction)) {
            stream << "SEL";
        }
        else if (std::holds_alternative<std::shared_ptr<inst::NIL>>(instruction)) {
            stream << "NIL";
        }
        else if (std::holds_alternative<std::shared_ptr<inst::CONS>>(instruction)) {
            stream << "CONS";
        }
        else if (std::holds_alternative<std::shared_ptr<inst::LD>>(instruction)) {
            auto tmp = std::get<std::shared_ptr<LD>>(instruction);
            stream << "LD " << tmp->i << "," << tmp->j;
        }
        else if (std::holds_alternative<std::shared_ptr<inst::LDF>>(instruction)) {
            stream << "LDF";
        }
        else if (std::holds_alternative<std::shared_ptr<inst::AP>>(instruction)) {
            stream << "AP";
        }
        else if (std::holds_alternative<std::shared_ptr<inst::RTN>>(instruction)) {
            stream << "RTN";
        }
        else if (std::holds_alternative<std::shared_ptr<inst::DUM>>(instruction)) {
            stream << "DUM";
        }
        else if (std::holds_alternative<std::shared_ptr<inst::RAP>>(instruction)) {
            stream << "RAP";
        }
        else if (std::holds_alternative<std::shared_ptr<inst::ERR>>(instruction)) {
            stream << "ERR";
        }
        else if (std::holds_alternative<std::shared_ptr<inst::CAR>>(instruction)) {
            stream << "CAR";
        }
        else if (std::holds_alternative<std::shared_ptr<inst::CDR>>(instruction)) {
            stream << "CDR";
        }
        else {
            stream << "mate too fast";
        }

    }
}

namespace secd {
    // data in secd
    struct NilT {};
    const std::shared_ptr<NilT> Nil = 
        std::make_shared<NilT>(NilT());

    template <typename T> 
    class List;

    template <typename T>
    struct ConsCell;

    template <typename T>
    using Value = std::variant<
        std::shared_ptr<NilT>, 
        std::shared_ptr<T>, 
        std::shared_ptr<ConsCell<T>>>;
    
    template <typename T>
    struct ConsCell {
        Value<T> car;
        Value<T> cdr;
    };

    template <typename T>
    Value<T> car(Value<T> val) {
        if (!std::holds_alternative<std::shared_ptr<ConsCell<T>>>(val)) {
            std::runtime_error("car error (value is not a cons cell)");
        }
        auto conscell = std::get<std::shared_ptr<ConsCell<T>>>(val);
        return (conscell->car);
    }
    
    template <typename T>
    Value<T> cdr(Value<T> val) {
        if (!std::holds_alternative<std::shared_ptr<ConsCell<T>>>(val)) {
            std::runtime_error("cdr error (value is not a cons cell)");
        }
        auto conscell = std::get<std::shared_ptr<ConsCell<T>>>(val);
        return (conscell->cdr);
    }

    template <typename T>
    Value<T> cons(Value<T> car, Value<T> cdr) {
        auto cell = std::make_shared<ConsCell<T>>();
        cell->car = car;
        cell->cdr = cdr;
        return cell;
    }

    template <typename T>
    Value<T> append(Value<T> cell, Value<T> val) {
        if (std::holds_alternative<std::shared_ptr<NilT>>(cell)) {
            return cons<T>(val, Nil);
        }
        if (std::holds_alternative<std::shared_ptr<ConsCell<T>>>(cell)) {
            Value<T> tmp = std::get<std::shared_ptr<ConsCell<T>>>(cell);
            return cons(car(tmp), append(cdr(tmp), val));
        }
        std::runtime_error("cannot append");
        return Nil;
    }

    template <typename T>
    Value<T> appendLists(Value<T> cell, Value<T> val) {
        if (std::holds_alternative<std::shared_ptr<NilT>>(cell)) {
            if (
                std::holds_alternative<std::shared_ptr<ConsCell<T>>>(val) ||
                std::holds_alternative<std::shared_ptr<NilT>>(val)
            ) {
                return val;
            }
            return cons<T>(val, Nil);
        }
        if (std::holds_alternative<std::shared_ptr<ConsCell<T>>>(cell)) {
            Value<T> tmp = std::get<std::shared_ptr<ConsCell<T>>>(cell);
            return cons(car(tmp), appendLists(cdr(tmp), val));
        }
        std::runtime_error("cannot append");
        return Nil;
    }

    void showInsts(Value<inst::Inst> val) {
        if (std::holds_alternative<std::shared_ptr<NilT>>(val)) {
            std::cout << " null ";
        }
        else if (std::holds_alternative<std::shared_ptr<inst::Inst>>(val)) {
            auto tmp = std::get<std::shared_ptr<inst::Inst>>(val);
            inst::show(*tmp, std::cout);
        }
        else if (std::holds_alternative<std::shared_ptr<ConsCell<inst::Inst>>>(val)) {
            std::cout << "( ";
            showInsts(car(val));
            std::cout << " ";
            showInsts(cdr(val));
            std::cout << ")";
        }
    }
    #define __MAX_DEPTH__ 8 
    template <typename T>
    void showValue(Value<T> val, int depth = 0) {
        if (std::holds_alternative<std::shared_ptr<T>>(val)) {
            auto tmp = *std::get<std::shared_ptr<T>>(val);
            std::cout << tmp;
        }
        else if (std::holds_alternative<std::shared_ptr<secd::NilT>>(val)) {
            std::cout << "nil";
        }
        else {
            if (depth >= __MAX_DEPTH__) {
                std::cout << "(...)";
            }
            else {
                std::cout << "( ";
                showValueInner(val, depth);
                std::cout << ")";
            }
        }
    }

    template <typename T>
    void showValueInner(Value<T> val, int depth = 0) {
        if (std::holds_alternative<std::shared_ptr<T>>(val)) {
            auto tmp = *std::get<std::shared_ptr<T>>(val);
            std::cout << tmp;
        }
        else if (std::holds_alternative<std::shared_ptr<secd::NilT>>(val)) {
        }
        else if (depth >= __MAX_DEPTH__) {
            std::cout << "...";
        }
        else {
            auto tmpcar = car(val);
            auto tmpcdr = cdr(val);
            if (
                std::holds_alternative<std::shared_ptr<ConsCell<T>>>(tmpcdr) ||
                std::holds_alternative<std::shared_ptr<NilT>>(tmpcdr)
                
            ) {
                showValue(tmpcar, depth + 1);
                std::cout << " ";
                showValueInner(tmpcdr, depth);
            }
            else {
                std::cout << "( ";
                showValue(tmpcar, depth + 1);
                std::cout << " ";
                showValue(tmpcdr, depth);
                std::cout << " )";
            }
        }
    }
    
    using Data = std::variant<
            int,
            inst::Inst
        >;
    std::ostream & operator<<(std::ostream & stream, const Data & val) {
        if (std::holds_alternative<int>(val)) {
            stream << std::get<int>(val);
        }
        else {
            inst::show(std::get<inst::Inst>(val), stream);
        }
        return stream;
    }

    // four parts of secd
    template <typename T>
    class Stack {
        public:
            Stack() : data(Nil) {}

            void pop() {
                data = cdr(data);
            }

            Value<T> top() {
                return car(data);
            }

            void push(Value<T> val) {
                data = cons(val, data);
            }

            Value<T> getData() {
                return data;
            }

            bool empty() {
                return std::holds_alternative<std::shared_ptr<NilT>>(data);
            }

            void set(Value<T> val) {
                data = val;
            }

        private:
            Value<T> data;
    };

    class Enviroment {
        public:
            Enviroment() : data(Nil) {}

            Value<Data> get(int x, int y) {
                auto tmp = data;
                for (int i = 0; i < x; i++) {
                    tmp = cdr(tmp);
                }
                tmp = car(tmp);
                for (int i = 0; i < y; i++) {
                    tmp = cdr(tmp);
                }
                return car(tmp);
            }

            Value<Data> get() {
                return data;
            }

            void set(Value<Data> val) {
                data = val;
            }
        private:
            Value<Data> data;
    };

    class Code {
        public:
            Code() : data(Nil) {}
            Code(Value<Data> data) : data(data) {}

            void prepend(Value<Data> val) {
                if (!std::holds_alternative<std::shared_ptr<NilT>>(val))
                    data = cons(val, data);
            }

            void add(Value<Data> val) {
                if (std::holds_alternative<std::shared_ptr<Data>>(val)) {
                    //Data d = std::get<std::shared_ptr<inst::Inst>>(val);
                    //Value<Data> tmp = val;
                    data = append(data, val);
                }
                else {
                    throw std::runtime_error("Can only add instruction to code");
                }
            }

            void addData(Value<Data> val) {
                data = append(data, val);
            }

            bool isHeadList() {
                return 
                    std::holds_alternative<std::shared_ptr<ConsCell<Data>>>
                    (car(data)) ||
                    std::holds_alternative<std::shared_ptr<NilT>>(car(data));
            }

            Value<Data> head() {
                if(empty()) {
                    return Nil;
                }
                return car(data);
            }

            Value<Data> next() {
                if(empty()) {
                    return Nil;
                }
                auto res = car(data);
                data = cdr(data);
                return res;
            }

            inst::Inst headInst() {
                auto res = car(data);
                data = cdr(data);
                if (!std::holds_alternative<std::shared_ptr<Data>>(res))
                    throw std::runtime_error("car in not instruction");
                auto tmp = *std::get<std::shared_ptr<Data>>(res);
                if (!std::holds_alternative<inst::Inst>(tmp))
                    throw std::runtime_error("car in not instruction");

                return std::get<inst::Inst>(tmp);
            }

            inst::Inst nextInst() {
                // in case branching creates only instruction
                if (std::holds_alternative<std::shared_ptr<Data>>(data)) {
                    auto tmp = *std::get<std::shared_ptr<Data>>(data);
                    if (std::holds_alternative<inst::Inst>(tmp)) {   
                        auto resinst = std::get<inst::Inst>(tmp);
                        data = Nil;
                        return resinst;
                    }
                    else {
                        throw std::runtime_error("code does not contain instruction");
                    }
                }
                if (empty()) {
                    throw std::runtime_error("??????");
                }
                auto res = car(data);
                data = cdr(data);
                if (!std::holds_alternative<std::shared_ptr<Data>>(res))
                    throw std::runtime_error("car in not instruction");
                auto tmp = *std::get<std::shared_ptr<Data>>(res);
                if (!std::holds_alternative<inst::Inst>(tmp))
                    throw std::runtime_error("car in not instruction");

                return std::get<inst::Inst>(tmp);
            }

            Value<Data> getData() {
                return data;
            }

            bool empty() {
                if (std::holds_alternative<std::shared_ptr<NilT>>(data))
                    return true;
                return false;
            }
            Value<Data> data;
    };

    class Dump {
        public:
            Dump() : data(Stack<Data>()) {}

            void dump(Value<Data> val) {
                data.push(val);
            }

            Value<Data> recover() {
                if (data.empty())
                    throw std::runtime_error("Cannot recover dump is empty");
                auto tmp = data.top();
                data.pop();
                return tmp;
            }

            Stack<Data> & getData() {
                return data;
            }
        private:
            Stack<Data> data;
    };
}