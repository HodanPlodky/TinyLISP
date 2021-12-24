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
        std::shared_ptr<RTN>>;
    
    struct LDC {
        LDC(int number) : number(number) {}
        int number;
    };

    struct LD {
        LD(int i, int j) : i(i), j(j) {}
        int i, j;
    };

    /*
    struct LDF {
        LDF(Inst inner) : inner(inner) {}
        Inst inner;
    };
    */

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
        else if (std::holds_alternative<std::shared_ptr<inst::ERR>>(instruction)) {
            stream << "ERR";
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
        auto conscell = std::move(std::get<std::shared_ptr<ConsCell<T>>>(val));
        return conscell->car;
    }
    
    template <typename T>
    Value<T> cdr(Value<T> val) {
        if (!std::holds_alternative<std::shared_ptr<ConsCell<T>>>(val)) {
            std::runtime_error("cdr error (value is not a cons cell)");
        }
        auto conscell = std::move(std::get<std::shared_ptr<ConsCell<T>>>(val));
        return conscell->cdr;
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

    template <typename T>
    void showValue(Value<T> val) {
        if (std::holds_alternative<std::shared_ptr<T>>(val)) {
            auto tmp = *std::get<std::shared_ptr<T>>(val);
            std::cout << tmp;
        }
        else if (std::holds_alternative<std::shared_ptr<secd::NilT>>(val)) {
            std::cout << "()";
        }
        else {
            std::cout << "( ";
            showValueInner(val);
            std::cout << ")";
        }
    }

    template <typename T>
    void showValueInner(Value<T> val) {
        if (std::holds_alternative<std::shared_ptr<T>>(val)) {
            auto tmp = *std::get<std::shared_ptr<T>>(val);
            std::cout << tmp;
        }
        else if (std::holds_alternative<std::shared_ptr<secd::NilT>>(val)) {
        }
        else {
            auto tmpcar = car(val);
            auto tmpcdr = cdr(val);
            if (
                std::holds_alternative<std::shared_ptr<ConsCell<T>>>(tmpcdr) ||
                std::holds_alternative<std::shared_ptr<NilT>>(tmpcdr)
                
            ) {
                showValue(tmpcar);
                std::cout << " ";
                showValueInner(tmpcdr);
            }
            else {
                std::cout << "( ";
                showValue(tmpcar);
                std::cout << " ";
                showValue(tmpcdr);
                std::cout << " )";
            }
        }
    }

    template <typename T>
    Value<T> cons(Value<T> car, Value<T> cdr) {
        auto cell = std::make_shared<ConsCell<T>>(ConsCell<T>());
        cell->car = std::move(car);
        cell->cdr = std::move(cdr);
        return std::move(cell);
    }

    template <typename T>
    Value<T> append(Value<T> cell, Value<T> val) {
        if (std::holds_alternative<std::shared_ptr<NilT>>(cell)) {
            return cons<T>(std::move(val), std::move(Nil));
        }
        if (std::holds_alternative<std::shared_ptr<ConsCell<T>>>(cell)) {
            auto tmp = std::move(std::get<std::shared_ptr<ConsCell<T>>>(cell));
            return cons(std::move(tmp->car), std::move(append(tmp->cdr, val)));
        }
        std::runtime_error("cannot append");
        return std::move(Nil);
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
            return cons<T>(std::move(val), std::move(Nil));
        }
        if (std::holds_alternative<std::shared_ptr<ConsCell<T>>>(cell)) {
            auto tmp = std::move(std::get<std::shared_ptr<ConsCell<T>>>(cell));
            return cons(std::move(tmp->car), std::move(appendLists(tmp->cdr, val)));
        }
        std::runtime_error("cannot append");
        return std::move(Nil);
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
    //template <typename T>
    //using Stack = std::stack<Value<T>>;
    template <typename T>
    class Stack {
        public:
            Stack() : data(std::move(Nil)) {}

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
                data = std::move(val);
            }

        private:
            Value<T> data;
    };

    class Enviroment {
        public:
            Enviroment() : data(std::move(Nil)) {}

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
                data = std::move(val);
            }
        private:
            Value<Data> data;
    };

    class Code {
        public:
            Code() : data(std::move(Nil)) {}
            Code(Value<Data> data) : data(data) {}

            void prepend(Value<Data> val) {
                if (!std::holds_alternative<std::shared_ptr<NilT>>(val))
                    data = std::move(cons(val, data));
            }

            void add(Value<inst::Inst> val) {
                if (std::holds_alternative<std::shared_ptr<inst::Inst>>(val)) {
                    Data d = *std::get<std::shared_ptr<inst::Inst>>(val);
                    Value<Data> tmp = std::make_shared<Data>(d);
                    data = std::move(append(data, tmp));
                }
                else {
                    throw std::runtime_error("Can only add instruction to code");
                }
            }

            void addData(Value<Data> val) {
                data = std::move(append(data, val));
            }

            bool isHeadList() {
                return 
                    std::holds_alternative<std::shared_ptr<ConsCell<Data>>>
                    (car(data));
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
                auto res = std::move(car(data));
                data = std::move(cdr(data));
                return res;
            }

            inst::Inst headInst() {
                auto res = std::move(car(data));
                data = std::move(cdr(data));
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
                        data = std::move(Nil);
                        return resinst;
                    }
                    else {
                        throw std::runtime_error("code does not contain instruction");
                    }
                }
                if (empty()) {
                    throw std::runtime_error("??????");
                }
                auto res = std::move(car(data));
                data = std::move(cdr(data));
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
                auto tmp = std::move(data.top());
                data.pop();
                return tmp;
            }
        private:
            Stack<Data> data;
    };
}