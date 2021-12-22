#include <variant>
#include <memory>
#include <stack>
#include <stdexcept>

namespace inst {
    struct LDC;
    struct NIL{};
    struct ADD{};
    struct SUB{};
    struct MUL{};
    struct DIV{};
    struct CONS{};
    struct CAR{};
    struct CDR{};
    struct CONSP{};
    struct SEL{};
    struct JOIN{};
    struct LD;
    struct LDF;
    struct AP{};
    struct RTN{};

    using Inst = std::variant<
        std::shared_ptr<LDC>,
        std::shared_ptr<NIL>,
        std::shared_ptr<ADD>,
        std::shared_ptr<SUB>,
        std::shared_ptr<MUL>,
        std::shared_ptr<DIV>,
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

    template <typename T>
    Value<T> cons(Value<T> car, Value<T> cdr) {
        auto cell = make_shared<ConsCell<T>>(ConsCell<T>());
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

    // four parts of secd
    template <typename T>
    using Stack = std::stack<Value<T>>;

    class Enviroment {

    };

    class Code {
        public:
            Code() : data(std::move(Nil)) {}
            Code(Value<inst::Inst> data) : data(data) {}

            void prepend(Value<inst::Inst> val) {
                data = cons(val, data);
            }

            void add(Value<inst::Inst> val) {
                data = std::move(append(data, val));
            }

            bool isHeadList() {
                return 
                    std::holds_alternative<std::shared_ptr<ConsCell<inst::Inst>>>
                    (car(data));
            }

            Value<inst::Inst> head() {
                return car(data);
            }

            Value<inst::Inst> next() {
                auto res = std::move(car(data));
                data = std::move(cdr(data));
                return res;
            }

            inst::Inst headInst() {
                auto res = std::move(car(data));
                if (!std::holds_alternative<std::shared_ptr<inst::Inst>>(res))
                    std::runtime_error("car in not instruction");

                return std::move(*std::get<std::shared_ptr<inst::Inst>>(res));
            }

            inst::Inst nextInst() {
                // in case branching creates only instruction
                if (std::holds_alternative<std::shared_ptr<inst::Inst>>(data)) {
                    auto tmp = std::get<std::shared_ptr<inst::Inst>>(data);
                    data = std::move(Nil);
                    return *tmp;
                }
                auto res = std::move(car(data));
                data = std::move(cdr(data));
                if (!std::holds_alternative<std::shared_ptr<inst::Inst>>(res))
                    std::runtime_error("car in not instruction");

                return std::move(*std::get<std::shared_ptr<inst::Inst>>(res));
            }

            Value<inst::Inst> getData() {
                return data;
            }

            bool empty() {
                return std::holds_alternative<std::shared_ptr<NilT>>(data);
            }
        private:
            Value<inst::Inst> data;
    };

    class Dump {
        public:
            Dump() : data(Stack<inst::Inst>()) {}

            void dump(Value<inst::Inst> val) {
                data.push(val);
            }

            Value<inst::Inst> recover() {
                if (data.empty())
                    throw std::runtime_error("Cannot recover dump is empty");
                auto tmp = std::move(data.top());
                data.pop();
                return tmp;
            }
        private:
            Stack<inst::Inst> data;
    };
}