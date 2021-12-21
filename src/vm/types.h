#include <variant>
#include <memory>

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
    const NilT Nil = NilT();

    class List;

    using Value = std::variant<NilT, int, List*>;

    class List {
        public:
            List() : value(Nil), rest(nullptr) {}
            List(Value val) : value(val), rest(nullptr) {}
            List(Value val, List * rest) : value(val), rest(rest) {}

            bool empty() const {
                return std::holds_alternative<NilT>(value) && rest == nullptr;
            }

            bool last() const {
                return !std::holds_alternative<NilT>(value) && rest == nullptr;
            }

            Value head() const {
                return value;
            }

            List * tail() const {
                return rest;
            }

            List * prepend(Value val) {
                return new List(val, this);
            }

            void append(Value  val) {
                if (empty()) 
                    value = val;
                else if (last()) 
                    rest = new List(val);
                else 
                    rest->append(val);
            }

            void clearAll() {
                if (rest != nullptr) {
                    rest->clearAll();
                    delete rest;
                }
            }
            /*
            ~List() {
                if (rest != nullptr) {
                    delete rest;
                }
            }*/
        private:
            Value value;
            List * rest;
    };

    

    // four parts of secd
    class Stack {
        public:
            Stack() : data(new List()) {}
            
            // memory leak is created here
            // todo GC, lets see
            // seems solved lets see more
            Value pop() {
                auto tmp = data->head();
                auto tail = data->tail();
                delete data;
                data = tail;
                return tmp;
            }

            Value top() const {
                return data->head();
            }

            void push(Value val) {
                data = data->prepend(val);
            }

            ~Stack() {
                data->clearAll();
            }
        private:
            List * data;
    };

    class Enviroment {

    };

    class Code {
        public:
            Code() : data(new Stack()) {}
            Code(Stack * data) : data(data) {}
        private:
            Stack * data;
    };

    class Dump {

    };
}