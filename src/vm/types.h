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

    template <typename T> 
    class List;

    template <typename T> 
    using Value = std::variant<NilT, T, List<T>*>;

    template <typename T> 
    class List {
        public:
            List() : value(Nil), rest(nullptr) {}
            List(Value<T> val) : value(val), rest(nullptr) {}
            List(Value<T> val, List * rest) : value(val), rest(rest) {}

            bool empty() const {
                return std::holds_alternative<NilT>(value) && rest == nullptr;
            }

            bool last() const {
                return !std::holds_alternative<NilT>(value) && rest == nullptr;
            }

            Value<T> head() const {
                return value;
            }

            List * tail() const {
                return rest;
            }

            List * prepend(Value<T> val) {
                return new List<T>(val, this);
            }

            void append(Value<T> val) {
                if (empty()) 
                    value = val;
                else if (last()) 
                    rest = new List<T>(val);
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
            Value<T> value;
            List<T> * rest;
    };

    

    // four parts of secd
    template <typename T> 
    class Stack {
        public:
            Stack() : data(new List<T>()) {}
            
            // memory leak is created here
            // todo GC, lets see
            // seems solved lets see more
            T pop() {
                auto tmp = data->head();
                auto tail = data->tail();
                delete data;
                data = tail;
                return tmp;
            }

            T top() const {
                return data->head();
            }

            void push(Value<T> val) {
                data = data->prepend(val);
            }

            ~Stack() {
                data->clearAll();
            }
        private:
            List<T> * data;
    };

    class Enviroment {

    };

    class Code {
        public:
            Code() : data(new Stack<inst::Inst>()) {}
            Code(Stack<inst::Inst> * data) : data(data) {}
        private:
            Stack<inst::Inst> * data;
    };

    class Dump {

    };
}