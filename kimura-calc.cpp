// Whole system:  [string]--> parser --[node]--> evaluater --> [integer]

// These are examples with which this program can not deal with properly:
//   `x` (Can not refer to variables)
//   `1 + (1 + 1)`

#include <iostream>
#include <string>
#include <regex>
#include <map>
#include <typeinfo>
#include <boost/variant.hpp>
#include <boost/algorithm/string.hpp>

// It shows a process of parsing when true.
// There must be a more smart way tho
const bool debugs = true;

namespace op {
    // unary operator
    struct neg;
    struct pos;
    // binary operator
    struct add;
    struct sub;
    struct mul;
    struct div;
    struct mod;
}

template <class Op> struct Opn1;
template <class Op> struct Opn2;

// Element of syntax tree
using Node = boost::variant<
    int // literal
    , std::string // variable
    // unary operation
    , boost::recursive_wrapper< Opn1<op::pos> >
    , boost::recursive_wrapper< Opn1<op::neg> >
    // binary operation
    , boost::recursive_wrapper< Opn2<op::add> >
    , boost::recursive_wrapper< Opn2<op::sub> >
    , boost::recursive_wrapper< Opn2<op::mul> >
    , boost::recursive_wrapper< Opn2<op::div> >
    , boost::recursive_wrapper< Opn2<op::mod> >
>;

template <class Op> struct Opn1 {
    Node right;
    Opn1(Node const& r): right(r) { }
};

template <class Op> struct Opn2 {
    Node left, right;
    Opn2(Node const& l, Node const& r): left(l), right(r) { }
};

// Map from variable name to integer value
using Ctx = std::map<std::string, int>;

// Evaluate syntax tree into integer value
int eval (Ctx& ctx, Node node) {
    if(debugs) std::cout << "eval" << std::endl;
    if(node.type() == typeid(int)) return boost::get<int>(node);
    if(node.type() == typeid(std::string)) {
        auto v = boost::get<std::string>(node);
        if (ctx.count(v)) return ctx[v];
        throw "Undefined variable `" + v + "`";
    }
    if(node.type() == typeid(Opn1<op::pos>)) return eval(ctx, node);
    if(node.type() == typeid(Opn1<op::neg>)) return - eval(ctx, node);
    if(node.type() == typeid(Opn2<op::add>)) {
        auto opn = boost::get<Opn2<op::add> >(node);
        return eval(ctx, opn.left) + eval(ctx, opn.right);
    }
    if(node.type() == typeid(Opn2<op::sub>)) {
        auto opn = boost::get<Opn2<op::sub> >(node);
        return eval(ctx, opn.left) - eval(ctx, opn.right);
    }
    if(node.type() == typeid(Opn2<op::mul>)) {
        auto opn = boost::get<Opn2<op::mul> >(node);
        return eval(ctx, opn.left) * eval(ctx, opn.right);
    }
    if(node.type() == typeid(Opn2<op::div>)) {
        auto opn = boost::get<Opn2<op::div> >(node);
        return eval(ctx, opn.left) / eval(ctx, opn.right);
    }
    if(node.type() == typeid(Opn2<op::mod>)) {
        auto opn = boost::get<Opn2<op::mod> >(node);
        return eval(ctx, opn.left) % eval(ctx, opn.right);
    }
    throw std::string("This never occurs!");
}

// Parser that parses string into node
namespace parse {
    Node num(std::string s) {
        if(debugs) std::cout << "num: " << s << std::endl;
        std::smatch results;
        if(std::regex_match(s, results, std::regex(R"(\s*(\d+)\s*)"))) {
            auto n = results[1].str();
            return std::stoi(n);
        }
        throw std::string("Not a valid number");
    }
    Node var(std::string s) {
        if(debugs) std::cout << "var: " << s << std::endl;
        std::smatch results;
        if(std::regex_match(s, results, std::regex(R"(\s*([a-zA-Z]+)\s*)"))) {
            auto v = results[1].str();
            return v;
        }
        throw std::string("Not a valid variable");
    }
    Node expr(std::string);
    Node fact(std::string s) {
        if(debugs) std::cout << "fact: " << s << std::endl;
        // factor ::= number | variable | "(" expression ")"
        std::smatch results;
        if(std::regex_match(s, results, std::regex(R"(\s*([\+\-])\s*(.+)\s*)"))) {
            auto o = results[1].str();
            auto e = results[2].str();
            if(o == "+") return Opn1<op::pos>(expr(e));
            if(o == "-") return Opn1<op::neg>(expr(e));
        }
        if(std::regex_match(s, results, std::regex(R"(\s*\((.+)\)\s*)"))) {
            auto e = results[1].str();
            return expr(e);
        }
        try { return num(s); } catch(std::string e) {}
        try { return var(s); } catch(std::string e) {}
        throw std::string("Not a valid factor");
    }
    Node term(std::string);
    Node expr(std::string s) {
        if(debugs) std::cout << "expr: " << s << std::endl;
        // expression ::= term | expression "+" term
        std::smatch results;
        if(std::regex_match(s, results, std::regex(R"(\s*(.+)\s*([\+\-])\s*(.+)\s*)"))) {
            auto e = results[1].str();
            auto o = results[2].str();
            auto t = results[3].str();
            if(o == "+") return Opn2<op::add>(expr(e), term(t));
            if(o == "-") return Opn2<op::sub>(expr(e), term(t));
        }
        try { return term(s); } catch(std::string e) {}
        throw std::string("Not a valid expression");
    }
    Node term(std::string s) {
        if(debugs) std::cout << "term: " << s << std::endl;
        // term ::= factor | term "*" factor
        try { return fact(s); } catch(std::string e) {}
        std::smatch results;
        if(std::regex_match(s, results, std::regex(R"(\s*(.+)\s*([\*/%])\s*(.+)\s*)"))) {
            auto t = results[1].str();
            auto o = results[2].str();
            auto f = results[3].str();
            if(o == "*") return Opn2<op::mul>(term(t), fact(f));
            if(o == "/") return Opn2<op::div>(term(t), fact(f));
            if(o == "%") return Opn2<op::mod>(term(t), fact(f));
        }
        throw std::string("Not a valid term");
    }
    Node asgn(Ctx& ctx, std::string s) {
        if(debugs) std::cout << "asgn: " << s << std::endl;
        std::smatch results;
        if(std::regex_match(s, results, std::regex(R"(\s*(.+)\s*=\s*(.+)\s*)"))) {
            auto v = results[1].str();
            auto e = results[2].str(); // expr
            auto x = expr(e);
            ctx[v] = eval(ctx, x);
            return x;
        }
        throw std::string("Not a valid assignmnt");
    }
    Node input(Ctx& ctx, std::string s) {
        if(debugs) std::cout << "input: " << s << std::endl;
        try { return asgn(ctx, s); } catch(std::string e) {}
        try { return expr(s); } catch(std::string e) {}
        throw std::string("Not a valid input");
    }
}

int main(void) {
    std::cout << "Interactive Calculator with Variables\n";

    Ctx ctx;
    if(debugs) ctx["answer"] = 42;
    while(true) {
        try {
            std::cout << "> ";
            std::string s;
            std::getline(std::cin, s);

            Node node = parse::input(ctx, s);
            int i = eval(ctx, node);
            std::cout << i << std::endl;
        } catch (std::string e) { std::cout << e << std::endl; }
    }
    return 0;
}