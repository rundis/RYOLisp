package com.rollyourowncode.lisp

import static java.lang.System.exit

public class RYOLisp {
    Env outerEnv

    static void main(args) {
        RYOLisp ryoLisp = new RYOLisp()
        run(ryoLisp)
    }

    private static run(RYOLisp ryoLisp) {
        try {
            ryoLisp.repl()
        } catch (Exception e) {
            println e.message
            run(ryoLisp)
        }
    }

    RYOLisp() {
        outerEnv = addGlobals(new Env())
    }

    def repl() {
        while (true) println(evaluate(parse(read())))
    }

    def read() {
        print "repl >> "
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in))
        def source = br.readLine()
        if (source == "exit") exit(0)
        return source
    }

    def interpret(s) {
        return evaluate(parse(s))
    }

    def evaluate(x, Env env = outerEnv) {
        switch (x) {
            case String:
                return evaluateString(env, x)
            case Collection:
                return evaluateCollection(env, x)
            default:
                return x
        }
    }

    private def evaluateString(Env env, String x) {
        println "evaluate string, find in env: $x"
        return env.find(x)[x]
    }

    private def evaluateCollection(env, x) {
        switch (x[0]) {
            case "set!":
                def (_, var, exp) = x
                return env.find(var)[var] = evaluate(exp, env)
            case "quote":
                def (_, exp) = x
                return exp
            case "if":
                def (_, test, conseq, alt) = x
                return evaluate(evaluate(test, env) ? conseq : alt, env)
            case "define":
                def (_, var, exp) = x
                return env[var] = evaluate(exp, env)
            case "fn":
                def (_, vars, exp) = x
                println "lambda: vars: " + vars + ", exp: " + exp
                return { Object[] args -> evaluate(exp, new Env(vars, args, env))}
            case "begin":
                /*
                     def val
                     x[1..-1].each { val = evaluate(it, env) }
                     return val
                 */
                return x[1..-1].collect {evaluate(it, env)}.last()
            case "eval":
                def (_, form) = x
                return evaluate(evaluate(form, env))
            default:
                return runProcedure(x, env)
        }
    }

    private def runProcedure(Collection x, Env env) {
        //def expressions = x.collect{evaluate(it, env)} as ArrayDeque
        def expressions = new ArrayDeque()
        for (expression in x) {
            expressions.add(evaluate(expression, env))
        }

        def procedure = expressions.pop()
        List arguments = new ArrayList(expressions)
        return procedure(* arguments)
    }

    def addGlobals(Env env) {
        def bool = {it ? 1 : 0}
        env.putAll([
                "+": { Object[] x -> x.sum() },
                "-": { a, b -> a - b },
                "*": { a, b -> a * b },
                ">": { a, b -> a > b} >> bool,
                "<": { a, b -> a < b} >> bool,
                "<=": { a, b -> a <= b} >> bool,
                ">=": { a, b -> a >= b} >> bool,

                "not": { !it},

                "car": { it.head()},
                "cdr": { it.tail()},

                "list": { Object[] x -> [* x]},
                "list?": { it instanceof List} >> bool,

                "equal?": { a, b -> a == b} >> bool,

                "cons": { x, y -> [x] + y }])
        return env
    }

    def parse(s) {
        return treeify(tokenize(s))
    }

    def treeify(tokens) {
        if (!tokens) throw new Exception("unexpected EOF while reading")
        if (tokens.size() == 1) return atom(tokens[0])

        // fold left
        def ast = tokens.inject([root: null, curr: null]) {ast, token ->
            switch (token) {
                case "(":
                    if (!ast.root) {
                        def curr = new Node(null, [])
                        return [root: curr, curr: curr]
                    }
                    return [root: ast.root, curr: ast.curr.appendNode(null, [])]
                case ")":
                    ast.curr = ast.curr.parent() ?: ast.root
                    return ast
                default:
                    ast.curr.value = ast.curr.value() + atom(token)
                    return ast

            }
        }

        nestedLists(ast.root)

        /*def token = tokens.remove(0) //tokens.pop() //[0]
        switch (token) {
            case '(':
                def L = []
                while (tokens.first() != ')') {
                    L << treeify(tokens)
                }
                tokens.remove(0)  // pop off ')'
                return L
            case ')':
                throw new Exception("unexpected )")
            default:
                return atom(token)
        } */
    }

    def nestedLists(node) {
       node.value().collect{
           it instanceof Node ? nestedLists(it) : it
       }
    }

    def atom(String token) {
        if (token.isInteger()) {
            return token.toInteger()
        }
        if (token.isFloat()) {
            return token.toFloat()
        }
        //return new Symbol(symbol: token)
        return token
    }

    def tokenize(s) {
        return s.replace('(', ' ( ').replace(')', ' ) ').split() as List
    }


}
