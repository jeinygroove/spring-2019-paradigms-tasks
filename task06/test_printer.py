#!/usr/bin/env python3
import io
from model import *
from printer import *


def pretty_print(program):
    printer = PrettyPrinter()
    program.accept(printer)
    return str(printer)


def test_program1():
    program = FunctionDefinition('main', Function(['arg1'], [
        Read('x'),
        Print(Reference('x')),
        Conditional(
            BinaryOperation(Number(2), '==', Number(3)),
            [
                Conditional(Number(1), [], [])
            ],
            [
                FunctionCall(Reference('exit'), [
                    UnaryOperation('-', Reference('arg1'))
                ])
            ],
        ),
    ]))

    assert pretty_print(program) == """\
def main(arg1) {
    read x;
    print x;
    if ((2) == (3)) {
        if (1) {
        }
    } else {
        exit(-(arg1));
    }
}"""


def test_program2():
    scope = Scope()
    program = FunctionDefinition(
        'factorial', Function(['n'], [
            Conditional(
                BinaryOperation(Reference('n'),
                                '==',
                                Number(0)),
                [Number(1)],
                [
                    BinaryOperation(
                        Reference('n'),
                        '*',
                        FunctionCall(
                            Reference('factorial'), [
                                BinaryOperation(
                                    Reference('n'),
                                    '-',
                                    Number(1))
                            ])
                    )
                ])
        ]))

    assert pretty_print(program) == """\
def factorial(n) {
    if ((n) == (0)) {
        1;
    } else {
        (n) * (factorial((n) - (1)));
    }
}"""


def test_number():
    number = Number(10)
    assert pretty_print(number) == "10;"


def test_functiondefinition():
    func = FunctionDefinition("foo", Function([], []))
    assert pretty_print(func) == """\
def foo() {
}"""


def test_conditional():
    condition = Conditional(Number(42), [], [])
    assert pretty_print(condition) == """\
if (42) {
}"""


def test_print(capsys):
    func_print = Print(Number(42))
    assert pretty_print(func_print) == "print 42;"


def test_read(monkeypatch):
    func_print = Read('x')
    assert pretty_print(func_print) == "read x;"


def test_functioncall():
    scope = Scope()
    FunctionDefinition(
        'func', Function(['n'], [Reference('n')])).evaluate(scope)
    call = FunctionCall(Reference('func'), [
        Number(10)
    ])
    assert call.evaluate(scope).value == 10


def test_reference():
    refer = Reference('x')
    assert pretty_print(refer) == "x;"


def test_binary_operation():
    operation = BinaryOperation(Number(1), "*",
                                BinaryOperation(Number(2), "+", Number(3)))
    assert pretty_print(operation) == "(1) * ((2) + (3));"


def test_unary_operation():
    operation = UnaryOperation('-', Number(42))
    assert pretty_print(operation) == "-(42);"
