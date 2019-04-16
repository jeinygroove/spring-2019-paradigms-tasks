#!/usr/bin/env python3
from model import *


def fold_constants(program):
    const_folder = ConstantFolder()
    return program.accept(const_folder)


class ConstantFolder(ASTNodeVisitor):
    def visit_number(self, node):
        return Number(node.value)

    def visit_function(self, node):
        body = [cmd.accept(self) for cmd in node.body]
        return Function(node.args, body)

    def visit_function_definition(self, node):
        return FunctionDefinition(node.name, node.function.accept(self))

    def visit_conditional(self, node):
        condition = node.condition.accept(self)
        if_true = [cmd.accept(self) for cmd in node.if_true or []]
        if_false = [cmd.accept(self) for cmd in node.if_false or []]
        return Conditional(condition, if_true, if_false)

    def visit_print(self, node):
        expr = node.expr.accept(self)
        return Print(expr)

    def visit_read(self, node):
        return Read(node.name)

    def visit_function_call(self, node):
        fun_expr = node.fun_expr.accept(self)
        args = [argument.accept(self) for argument in node.args]
        return FunctionCall(fun_expr, args)

    def visit_reference(self, node):
        return Reference(node.name)

    def visit_binary_operation(self, node):
        lhs = node.lhs.accept(self)
        rhs = node.rhs.accept(self)
        op = node.op
        if isinstance(lhs, Number):
            if isinstance(rhs, Number):
                return BinaryOperation(lhs, op, rhs).evaluate(Scope())
            if lhs.value == 0 and isinstance(rhs, Reference) and op == '*':
                return Number(0)
        elif isinstance(rhs, Number):
            if rhs.value == 0 and isinstance(lhs, Reference) and op == '*':
                return Number(0)
        elif isinstance(rhs, Reference):
            if lhs.name == rhs.name and op == '-':
                return Number(0)
        return BinaryOperation(lhs, op, rhs)

    def visit_unary_operation(self, node):
        op = node.op
        expr = node.expr.accept(self)
        if isinstance(expr, Number):
            return UnaryOperation(op, expr).evaluate(Scope())
        return UnaryOperation(op, expr)
